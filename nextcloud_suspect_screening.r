library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(polars)
library(DT)
library(httr)
library(xml2)

# Configuration
BASE_URL <- "https://nas-osu-prammics.u-pec.fr"
MAIN_TOKEN <- "rQWagSzAiNH4tk8"

# Fonction pour calculer la masse exacte à partir d'un SMILES
calculate_exact_mass <- function(smiles) {
  tryCatch({
    if (requireNamespace("rcdk", quietly = TRUE)) {
      mol <- rcdk::parse.smiles(smiles)[[1]]
      mass <- rcdk::get.exact.mass(mol)
      return(mass)
    } else if (requireNamespace("webchem", quietly = TRUE)) {
      cid <- webchem::get_cid(smiles, from = "smiles")
      if (!is.na(cid[1])) {
        props <- webchem::pc_prop(cid[1], properties = "MolecularWeight")
        return(as.numeric(props$MolecularWeight))
      }
    }
    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

# Liste prédéfinie de molécules suspectes
predefined_suspects <- data.frame(
  name = c("Caffeine", "Nicotine", "Cocaine", "MDMA", "THC", "Fentanyl"),
  smiles = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", 
             "CN1CCCC1C2=CC=CN=C2",
             "COC(=O)C1C(OC(=O)C2=CC=CC=C2)CC3CCC1N3C",
             "CC(CC1=CC2=C(C=C1)OCO2)NC",
             "CCCCCC1=CC(=C2C3C=C(CCC3C(OC2=C1)(C)C)C)O",
             "CCC(=O)N(C1CCN(CC1)CCC2=CC=CC=C2)C3=CC=CC=C3"),
  exact_mass = c(195.0877, 163.1230, 304.1543, 194.1176, 315.2319, 337.2275),
  stringsAsFactors = FALSE
)

# Fonction pour lister les contenus Nextcloud
list_nextcloud_contents <- function(base_url, token, subpath = "") {
  tryCatch({
    base_url <- sub("/public.php.*$", "", base_url)
    base_url <- sub("/s/.*$", "", base_url)
    
    if (subpath == "" || subpath == "/") {
      webdav_url <- paste0(base_url, "/public.php/webdav/")
    } else {
      subpath <- gsub("^/+|/+$", "", subpath)
      webdav_url <- paste0(base_url, "/public.php/webdav/", subpath, "/")
    }
    
    body <- '<?xml version="1.0"?>
<a:propfind xmlns:a="DAV:">
  <a:allprop/>
</a:propfind>'
    
    req <- httr::VERB(
      verb = "PROPFIND",
      url = webdav_url,
      authenticate(token, ""),
      add_headers(
        Depth = "1",
        `Content-Type` = "application/xml",
        `User-Agent` = "Mozilla/5.0"
      ),
      body = body
    )
    
    if (status_code(req) != 207 && status_code(req) != 200) {
      stop(paste("Failed to list contents. Status:", status_code(req)))
    }
    
    content_xml <- content(req, as = "text", encoding = "UTF-8")
    doc <- read_xml(content_xml)
    responses <- xml_find_all(doc, ".//d:response", xml_ns(doc))
    
    items <- list()
    
    for (node in responses) {
      href <- xml_text(xml_find_first(node, ".//d:href", xml_ns(doc)))
      decoded_href <- URLdecode(href)
      name <- basename(decoded_href)
      
      if (name == "" || name == "webdav" || grepl("^/public.php/webdav/?$", decoded_href)) {
        next
      }
      
      href_path <- sub("^/public.php/webdav/", "", decoded_href)
      href_path <- gsub("^/+|/+$", "", href_path)
      current_clean <- gsub("^/+|/+$", "", subpath)
      
      if (href_path == current_clean || href_path == paste0(current_clean, "/")) {
        next
      }
      
      resource_type <- xml_find_first(node, ".//d:resourcetype", xml_ns(doc))
      is_collection <- !is.na(xml_find_first(resource_type, ".//d:collection", xml_ns(doc)))
      
      if (subpath == "" || subpath == "/") {
        relative_path <- name
      } else {
        relative_path <- paste0(gsub("^/+|/+$", "", subpath), "/", name)
      }
      
      # CORRECTION: Ne pas encoder les slashes dans l'URL finale
      download_url <- if (!is_collection) {
        paste0(base_url, "/public.php/dav/files/", token, "/", relative_path)
      } else {
        NA
      }
      
      items[[length(items) + 1]] <- data.frame(
        name = name,
        path = relative_path,
        is_folder = is_collection,
        url = as.character(download_url),
        stringsAsFactors = FALSE
      )
    }
    
    if (length(items) == 0) {
      return(data.frame(
        name = character(),
        path = character(),
        is_folder = logical(),
        url = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    do.call(rbind, items)
    
  }, error = function(e) {
    message("Error listing contents: ", e$message)
    return(data.frame(
      name = character(),
      path = character(),
      is_folder = logical(),
      url = character(),
      stringsAsFactors = FALSE
    ))
  })
}

# Helper functions pour les requêtes
query_eic <- function(data, mslevel, mz_min, mz_max) {
  result <- data$
    filter(pl$col("mslevel") == mslevel)$
    filter(pl$col("mz") >= mz_min & pl$col("mz") <= mz_max)$
    group_by(pl$col("rt"), pl$col("scanid"))$
    agg(pl$col("intensity")$sum()$alias("intensity"))$
    sort("rt")$
    collect() |>
    as.data.frame()
  return(result)
}

query_spectrum <- function(data, mslevel, rt_min, rt_max) {
  result <- data$
    filter(pl$col("mslevel") == mslevel)$
    filter(pl$col("rt") >= rt_min & pl$col("rt") <= rt_max)$
    group_by(pl$col("mz"))$
    agg(pl$col("intensity")$sum()$alias("intensity"))$
    sort("mz")$
    collect() |>
    as.data.frame()
  return(result)
}

query_data <- function(data, mslevel, mz_min, mz_max, rt_min, rt_max, min_intensity) {
  result <- data$
    filter(pl$col("mslevel") == mslevel)$
    filter(pl$col("mz") >= mz_min & pl$col("mz") <= mz_max)$
    filter(pl$col("rt") >= rt_min & pl$col("rt") <= rt_max)$
    filter(pl$col("intensity") >= min_intensity)$
    group_by(pl$col("rt"), pl$col("scanid"))$
    agg(
      pl$col("intensity")$max()$alias("max_intensity"),
      pl$col("mz")$mean()$alias("mean_mz"),
      pl$len()$alias("n_points")
    )$
    collect() |>
    as.data.frame()
  return(result)
}

# UI
ui <- page_navbar(
  title = "Nextcloud Suspect Screening",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  # Tab 1: Browser
  nav_panel(
    title = "File Browser",
    icon = icon("folder-open"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        textInput("base_url", "Nextcloud URL:", 
                  value = BASE_URL,
                  placeholder = "https://nextcloud.example.com"),
        
        textInput("token", "Share Token:", 
                  value = MAIN_TOKEN),
        
        actionButton("refresh", "Refresh", 
                     class = "btn-primary w-100 mb-3"),
        
        hr(),
        
        h5("Navigation"),
        uiOutput("breadcrumb_ui"),
        
        hr(),
        
        h5("Fichiers sélectionnés"),
        div(
          style = "max-height: 300px; overflow-y: auto;",
          uiOutput("selected_files_ui")
        ),
        
        actionButton("clear_selection", "Clear Selection", 
                     class = "btn-warning w-100 mt-2"),
        
        actionButton("load_selected", "Load Selected Files", 
                     class = "btn-success w-100 mt-2")
      ),
      
      card(
        card_header("Contents"),
        card_body(
          style = "display: flex; flex-direction: column; height: 100%;",
          uiOutput("current_path_display"),
          div(
            style = "flex: 1; min-height: 400px; overflow: auto;",
            DTOutput("contents_table")
          ),
          hr(),
          h5("Selected Files Details:"),
          div(
            style = "max-height: 300px; overflow: auto;",
            DTOutput("selected_details_table")
          )
        )
      ),
      
      card(
        card_header("Loaded Files Summary"),
        card_body(
          verbatimTextOutput("loaded_summary")
        )
      )
    )
  ),
  
  # Tab 2: Suspects
  nav_panel(
    title = "Suspect List",
    icon = icon("list"),
    
    layout_columns(
      col_widths = c(12),
      
      card(
        card_header("Suspect Molecules Management"),
        card_body(
          navset_tab(
            nav_panel(
              "Manual Entry",
              br(),
              layout_columns(
                col_widths = c(4, 4, 4),
                textInput("suspectName", "Compound Name:"),
                textInput("suspectSMILES", "SMILES Code:"),
                numericInput("suspectMass", "Exact Mass (m/z):", value = NULL)
              ),
              actionButton("addSuspect", "Add to List", class = "btn-success"),
              actionButton("usePredefined", "Load Predefined List", class = "btn-info")
            ),
            nav_panel(
              "CSV Import",
              br(),
              p("CSV file should contain columns: name, smiles, exact_mass (optional)"),
              fileInput("csvFile", "Choose CSV file:", accept = ".csv"),
              actionButton("loadCSV", "Load CSV", class = "btn-primary")
            ),
            nav_panel(
              "Current List",
              br(),
              DTOutput("suspectsTable"),
              br(),
              actionButton("clearSuspects", "Clear All", class = "btn-danger")
            )
          )
        )
      )
    )
  ),
  
  # Tab 3: Screening
  nav_panel(
    title = "Screening",
    icon = icon("search"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        h4("Screening Parameters"),
        
        selectInput("msLevel", "MS Level:",
                    choices = list("MS1" = "1", "MS2" = "2"),
                    selected = "1"),
        
        numericInput("massTolerance", "Mass Tolerance (±Da):", 
                     value = 0.01, min = 0.001, max = 1, step = 0.001),
        
        numericInput("minIntensity", "Minimum Intensity:", 
                     value = 1000, min = 0, step = 100),
        
        sliderInput("rtRange", "RT Range (min):",
                    min = 0, max = 30, value = c(0, 30), step = 0.1),
        
        hr(),
        
        checkboxInput("screenAllSamples", "Screen all loaded samples", value = TRUE),
        
        conditionalPanel(
          condition = "!input.screenAllSamples",
          selectInput("singleSampleSelect", "Select Sample:", choices = NULL)
        ),
        
        actionButton("runScreening", "Run Screening", 
                     class = "btn-primary btn-lg w-100")
      ),
      
      card(
        card_header("Screening Results"),
        card_body(
          withSpinner(DTOutput("screeningResults")),
          br(),
          downloadButton("downloadResults", "Download Results", class = "btn-success")
        )
      )
    )
  ),
  
  # Tab 4: Visualization
  nav_panel(
    title = "Visualization",
    icon = icon("chart-line"),
    
    layout_columns(
      col_widths = c(12),
      
      card(
        card_header("Visualization Settings"),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput("sampleForViz", "Select Sample:", choices = NULL),
            selectInput("selectedCompound", "Select Compound:", choices = NULL),
            numericInput("eicTolerance", "EIC Tolerance (±Da):", 
                         value = 0.01, min = 0.001, max = 0.5, step = 0.001)
          ),
          checkboxInput("overlayEICs", "Overlay all samples", value = FALSE)
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Extracted Ion Chromatogram (EIC)"),
        card_body(
          withSpinner(plotlyOutput("eicPlot", height = "400px"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          full_screen = TRUE,
          card_header("MS1 Spectrum"),
          card_body(
            p("Click on EIC to select RT", style = "color: #666; font-size: 0.9em;"),
            withSpinner(plotlyOutput("msPlotLow", height = "400px"))
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("MS2 Spectrum"),
          card_body(
            p("Click on EIC to select RT", style = "color: #666; font-size: 0.9em;"),
            withSpinner(plotlyOutput("msPlotHigh", height = "400px"))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    current_path = "",
    contents = NULL,
    selected_files = data.frame(
      name = character(),
      path = character(),
      folder = character(),
      url = character(),
      stringsAsFactors = FALSE
    ),
    data_files = list(),
    file_names = character(),
    suspects = data.frame(
      name = character(),
      smiles = character(),
      exact_mass = numeric(),
      stringsAsFactors = FALSE
    ),
    screening_results = NULL,
    eic_data = NULL
  )
  
  # === FILE BROWSER ===
  
  load_contents <- function() {
    req(input$base_url, input$token)
    
    withProgress(message = "Loading...", {
      contents <- list_nextcloud_contents(
        input$base_url, 
        input$token, 
        rv$current_path
      )
      rv$contents <- contents
    })
  }
  
  observe({
    load_contents()
  }) |> bindEvent(input$refresh, ignoreNULL = FALSE)
  
  output$breadcrumb_ui <- renderUI({
    if (rv$current_path == "" || rv$current_path == "/") {
      tags$div(
        actionLink("nav_home", "🏠 Home", 
                   style = "font-weight: bold; font-size: 16px;")
      )
    } else {
      path_parts <- strsplit(rv$current_path, "/")[[1]]
      path_parts <- path_parts[path_parts != ""]
      
      links <- list(
        actionLink("nav_home", "🏠", style = "margin-right: 5px;")
      )
      
      for (i in seq_along(path_parts)) {
        path_to <- paste(path_parts[1:i], collapse = "/")
        links <- c(links, list(
          tags$span(" / "),
          actionLink(paste0("nav_part_", i), path_parts[i],
                     `data-path` = path_to,
                     style = if(i == length(path_parts)) "font-weight: bold;")
        ))
      }
      
      do.call(tags$div, links)
    }
  })
  
  observe({
    rv$current_path <- ""
    load_contents()
  }) |> bindEvent(input$nav_home)
  
  observeEvent(rv$current_path, {
    if (rv$current_path == "" || rv$current_path == "/") return()
    
    path_parts <- strsplit(rv$current_path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""]
    
    for (i in seq_along(path_parts)) {
      local({
        idx <- i
        target_path <- paste(path_parts[1:idx], collapse = "/")
        
        observeEvent(input[[paste0("nav_part_", idx)]], {
          rv$current_path <- target_path
          load_contents()
        }, ignoreInit = TRUE)
      })
    }
  })
  
  output$current_path_display <- renderUI({
    if (rv$current_path == "" || rv$current_path == "/") {
      tags$p(tags$strong("Current location:"), " Root")
    } else {
      tags$p(tags$strong("Current location:"), " /", rv$current_path)
    }
  })
  
  output$contents_table <- renderDT({
    req(rv$contents)
    
    if (nrow(rv$contents) == 0) {
      return(datatable(
        data.frame(Message = "No items found"),
        options = list(dom = 't'),
        rownames = FALSE,
        selection = 'none'
      ))
    }
    
    display_df <- rv$contents
    display_df$Type <- ifelse(display_df$is_folder, "📁 Folder", "📄 File")
    
    show_df <- data.frame(
      Type = display_df$Type,
      Name = display_df$name,
      stringsAsFactors = FALSE
    )
    
    datatable(
      show_df,
      selection = 'single',
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = 'frtip',
        columnDefs = list(
          list(width = '80px', targets = 0)
        )
      )
    )
  })
  
  observeEvent(input$contents_table_rows_selected, {
    req(input$contents_table_rows_selected)
    selected_idx <- input$contents_table_rows_selected
    
    if (selected_idx > nrow(rv$contents)) return()
    
    selected_item <- rv$contents[selected_idx, ]
    
    if (selected_item$is_folder) {
      rv$current_path <- selected_item$path
      load_contents()
    } else if (grepl("\\.parquet$", selected_item$name, ignore.case = TRUE)) {
      new_file <- data.frame(
        name = selected_item$name,
        path = selected_item$path,
        folder = dirname(selected_item$path),
        url = selected_item$url,
        stringsAsFactors = FALSE
      )
      
      if (!selected_item$path %in% rv$selected_files$path) {
        rv$selected_files <- rbind(rv$selected_files, new_file)
        showNotification(paste("Added:", selected_item$name), type = "message")
      } else {
        showNotification(paste("Already selected:", selected_item$name), type = "warning")
      }
    }
  })
  
  output$selected_files_ui <- renderUI({
    if (nrow(rv$selected_files) == 0) {
      return(tags$p("No files selected", style = "color: gray; font-style: italic;"))
    }
    
    lapply(1:nrow(rv$selected_files), function(i) {
      file <- rv$selected_files[i, ]
      tags$div(
        class = "alert alert-info py-2 px-2 mb-2",
        style = "font-size: 12px;",
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$div(
            tags$strong(file$name),
            tags$br(),
            tags$small(file$folder, style = "color: #666;")
          ),
          actionButton(
            paste0("remove_", i),
            "✕",
            class = "btn-sm btn-outline-danger",
            style = "padding: 2px 8px;",
            onclick = sprintf("Shiny.setInputValue('remove_file', %d, {priority: 'event'})", i)
          )
        )
      )
    })
  })
  
  observeEvent(input$remove_file, {
    idx <- input$remove_file
    if (idx > 0 && idx <= nrow(rv$selected_files)) {
      rv$selected_files <- rv$selected_files[-idx, , drop = FALSE]
      showNotification("File removed from selection", type = "message")
    }
  })
  
  observeEvent(input$clear_selection, {
    rv$selected_files <- data.frame(
      name = character(),
      path = character(),
      folder = character(),
      url = character(),
      stringsAsFactors = FALSE
    )
    showNotification("Selection cleared", type = "message")
  })
  
  output$selected_details_table <- renderDT({
    if (nrow(rv$selected_files) == 0) {
      return(datatable(
        data.frame(Message = "No files selected"),
        options = list(dom = 't'),
        rownames = FALSE,
        selection = 'none'
      ))
    }
    
    datatable(
      rv$selected_files[, c("name", "folder"), drop = FALSE],
      options = list(
        pageLength = 10,
        dom = 'tp'
      ),
      rownames = FALSE,
      colnames = c("File", "Location"),
      selection = 'none'
    )
  })
  
  observeEvent(input$load_selected, {
    req(nrow(rv$selected_files) > 0)
    
    showNotification("Loading selected files...", type = "message", 
                     duration = NULL, id = "loadNotif")
    
    tryCatch({
      files_loaded <- 0
      
      for (i in 1:nrow(rv$selected_files)) {
        file_info <- rv$selected_files[i, ]
        file_name <- paste0(file_info$folder, "/", file_info$name)
        
        # Skip if already loaded
        if (file_name %in% rv$file_names) {
          next
        }
        
        cat("Loading:", file_info$url, "\n")

        # Lazy scan with corrected URL (no %2F encoding)
        rv$data_files[[file_name]] <- pl$scan_parquet(file_info$url)
        rv$file_names <- c(rv$file_names, file_name)
        files_loaded <- files_loaded + 1
      }
      
      removeNotification("loadNotif")
      showNotification(paste(files_loaded, "file(s) loaded!"), 
                       type = "message", duration = 3)
      
      # Update dropdowns
      updateSelectInput(session, "singleSampleSelect", choices = rv$file_names)
      updateSelectInput(session, "sampleForViz", choices = rv$file_names)
      
    }, error = function(e) {
      removeNotification("loadNotif")
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })
  
  output$loaded_summary <- renderPrint({
    if (length(rv$file_names) == 0) {
      cat("No files loaded.\n")
    } else {
      cat("Loaded files:\n")
      cat("-------------\n")
      for (i in seq_along(rv$file_names)) {
        cat(sprintf("%d. %s\n", i, rv$file_names[i]))
      }
    }
  })
  
  # === SUSPECTS MANAGEMENT ===
  
  observeEvent(input$addSuspect, {
    req(input$suspectName, input$suspectSMILES)
    
    mass <- input$suspectMass
    if (is.null(mass) || is.na(mass)) {
      mass <- calculate_exact_mass(input$suspectSMILES)
    }
    
    if (is.na(mass)) {
      showNotification("Could not calculate mass. Enter manually.", 
                       type = "warning", duration = 5)
      return()
    }
    
    new_suspect <- data.frame(
      name = input$suspectName,
      smiles = input$suspectSMILES,
      exact_mass = mass,
      stringsAsFactors = FALSE
    )
    
    rv$suspects <- rbind(rv$suspects, new_suspect)
    
    updateTextInput(session, "suspectName", value = "")
    updateTextInput(session, "suspectSMILES", value = "")
    updateNumericInput(session, "suspectMass", value = NULL)
    
    showNotification("Suspect added!", type = "message", duration = 2)
  })
  
  observeEvent(input$usePredefined, {
    rv$suspects <- predefined_suspects
    showNotification("Predefined list loaded!", type = "message", duration = 2)
  })
  
  observeEvent(input$loadCSV, {
    req(input$csvFile)
    
    tryCatch({
      suspects_csv <- read.csv(input$csvFile$datapath, stringsAsFactors = FALSE)
      
      if (!"exact_mass" %in% colnames(suspects_csv)) {
        suspects_csv$exact_mass <- sapply(suspects_csv$smiles, calculate_exact_mass)
      }
      
      rv$suspects <- suspects_csv[, c("name", "smiles", "exact_mass")]
      showNotification("CSV loaded!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })
  
  observeEvent(input$clearSuspects, {
    rv$suspects <- data.frame(
      name = character(),
      smiles = character(),
      exact_mass = numeric(),
      stringsAsFactors = FALSE
    )
  })
  
  output$suspectsTable <- renderDT({
    datatable(rv$suspects, 
              options = list(pageLength = 15),
              rownames = FALSE)
  })
  
  # === SCREENING ===
  
  observeEvent(input$runScreening, {
    req(nrow(rv$suspects) > 0, length(rv$file_names) > 0)
    
    showNotification("Running screening...", 
                     type = "message", duration = NULL, id = "screenNotif")
    
    tryCatch({
      all_results <- list()
      
      samples_to_screen <- if (input$screenAllSamples) {
        rv$file_names
      } else {
        req(input$singleSampleSelect)
        input$singleSampleSelect
      }
      
      for (file_name in samples_to_screen) {
        current_data <- rv$data_files[[file_name]]
        
        for (i in 1:nrow(rv$suspects)) {
          suspect <- rv$suspects[i, ]
          
          mz_min <- suspect$exact_mass - input$massTolerance
          mz_max <- suspect$exact_mass + input$massTolerance
          
          hits <- query_data(
            current_data,
            input$msLevel,
            mz_min,
            mz_max,
            input$rtRange[1],
            input$rtRange[2],
            input$minIntensity
          )
          
          if (nrow(hits) > 0) {
            detection_score <- min(100, (nrow(hits) / 10) * 100)
            max_int <- max(hits$max_intensity)
            mean_mz_obs <- mean(hits$mean_mz)
            mz_error <- (mean_mz_obs - suspect$exact_mass) / suspect$exact_mass * 1e6
            status <- ifelse(detection_score > 50, "Detected", "Tentative")
          } else {
            detection_score <- 0
            max_int <- 0
            mean_mz_obs <- NA
            mz_error <- NA
            status <- "Not Detected"
          }
          
            
            all_results[[length(all_results) + 1]] <- data.frame(
              Sample = file_name,
              Compound = suspect$name,
              Exact_Mass = suspect$exact_mass,
              Observed_MZ = if(!is.na(mean_mz_obs)) round(mean_mz_obs, 4) else NA,
              MZ_Error_ppm = if(!is.na(mz_error)) round(mz_error, 2) else NA,
              Detection_Score = round(detection_score, 1),
              Max_Intensity = round(max_int, 0),
              N_Detections = nrow(hits),
              Status = status
            )
        }
      }
      
      rv$screening_results <- do.call(rbind, all_results)
      
      detected_compounds <- unique(rv$screening_results$Compound[
        rv$screening_results$Detection_Score > 0])
      updateSelectInput(session, "selectedCompound", choices = detected_compounds)
      
      removeNotification("screenNotif")
      showNotification("Screening completed!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("screenNotif")
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
    
  })
  output$screeningResults <- renderDT({
    req(rv$screening_results)
    datatable(rv$screening_results,
              options = list(pageLength = 20, order = list(list(5, 'desc'))),
              rownames = FALSE,
              filter = 'top') |>
      formatStyle('Status',
                  backgroundColor = styleEqual(
                    c('Detected', 'Tentative', 'Not Detected'),
                    c('#d4edda', '#fff3cd', '#f8d7da')
                  ))
    
  })
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("screening_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$screening_results, file, row.names = FALSE)
    }
  )
 # === VISUALIZATION ===
    output$eicPlot <- renderPlotly({
      req(input$selectedCompound)
      suspect <- rv$suspects[rv$suspects$name == input$selectedCompound, ]
      req(nrow(suspect) > 0)
      
      mz_min <- suspect$exact_mass - input$eicTolerance
      mz_max <- suspect$exact_mass + input$eicTolerance
      
      if (input$overlayEICs && length(rv$file_names) > 0) {
        p <- plot_ly()
        
        colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', 
                    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
        
        for (i in seq_along(rv$file_names)) {
          file_name <- rv$file_names[i]
          current_data <- rv$data_files[[file_name]]
          
          eic_data <- query_eic(current_data, input$msLevel, mz_min, mz_max)
          
          if (nrow(eic_data) > 0) {
            color_idx <- ((i - 1) %% length(colors)) + 1
            p <- p |>
              add_trace(data = eic_data, x = ~rt, y = ~intensity, 
                        type = 'scatter', mode = 'lines',
                        name = file_name,
                        line = list(color = colors[color_idx], width = 2))
          }
        }
        
        p <- p |>
          layout(title = paste("EIC Overlay:", input$selectedCompound, 
                               sprintf("(m/z %.4f ± %.3f)", suspect$exact_mass, input$eicTolerance)),
                 xaxis = list(title = "RT (min)"),
                 yaxis = list(title = "Intensity"))
        
        return(p)
        
      } else {
        req(input$sampleForViz)
        current_data <- rv$data_files[[input$sampleForViz]]
        
        eic_data <- query_eic(current_data, input$msLevel, mz_min, mz_max)
        rv$eic_data <- eic_data
        
        if (nrow(eic_data) == 0) {
          plot_ly() |>
            layout(title = "No data found")
        } else {
          plot_ly(eic_data, x = ~rt, y = ~intensity, 
                  type = 'scatter', mode = 'lines+markers',
                  source = "eicPlot",
                  line = list(color = 'rgba(0,0,0,1)', width = 1),
                  marker = list(size = 4),
                  hovertemplate = 'RT: %{x:.2f} min<br>Intensity: %{y:.0f}<extra></extra>') |>
            layout(title = paste("EIC:", input$selectedCompound, 
                                 sprintf("(m/z %.4f ± %.3f)", suspect$exact_mass, input$eicTolerance)),
                   xaxis = list(title = "RT (min)"),
                   yaxis = list(title = "Intensity"))
        }
      }
    })
  output$msPlotLow <- renderPlotly({
    req(input$selectedCompound,rv$selectedCompound,rv$eic_data, input$sampleForViz)
    
    click_data <- event_data("plotly_click", source = "eicPlot")
    
    if (is.null(click_data)) {
      max_rt <- rv$eic_data$rt[which.max(rv$eic_data$intensity)]
    } else {
      max_rt <- click_data$x
    }
    
    suspect <- rv$suspects[rv$suspects$name == input$selectedCompound, ]
    
    rt_window <- rv$eic_data[rv$eic_data$rt >= (max_rt - 0.5) & 
                               rv$eic_data$rt <= (max_rt + 0.5), ]
    
    if (nrow(rt_window) > 3) {
      max_int <- max(rt_window$intensity)
      half_max <- max_int / 2
      above_half <- rt_window[rt_window$intensity > half_max, ]
      if (nrow(above_half) > 0) {
        fwhm <- max(above_half$rt) - min(above_half$rt)
        rt_tolerance <- max(0.05, fwhm)
      } else {
        rt_tolerance <- 0.1
      }
    } else {
      rt_tolerance <- 0.1
    }
    
    current_data <- rv$data_files[[input$sampleForViz]]
    ms_data <- query_spectrum(current_data, "1", 
                              max_rt - rt_tolerance, max_rt + rt_tolerance)
    
    if (nrow(ms_data) == 0) {
      plot_ly() |>
        layout(title = "No MS1 data found")
    } else {
      plot_ly(ms_data, x = ~mz, y = ~intensity, type = 'bar',
              marker = list(color = 'rgba(0,100,200,0.7)'),
              hovertemplate = 'm/z: %{x:.4f}<br>Intensity: %{y:.0f}<extra></extra>') |>
        add_segments(x = suspect$exact_mass, xend = suspect$exact_mass,
                     y = 0, yend = max(ms_data$intensity),
                     line = list(color = 'red', dash = 'dash', width = 2),
                     name = "Target m/z") |>
        layout(title = sprintf("MS1 at RT %.2f min (± %.3f)", max_rt, rt_tolerance),
               xaxis = list(title = "m/z"),
               yaxis = list(title = "Intensity"))
    }
  })
  output$msPlotHigh <- renderPlotly({
    req(input$selectedCompound,rv$selectedCompound, rv$eic_data, input$sampleForViz)
    
    click_data <- event_data("plotly_click", source = "eicPlot")
    
    if (is.null(click_data)) {
      max_rt <- rv$eic_data$rt[which.max(rv$eic_data$intensity)]
    } else {
      max_rt <- click_data$x
    }
    
    suspect <- rv$suspects[rv$suspects$name == input$selectedCompound, ]
    
    rt_window <- rv$eic_data[rv$eic_data$rt >= (max_rt - 0.5) & 
                               rv$eic_data$rt <= (max_rt + 0.5), ]
    
    if (nrow(rt_window) > 3) {
      max_int <- max(rt_window$intensity)
      half_max <- max_int / 2
      above_half <- rt_window[rt_window$intensity > half_max, ]
      if (nrow(above_half) > 0) {
        fwhm <- max(above_half$rt) - min(above_half$rt)
        rt_tolerance <- max(0.05, fwhm)
      } else {
        rt_tolerance <- 0.1
      }
    } else {
      rt_tolerance <- 0.1
    }
    
    current_data <- rv$data_files[[input$sampleForViz]]
    ms_data <- query_spectrum(current_data, "2", 
                              max_rt - rt_tolerance, max_rt + rt_tolerance)
    
    if (nrow(ms_data) == 0) {
      plot_ly() |>
        layout(title = "No MS2 data found")
    } else {
      plot_ly(ms_data, x = ~mz, y = ~intensity, type = 'bar',
              marker = list(color = 'rgba(200,50,50,0.7)'),
              hovertemplate = 'm/z: %{x:.4f}<br>Intensity: %{y:.0f}<extra></extra>') |>
        add_segments(x = suspect$exact_mass, xend = suspect$exact_mass,
                     y = 0, yend = max(ms_data$intensity),
                     line = list(color = 'red', dash = 'dash', width = 2),
                     name = "Precursor m/z") |>
        layout(title = sprintf("MS2 at RT %.2f min (± %.3f)", max_rt, rt_tolerance),
               xaxis = list(title = "m/z"),
               yaxis = list(title = "Intensity"))
    }
  })
}
# Run app
shinyApp(ui, server)
