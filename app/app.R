library(shiny)
library(bslib)
library(DT)

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' is required. Install it with install.packages('plotly').")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

find_project_root <- function() {
  configured_root <- Sys.getenv("PROJECT_ROOT", unset = "")
  if (nzchar(configured_root) && file.exists(file.path(configured_root, "data", "processed", "metadata_index.csv"))) {
    return(normalizePath(configured_root, mustWork = TRUE))
  }

  ofile <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  app_dir <- if (!is.null(ofile)) dirname(normalizePath(ofile, mustWork = FALSE)) else getwd()
  candidates <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), ".."),
    app_dir,
    file.path(app_dir, ".."),
    file.path(app_dir, "..", "..")
  ), mustWork = FALSE))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "data", "processed", "metadata_index.csv"))) {
      return(candidate)
    }
  }

  stop("Could not find project root containing data/processed/metadata_index.csv")
}

project_root <- find_project_root()

metadata_file <- file.path(project_root, "data", "processed", "metadata_index.csv")
compounds_file <- file.path(project_root, "data", "processed", "compounds_reference.csv")
ccs_drift_time_script <- file.path(project_root, "scripts", "ccs_drift_time.R")
chromatograms_script <- file.path(project_root, "scripts", "parquet_chromatograms.R")
ms2_reference_script <- file.path(project_root, "scripts", "ms2_reference_spectra.R")
nextcloud_script <- file.path(project_root, "scripts", "nextcloud_public_webdav.R")

if (file.exists(ccs_drift_time_script)) {
  source(ccs_drift_time_script)
} else {
  stop("Missing script: ", ccs_drift_time_script)
}

if (file.exists(chromatograms_script)) {
  source(chromatograms_script)
} else {
  stop("Missing script: ", chromatograms_script)
}

if (file.exists(ms2_reference_script)) {
  source(ms2_reference_script)
} else {
  stop("Missing script: ", ms2_reference_script)
}

if (file.exists(nextcloud_script)) {
  source(nextcloud_script)
} else {
  stop("Missing script: ", nextcloud_script)
}

read_app_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Missing file: ", path)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

sanitize_csv_for_export <- function(data) {
  export <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  export[] <- lapply(export, function(column) {
    if (is.factor(column)) {
      column <- as.character(column)
    }
    if (!is.character(column)) {
      return(column)
    }
    formula <- !is.na(column) & grepl("^[[:space:]]*[=+@-]", column)
    column[formula] <- paste0("'", column[formula])
    column
  })
  export
}

default_parquet_root <- function() {
  env_path <- Sys.getenv("DATA_PATH", unset = "")
  if (nzchar(env_path)) {
    return(normalizePath(env_path, mustWork = FALSE))
  }
  file.path(project_root, "data", "raw", "parquet")
}

default_nextcloud_url <- function() {
  configured_url <- trimws(Sys.getenv("NEXTCLOUD_PUBLIC_URL", unset = ""))
  if (!nzchar(configured_url)) {
    return("")
  }
  # A public-share URL contains a secret token. Only display its server URL.
  if (nzchar(nextcloud_share_token_from_url(configured_url))) {
    return(tryCatch(nextcloud_base_url(configured_url), error = function(e) ""))
  }
  configured_url
}

default_nextcloud_share_token <- function() {
  # Never prefill a server secret into a browser input.
  ""
}

configured_nextcloud_share_token <- function() {
  token <- trimws(Sys.getenv("NEXTCLOUD_SHARE_TOKEN", unset = ""))
  if (nzchar(token)) {
    return(token)
  }
  nextcloud_share_token_from_url(Sys.getenv("NEXTCLOUD_PUBLIC_URL", unset = ""))
}

resolve_nextcloud_share_token <- function(input_token) {
  token <- if (is.null(input_token) || length(input_token) == 0 || is.na(input_token[[1]])) {
    ""
  } else {
    trimws(as.character(input_token[[1]]))
  }
  if (nzchar(token)) token else configured_nextcloud_share_token()
}

default_nextcloud_access_mode <- function() {
  mode <- Sys.getenv("NEXTCLOUD_ACCESS_MODE", unset = "public")
  if (mode %in% c("public", "account")) mode else "public"
}

default_nextcloud_username <- function() {
  Sys.getenv("NEXTCLOUD_USERNAME", unset = "")
}

format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  if (is.na(bytes)) return("-")
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- bytes
  unit <- units[1]
  for (candidate in units) {
    unit <- candidate
    if (value < 1024 || candidate == units[length(units)]) break
    value <- value / 1024
  }
  paste0(format(round(value, 2), nsmall = 2, trim = TRUE), " ", unit)
}

relative_to_root <- function(paths, root) {
  root <- normalizePath(root, mustWork = FALSE)
  paths <- normalizePath(paths, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  ifelse(startsWith(paths, prefix), substring(paths, nchar(prefix) + 1), paths)
}

empty_parquet_files <- function() {
  data.frame(
    parquet_id = character(),
    relative_path = character(),
    full_path = character(),
    source_type = character(),
    source_url = character(),
    size = numeric(),
    size_label = character(),
    modified = character(),
    stringsAsFactors = FALSE
  )
}

list_parquet_files <- function(root) {
  if (!dir.exists(root)) {
    return(empty_parquet_files())
  }

  paths <- list.files(root, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  paths <- sort(normalizePath(paths, mustWork = FALSE))
  if (length(paths) == 0) {
    return(empty_parquet_files())
  }

  info <- file.info(paths)
  relative_paths <- relative_to_root(paths, root)
  data.frame(
    # The relative path is stable across a refresh, unlike a generated row number.
    parquet_id = relative_paths,
    relative_path = relative_paths,
    full_path = paths,
    source_type = "local",
    source_url = NA_character_,
    size = info$size,
    size_label = vapply(info$size, format_bytes, character(1)),
    modified = format(info$mtime, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

nextcloud_to_parquet_files <- function(files) {
  if (is.null(files) || nrow(files) == 0) {
    return(empty_parquet_files())
  }
  paths <- normalize_relative_path(files$path)
  data.frame(
    parquet_id = paste0("nextcloud::", paths),
    relative_path = paths,
    full_path = NA_character_,
    source_type = "nextcloud",
    source_url = as.character(files$url),
    size = suppressWarnings(as.numeric(files$size)),
    size_label = vapply(suppressWarnings(as.numeric(files$size)), format_bytes, character(1)),
    modified = as.character(files$modified),
    stringsAsFactors = FALSE
  )
}

normalize_relative_path <- function(path) {
  gsub("\\\\", "/", as.character(path), fixed = FALSE)
}

metadata_relative_key <- function(path) {
  paths <- normalize_relative_path(path)
  paths <- sub("^/+", "", paths)
  vapply(paths, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return("")
    }
    parts <- strsplit(value, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    mode_index <- which(tolower(parts) %in% c("pos", "neg"))
    if (length(mode_index) != 1L) {
      return(paste(parts, collapse = "/"))
    }
    mode_index <- mode_index[[1]]
    year_index <- mode_index - 1L
    if (year_index >= 1L && grepl("^\\d{4}$", parts[[year_index]])) {
      return(paste(parts[year_index:length(parts)], collapse = "/"))
    }
    paste(parts[mode_index:length(parts)], collapse = "/")
  }, character(1))
}

valid_mode <- function(x) {
  length(x) == 1 && !is.na(x) && tolower(as.character(x)) %in% c("pos", "neg")
}

parse_optional_logical <- function(x) {
  values <- tolower(trimws(as.character(x)))
  result <- rep(NA, length(values))
  result[values %in% c("true", "t", "1", "yes", "y", "oui")] <- TRUE
  result[values %in% c("false", "f", "0", "no", "n", "non")] <- FALSE
  result
}

mode_from_parquet_path <- function(relative_path) {
  path_parts <- strsplit(tolower(normalize_relative_path(relative_path)), "/", fixed = TRUE)[[1]]
  matches <- unique(path_parts[path_parts %in% c("pos", "neg")])
  if (length(matches) == 1) matches[[1]] else NA_character_
}

filename_stem <- function(paths) {
  paths <- normalize_relative_path(paths)
  paths[is.na(paths)] <- ""
  sub("\\.parquet$", "", basename(paths), ignore.case = TRUE)
}

filename_capture <- function(values, pattern, group = 1L) {
  values <- as.character(values)
  values[is.na(values)] <- ""
  matches <- regmatches(values, regexec(pattern, values, perl = TRUE, ignore.case = TRUE))
  capture_index <- as.integer(group) + 1L
  vapply(matches, function(parts) {
    if (length(parts) >= capture_index && nzchar(parts[[capture_index]])) {
      parts[[capture_index]]
    } else {
      NA_character_
    }
  }, character(1))
}

metadata_from_parquet_filename <- function(relative_paths) {
  stem <- filename_stem(relative_paths)
  reference_year <- filename_capture(
    stem,
    "(?:^|[^0-9])(20[0-9]{2})[-_](0[1-9]|1[0-2])",
    group = 1
  )
  reference_month <- filename_capture(
    stem,
    "(?:^|[^0-9])(20[0-9]{2})[-_](0[1-9]|1[0-2])",
    group = 2
  )
  compact_duplicate <- filename_capture(
    stem,
    "20[0-9]{2}[-_](?:0[1-9]|1[0-2])([A-Za-z])(?=[_-]|$)"
  )
  named_duplicate <- filename_capture(stem, "-clichy-([A-Za-z0-9]+)(?:_replicate|$)")
  duplicate_label <- ifelse(!is.na(compact_duplicate), compact_duplicate, named_duplicate)
  is_blank <- grepl(
    "(^|[^[:alpha:]])(blanc|blank)([^[:alpha:]]|$)",
    stem,
    ignore.case = TRUE,
    perl = TRUE
  )
  sample_base_name <- ifelse(nzchar(stem), sub("_replicate_[^/]+$", "", stem, ignore.case = TRUE), NA_character_)

  data.frame(
    reference_year = reference_year,
    reference_month = reference_month,
    file_type = ifelse(
      is_blank,
      "Blanc",
      ifelse(!is.na(reference_year), "Echantillon", NA_character_)
    ),
    duplicate_label = duplicate_label,
    replicate_label = filename_capture(stem, "_replicate_(.+)$"),
    sample_name = ifelse(nzchar(stem), stem, NA_character_),
    sample_base_name = sample_base_name,
    sample_group = sample_base_name,
    stringsAsFactors = FALSE
  )
}

fill_missing_metadata <- function(values, fallback) {
  values <- as.character(values)
  fallback <- as.character(fallback)
  missing <- is.na(values) | values == ""
  values[missing] <- fallback[missing]
  values
}

enrich_parquet_files <- function(files, metadata) {
  if (nrow(files) == 0) {
    files$metadata_match <- logical()
    files$metadata_mode <- character()
    files$path_mode <- character()
    files$parquet_mode <- character()
    files$mode_source <- character()
    files$reference_year <- character()
    files$reference_month <- character()
    files$file_type <- character()
    files$duplicate_label <- character()
    files$replicate_label <- character()
    files$sample_name <- character()
    files$sample_base_name <- character()
    files$sample_group <- character()
    files$sample_result_id <- character()
    files$has_ccs_calibration <- logical()
    files$ccs_calibration_c1 <- numeric()
    files$ccs_calibration_c2 <- numeric()
    return(files)
  }

  lookup_columns <- c(
    "parquet_relative_path", "mode_dir", "reference_year", "reference_month",
    "file_type", "duplicate_label", "replicate_label", "sample_name", "sample_base_name", "sample_group",
    "sample_result_id", "has_ccs_calibration", "ccs_calibration_c1", "ccs_calibration_c2"
  )
  if (!"parquet_relative_path" %in% names(metadata)) {
    stop("metadata_index.csv must contain the column 'parquet_relative_path'.")
  }
  available_columns <- intersect(lookup_columns, names(metadata))
  lookup <- metadata[, available_columns, drop = FALSE]
  # Nextcloud may add a storage-root prefix such as `observatoire-db/`.
  # Keep only the stable year/mode/file part, and never fall back to a basename:
  # blank filenames may be identical across years or ionisation modes.
  lookup$key <- metadata_relative_key(lookup$parquet_relative_path)
  valid_lookup_key <- !is.na(lookup$key) & nzchar(lookup$key)
  lookup <- lookup[valid_lookup_key & !duplicated(lookup$key), , drop = FALSE]
  files$path_mode <- vapply(files$relative_path, mode_from_parquet_path, character(1))
  match_index <- match(metadata_relative_key(files$relative_path), lookup$key)
  matched <- !is.na(match_index)

  value_from_lookup <- function(column) {
    value <- rep(NA_character_, nrow(files))
    if (column %in% names(lookup) && any(matched)) {
      value[matched] <- as.character(lookup[[column]][match_index[matched]])
    }
    value
  }
  numeric_from_lookup <- function(column) {
    suppressWarnings(as.numeric(value_from_lookup(column)))
  }
  logical_from_lookup <- function(column) {
    parse_optional_logical(value_from_lookup(column))
  }

  files$metadata_match <- matched
  files$metadata_mode <- tolower(value_from_lookup("mode_dir"))
  metadata_mode_valid <- vapply(files$metadata_mode, valid_mode, logical(1))
  path_mode_valid <- vapply(files$path_mode, valid_mode, logical(1))
  # The storage path is authoritative when it contradicts an associated JSON.
  # This keeps a blank in a `neg` folder negative even when its filename is
  # ambiguous or its basename was matched to another metadata entry.
  path_preferred <- path_mode_valid & (!metadata_mode_valid | files$path_mode != files$metadata_mode)
  files$parquet_mode <- ifelse(
    path_preferred,
    files$path_mode,
    ifelse(metadata_mode_valid, files$metadata_mode, files$path_mode)
  )
  files$mode_source <- ifelse(
    path_preferred,
    "Chemin",
    ifelse(metadata_mode_valid, "JSON", ifelse(path_mode_valid, "Chemin", "Inconnu"))
  )
  filename_metadata <- metadata_from_parquet_filename(files$relative_path)
  files$reference_year <- fill_missing_metadata(value_from_lookup("reference_year"), filename_metadata$reference_year)
  files$reference_month <- fill_missing_metadata(value_from_lookup("reference_month"), filename_metadata$reference_month)
  files$file_type <- fill_missing_metadata(value_from_lookup("file_type"), filename_metadata$file_type)
  files$duplicate_label <- fill_missing_metadata(value_from_lookup("duplicate_label"), filename_metadata$duplicate_label)
  files$replicate_label <- fill_missing_metadata(value_from_lookup("replicate_label"), filename_metadata$replicate_label)
  files$sample_name <- fill_missing_metadata(value_from_lookup("sample_name"), filename_metadata$sample_name)
  files$sample_base_name <- fill_missing_metadata(value_from_lookup("sample_base_name"), filename_metadata$sample_base_name)
  files$sample_group <- fill_missing_metadata(value_from_lookup("sample_group"), filename_metadata$sample_group)
  files$sample_result_id <- value_from_lookup("sample_result_id")
  files$has_ccs_calibration <- logical_from_lookup("has_ccs_calibration")
  files$ccs_calibration_c1 <- numeric_from_lookup("ccs_calibration_c1")
  files$ccs_calibration_c2 <- numeric_from_lookup("ccs_calibration_c2")
  files
}

ccs_calibration_for_file <- function(file) {
  value <- function(column) {
    if (is.null(file) || nrow(file) == 0 || !column %in% names(file)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(file[[column]][[1]]))
  }
  list(C1 = value("ccs_calibration_c1"), C2 = value("ccs_calibration_c2"))
}

arcms_ccs_to_drifttime_converter <- function() {
  if (!requireNamespace("arcMS", quietly = TRUE)) {
    return(NULL)
  }
  namespace <- asNamespace("arcMS")
  if (!exists("convert_ccs_to_drifttime", envir = namespace, inherits = FALSE)) {
    return(NULL)
  }
  get("convert_ccs_to_drifttime", envir = namespace, inherits = FALSE)
}

make_parquet_choices <- function(x) {
  if (nrow(x) == 0) {
    return(c("Aucun fichier Parquet" = ""))
  }
  source_label <- ifelse(x$source_type == "nextcloud", "Nextcloud", "Local")
  labels <- paste0("[", source_label, "] ", x$relative_path, " | ", x$size_label)
  c("Choisir" = "", stats::setNames(x$parquet_id, labels))
}

make_nextcloud_file_choices <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(c("Aucun fichier" = ""))
  }
  modes <- vapply(x$path, mode_from_parquet_path, character(1))
  mode_labels <- ifelse(vapply(modes, valid_mode, logical(1)), toupper(modes), "MODE ?")
  labels <- paste0("[", mode_labels, "] ", x$path, " | ", vapply(x$size, format_bytes, character(1)))
  c("Choisir" = "", stats::setNames(x$path, labels))
}

make_screening_source_choices <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(c("Aucun fichier" = ""))
  }
  source_label <- ifelse(x$source_type == "nextcloud", "Nextcloud", "Local")
  has_mode <- vapply(x$parquet_mode, valid_mode, logical(1))
  mode_label <- ifelse(has_mode, toupper(x$parquet_mode), "mode inconnu")
  labels <- paste0("[", source_label, "] ", x$relative_path, " | ", mode_label)
  c("Choisir" = "", stats::setNames(x$parquet_id, labels))
}

monitoring_metric_choices <- c(
  "Somme des intensites EIC (fenetre RT)" = "rt_area_sum",
  "Intensite maximale EIC (fenetre RT)" = "rt_max_intensity"
)

monitoring_metric_label <- function(metric) {
  switch(
    as.character(metric[[1]]),
    "rt_area_sum" = "Somme des intensites EIC (fenetre RT)",
    "rt_max_intensity" = "Intensite maximale EIC (fenetre RT)",
    "Signal EIC"
  )
}

monitoring_treatment_label <- function(treatment) {
  switch(
    as.character(treatment[[1]]),
    "blank_corrected" = "Signal corrige du blanc",
    "Signal brut"
  )
}

monitoring_normalization_label <- function(normalization) {
  switch(
    as.character(normalization[[1]]),
    "internal_standard" = "Par etalon interne choisi",
    "closest_internal_standard" = "Par etalon interne le plus proche en RT",
    "total_intensity" = "Par intensite totale du niveau MS",
    "Aucune"
  )
}

monitoring_uses_internal_standard <- function(normalization) {
  as.character(normalization[[1]]) %in% c("internal_standard", "closest_internal_standard")
}

monitoring_value_label <- function(metric, treatment, normalization = "none", reference_name = NA_character_) {
  label <- monitoring_metric_label(metric)
  if (identical(treatment, "blank_corrected")) {
    label <- paste0(label, " corrigee du blanc")
  }
  if (monitoring_uses_internal_standard(normalization)) {
    reference_label <- if (is.character(reference_name) && length(reference_name) == 1 &&
      !is.na(reference_name) && nzchar(reference_name)) {
      reference_name
    } else {
      "etalon interne"
    }
    reference_prefix <- if (identical(normalization, "closest_internal_standard")) {
      "etalon proche RT: "
    } else {
      ""
    }
    label <- paste0(label, " / ", reference_prefix, reference_label)
  } else if (identical(normalization, "total_intensity")) {
    label <- paste0(label, " / intensite totale")
  }
  label
}

monitoring_reference_dates <- function(reference_year, reference_month, screened_at) {
  rows <- max(length(reference_year), length(reference_month), length(screened_at))
  if (rows == 0) {
    return(as.Date(character()))
  }

  year <- trimws(as.character(rep_len(reference_year, rows)))
  month <- suppressWarnings(as.integer(rep_len(reference_month, rows)))
  date <- as.Date(rep(NA_character_, rows))
  valid_period <- grepl("^20[0-9]{2}$", year) & is.finite(month) & month >= 1 & month <= 12
  date[valid_period] <- as.Date(sprintf("%s-%02d-01", year[valid_period], month[valid_period]))

  fallback <- suppressWarnings(as.Date(substr(as.character(rep_len(screened_at, rows)), 1, 10)))
  date[is.na(date)] <- fallback[is.na(date)]
  date
}

make_monitoring_compound_choices <- function(x) {
  required <- c("compound_id", "compound_name", "mode")
  if (is.null(x) || nrow(x) == 0 || !all(required %in% names(x))) {
    return(c("Aucun resultat de screening" = ""))
  }
  choice_columns <- c(required, intersect("compound_type", names(x)))
  choices <- unique(x[, choice_columns, drop = FALSE])
  choices <- choices[!is.na(choices$compound_id) & nzchar(choices$compound_id), , drop = FALSE]
  choices <- choices[order(tolower(choices$compound_name), choices$mode), , drop = FALSE]
  type_label <- if ("compound_type" %in% names(choices)) {
    paste0(" | ", compound_type_label(choices$compound_type))
  } else {
    ""
  }
  labels <- paste0(choices$compound_name, " | ", toupper(choices$mode), type_label)
  c("Choisir" = "", stats::setNames(as.character(choices$compound_id), labels))
}

make_monitoring_blank_choices <- function(x) {
  required <- c("parquet_id", "file", "mode")
  if (is.null(x) || nrow(x) == 0 || !all(required %in% names(x))) {
    return(c("Aucun blanc disponible" = ""))
  }
  year <- if ("reference_year" %in% names(x)) ifelse(is.na(x$reference_year), "-", x$reference_year) else "-"
  month <- if ("reference_month" %in% names(x)) ifelse(is.na(x$reference_month), "-", x$reference_month) else "-"
  labels <- paste0(year, "-", month, " | ", toupper(x$mode), " | ", x$file)
  c("Aucun blanc selectionne" = "", stats::setNames(as.character(x$parquet_id), labels))
}

make_monitoring_reference_choices <- function(x) {
  required <- c("compound_id", "compound_name", "mode")
  if (is.null(x) || nrow(x) == 0 || !all(required %in% names(x))) {
    return(c("Aucun etalon de reference disponible" = ""))
  }
  choices <- unique(x[, required, drop = FALSE])
  choices <- choices[!is.na(choices$compound_id) & nzchar(choices$compound_id), , drop = FALSE]
  choices <- choices[order(tolower(choices$compound_name), choices$mode), , drop = FALSE]
  labels <- paste0(choices$compound_name, " | ", toupper(choices$mode))
  c("Aucun etalon de reference selectionne" = "", stats::setNames(as.character(choices$compound_id), labels))
}

add_monitoring_compound_metadata <- function(results, catalog) {
  x <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(x) == 0 || !"compound_id" %in% names(x)) {
    return(x)
  }

  if (is.null(catalog) || nrow(catalog) == 0 || !"compound_id" %in% names(catalog)) {
    catalog <- data.frame(compound_id = character(), stringsAsFactors = FALSE)
  }
  catalog <- as.data.frame(catalog, stringsAsFactors = FALSE, check.names = FALSE)
  catalog <- catalog[!duplicated(catalog$compound_id), , drop = FALSE]
  match_index <- match(as.character(x$compound_id), as.character(catalog$compound_id))

  if (!"compound_type" %in% names(x)) {
    x$compound_type <- NA_character_
  }
  x$compound_type <- as.character(x$compound_type)
  catalog_type <- if ("compound_type" %in% names(catalog)) {
    as.character(catalog$compound_type[match_index])
  } else {
    rep(NA_character_, nrow(x))
  }
  missing_type <- is.na(x$compound_type) | !nzchar(trimws(x$compound_type))
  x$compound_type[missing_type] <- catalog_type[missing_type]

  if (!"expected_rt" %in% names(x)) {
    x$expected_rt <- NA_real_
  }
  x$expected_rt <- suppressWarnings(as.numeric(x$expected_rt))
  catalog_rt <- if ("rt" %in% names(catalog)) {
    suppressWarnings(as.numeric(catalog$rt[match_index]))
  } else {
    rep(NA_real_, nrow(x))
  }
  missing_rt <- !is.finite(x$expected_rt)
  x$expected_rt[missing_rt] <- catalog_rt[missing_rt]
  x
}

closest_internal_standard_by_rt <- function(results, target_id, target_mode = "") {
  empty <- function(status, target_rt = NA_real_) {
    data.frame(
      compound_id = NA_character_,
      compound_name = NA_character_,
      mode = NA_character_,
      target_expected_rt = target_rt,
      reference_expected_rt = NA_real_,
      rt_distance = NA_real_,
      selection_status = status,
      stringsAsFactors = FALSE
    )
  }

  x <- as.data.frame(results, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("compound_id", "compound_name", "mode", "compound_type", "expected_rt")
  if (nrow(x) == 0 || !all(required %in% names(x))) {
    return(empty("Aucun resultat de screening compatible"))
  }

  target_id <- trimws(as.character(target_id[[1]]))
  if (!nzchar(target_id)) {
    return(empty("Aucune molecule selectionnee"))
  }
  x$compound_id <- as.character(x$compound_id)
  x$compound_name <- as.character(x$compound_name)
  x$mode <- tolower(trimws(as.character(x$mode)))
  x$compound_type <- tolower(trimws(as.character(x$compound_type)))
  x$expected_rt <- suppressWarnings(as.numeric(x$expected_rt))

  target <- x[x$compound_id == target_id, , drop = FALSE]
  if (nrow(target) == 0) {
    return(empty("Molecule cible absente du lot"))
  }

  target_mode <- tolower(trimws(as.character(target_mode[[1]])))
  if (!nzchar(target_mode)) {
    modes <- unique(target$mode[vapply(target$mode, valid_mode, logical(1))])
    if (length(modes) != 1) {
      return(empty("Mode de la molecule cible ambigu"))
    }
    target_mode <- modes[[1]]
  }
  target <- target[target$mode == target_mode, , drop = FALSE]
  target_rt <- unique(target$expected_rt[is.finite(target$expected_rt)])
  if (length(target_rt) == 0) {
    return(empty("RT attendue absente pour la molecule cible"))
  }
  target_rt <- target_rt[[1]]

  candidates <- x[
    x$compound_id != target_id &
      x$mode == target_mode &
      x$compound_type == "internal_standard" &
      is.finite(x$expected_rt),
    c("compound_id", "compound_name", "mode", "expected_rt"),
    drop = FALSE
  ]
  candidates <- candidates[!duplicated(candidates$compound_id), , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(empty("Aucun etalon interne avec RT dans le lot", target_rt))
  }

  candidates$rt_distance <- abs(candidates$expected_rt - target_rt)
  candidates <- candidates[order(candidates$rt_distance, tolower(candidates$compound_name), candidates$compound_id), , drop = FALSE]
  selected <- candidates[1, , drop = FALSE]
  data.frame(
    compound_id = as.character(selected$compound_id),
    compound_name = as.character(selected$compound_name),
    mode = as.character(selected$mode),
    target_expected_rt = target_rt,
    reference_expected_rt = selected$expected_rt,
    rt_distance = selected$rt_distance,
    selection_status = "Etalon interne le plus proche selectionne",
    stringsAsFactors = FALSE
  )
}

format_monitoring_value <- function(value) {
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value)) {
    return("-")
  }
  if (value != 0 && abs(value) < 0.01) {
    return(format(value, scientific = TRUE, digits = 3, trim = TRUE))
  }
  format(round(value, 2), big.mark = " ", trim = TRUE, scientific = FALSE)
}

monitoring_aggregation_choices <- c(
  "Valeurs separees" = "none",
  "Moyenne" = "mean",
  "Mediane" = "median"
)

monitoring_aggregation_label <- function(method) {
  switch(
    as.character(method[[1]]),
    "mean" = "Moyenne",
    "median" = "Mediane",
    "Valeurs separees"
  )
}

monitoring_character_column <- function(x, column, default = "") {
  if (is.null(x) || nrow(x) == 0) {
    return(character())
  }
  values <- if (column %in% names(x)) as.character(x[[column]]) else rep(default, nrow(x))
  values[is.na(values)] <- default
  values
}

monitoring_blank_flags <- function(x) {
  tolower(monitoring_character_column(x, "file_type")) %in% c("blanc", "blank")
}

monitoring_sample_groups <- function(x) {
  candidates <- c("sample_group", "sample_base_name", "sample_name", "file", "parquet_id")
  values <- rep("", nrow(x))
  for (column in candidates) {
    candidate <- monitoring_character_column(x, column)
    missing <- !nzchar(values) & nzchar(candidate)
    values[missing] <- candidate[missing]
  }
  values <- sub("\\.parquet$", "", values, ignore.case = TRUE)
  values <- sub("_replicate_[^/]+$", "", values, ignore.case = TRUE)
  values[!nzchar(values)] <- "Echantillon non renseigne"
  values
}

monitoring_stage_groups <- function(x, stage) {
  stage <- match.arg(stage, c("injections", "duplicates"))
  period <- monitoring_character_column(x, "period_label", "Date inconnue")
  period[!nzchar(period)] <- "Date inconnue"
  compound <- monitoring_character_column(x, "compound_id", "Molecule non renseignee")
  mode <- monitoring_character_column(x, "mode", "Mode inconnu")
  duplicate <- monitoring_character_column(x, "duplicate_label", "Non renseigne")
  duplicate[!nzchar(duplicate)] <- "Non renseigne"
  sample_group <- monitoring_sample_groups(x)

  if (identical(stage, "injections")) {
    key <- paste(compound, mode, period, duplicate, sample_group, sep = "\r")
    label <- paste(period, duplicate, sample_group, sep = " | ")
  } else {
    key <- paste(compound, mode, period, sep = "\r")
    label <- paste(period, "Duplicats", sep = " | ")
  }
  list(key = key, label = label, sample_group = sample_group, duplicate = duplicate, period = period)
}

monitoring_mad_outlier_flags <- function(values, eligible, threshold = 3.5) {
  values <- suppressWarnings(as.numeric(values))
  threshold <- suppressWarnings(as.numeric(threshold[[1]]))
  flags <- rep(FALSE, length(values))
  eligible_index <- which(eligible & is.finite(values))
  if (length(eligible_index) < 3 || !is.finite(threshold) || threshold <= 0) {
    return(flags)
  }
  eligible_values <- values[eligible_index]
  centre <- stats::median(eligible_values)
  mad_value <- stats::median(abs(eligible_values - centre))
  if (!is.finite(mad_value) || mad_value <= 0) {
    return(flags)
  }
  robust_z <- 0.67448975 * (eligible_values - centre) / mad_value
  flags[eligible_index] <- abs(robust_z) > threshold
  flags
}

monitoring_aggregate_numeric <- function(values, index, method) {
  values <- suppressWarnings(as.numeric(values[index]))
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(NA_real_)
  }
  if (identical(method, "median")) stats::median(values) else mean(values)
}

monitoring_join_values <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  values <- unique(values)
  if (length(values) == 0) "" else paste(values, collapse = " | ")
}

monitoring_confidence_from_group <- function(group, index) {
  if (!"confidence_level" %in% names(group)) {
    return(list(level = NA_integer_, label = NA_character_))
  }
  levels <- suppressWarnings(as.integer(group$confidence_level))
  candidate_index <- index[is.finite(levels[index])]
  if (length(candidate_index) == 0) {
    candidate_index <- which(is.finite(levels))
  }
  if (length(candidate_index) == 0) {
    return(list(level = NA_integer_, label = NA_character_))
  }
  selected <- candidate_index[[which.max(levels[candidate_index])]]
  label <- if ("confidence_label" %in% names(group)) as.character(group$confidence_label[[selected]]) else NA_character_
  list(level = levels[[selected]], label = label)
}

aggregate_monitoring_stage <- function(
    x,
    stage = c("injections", "duplicates"),
    method = c("none", "mean", "median"),
    exclude_outliers = FALSE,
    outlier_threshold = 3.5) {
  stage <- match.arg(stage)
  method <- match.arg(method)
  if (is.null(x) || nrow(x) == 0) {
    return(list(results = x, audit = x))
  }
  if (!"signal_value" %in% names(x)) {
    stop("Les resultats de suivi ne contiennent pas la colonne signal_value.")
  }

  groups <- monitoring_stage_groups(x, stage)
  blank_flags <- monitoring_blank_flags(x)
  status <- monitoring_character_column(x, "status")
  values <- suppressWarnings(as.numeric(x$signal_value))
  eligible <- is.finite(values) & (if ("status" %in% names(x)) status == "Detected" else TRUE)
  group_index <- split(seq_len(nrow(x)), groups$key)
  result_rows <- vector("list", length(group_index))
  audit_rows <- vector("list", length(group_index))
  stage_label <- if (identical(stage, "injections")) "Injections" else "Duplicats"
  method_label <- monitoring_aggregation_label(method)
  numeric_columns <- c(
    "signal_value", "signal_brut", "signal_blanc_selectionne", "signal_corrige_blanc",
    "signal_traite", "signal_normalise", "normalisation_divisor", "rt_at_max",
    "rt_area_sum", "rt_max_intensity", "total_intensity",
    "normalisation_reference_expected_rt", "normalisation_reference_rt_distance"
  )

  for (group_number in seq_along(group_index)) {
    index <- group_index[[group_number]]
    group <- x[index, , drop = FALSE]
    group_blank <- blank_flags[index]
    group_eligible <- eligible[index]
    group_outlier <- if (isTRUE(exclude_outliers) && !all(group_blank)) {
      monitoring_mad_outlier_flags(values[index], group_eligible, outlier_threshold)
    } else {
      rep(FALSE, nrow(group))
    }
    used <- group_eligible & !group_outlier
    group_label <- groups$label[index[[1]]]
    n_total <- nrow(group)
    n_detected <- sum(status[index] == "Detected", na.rm = TRUE)
    n_eligible <- sum(group_eligible)
    n_excluded <- sum(group_outlier)

    audit <- group
    audit$aggregation_stage <- stage_label
    audit$aggregation_method <- method_label
    audit$aggregation_group <- group_label
    audit$aggregation_value_eligible <- group_eligible
    audit$aggregation_outlier <- group_outlier
    audit$aggregation_included <- if (identical(method, "none") || all(group_blank)) !group_outlier else used
    audit$aggregation_reason <- ifelse(
      group_blank,
      "Blanc conserve sans agregation",
      ifelse(
        group_outlier,
        "Exclue: atypique MAD",
        ifelse(
          group_eligible,
          if (identical(method, "none")) "Conservee individuellement" else "Utilisee pour agregation",
          "Non incluse: statut ou signal non exploitable"
        )
      )
    )
    audit_rows[[group_number]] <- audit

    if (all(group_blank) || identical(method, "none")) {
      kept <- group[!group_outlier, , drop = FALSE]
      if (nrow(kept) == 0) {
        result_rows[[group_number]] <- kept
        next
      }
      kept$aggregation_stage <- stage_label
      kept$aggregation_method <- if (all(group_blank)) "Blanc non agrege" else method_label
      kept$aggregation_group <- group_label
      kept$aggregation_n_total <- n_total
      kept$aggregation_n_detected <- n_detected
      kept$aggregation_n_eligible <- n_eligible
      kept$aggregation_n_used <- 1L
      kept$aggregation_n_excluded <- n_excluded
      kept$aggregation_exclusion_method <- if (isTRUE(exclude_outliers)) "MAD" else "Aucune"
      kept$aggregation_source_parquet_ids <- if ("aggregation_source_parquet_ids" %in% names(kept)) {
        monitoring_character_column(kept, "aggregation_source_parquet_ids")
      } else {
        monitoring_character_column(kept, "parquet_id")
      }
      kept$aggregation_source_files <- if ("aggregation_source_files" %in% names(kept)) {
        monitoring_character_column(kept, "aggregation_source_files")
      } else {
        monitoring_character_column(kept, "file")
      }
      kept$aggregation_is_aggregated <- FALSE
      result_rows[[group_number]] <- kept
      next
    }

    result <- group[1, , drop = FALSE]
    result$parquet_id <- paste0("aggregation::", stage, "::", group_number)
    if ("file" %in% names(result)) {
      result$file <- paste0("Agregat ", method_label, " - ", group_label)
    }
    if ("file_relative_path" %in% names(result)) {
      result$file_relative_path <- NA_character_
    }
    if ("source_type" %in% names(result)) {
      result$source_type <- "aggregation"
    }
    if ("duplicate_label" %in% names(result) && identical(stage, "duplicates")) {
      result$duplicate_label <- monitoring_join_values(monitoring_character_column(group, "duplicate_label"))
    }
    if ("replicate_label" %in% names(result)) {
      result$replicate_label <- monitoring_join_values(monitoring_character_column(group, "replicate_label"))
    }
    if ("sample_group" %in% names(result)) {
      result$sample_group <- if (identical(stage, "injections")) groups$sample_group[index[[1]]] else group_label
    }
    if ("sample_name" %in% names(result) && identical(stage, "duplicates")) {
      result$sample_name <- group_label
    }
    if ("selected_blank" %in% names(result)) {
      result$selected_blank <- FALSE
    }

    for (column in intersect(numeric_columns, names(result))) {
      result[[column]] <- monitoring_aggregate_numeric(group[[column]], which(used), method)
    }
    result$status <- if (sum(used) > 0) {
      "Detected"
    } else if (any(status[index] == "Error", na.rm = TRUE)) {
      "Error"
    } else {
      "Not Detected"
    }
    confidence <- monitoring_confidence_from_group(group, which(used))
    if ("confidence_level" %in% names(result)) result$confidence_level <- confidence$level
    if ("confidence_label" %in% names(result)) result$confidence_label <- confidence$label
    if ("normalisation_status" %in% names(result)) {
      result$normalisation_status <- if (sum(used) == 0) {
        "Non applicable"
      } else {
        monitoring_join_values(monitoring_character_column(group[which(used), , drop = FALSE], "normalisation_status"))
      }
    }
    if ("error" %in% names(result)) {
      result$error <- monitoring_join_values(monitoring_character_column(group, "error"))
    }
    result$aggregation_stage <- stage_label
    result$aggregation_method <- method_label
    result$aggregation_group <- group_label
    result$aggregation_n_total <- n_total
    result$aggregation_n_detected <- n_detected
    result$aggregation_n_eligible <- n_eligible
    result$aggregation_n_used <- sum(used)
    result$aggregation_n_excluded <- n_excluded
    result$aggregation_exclusion_method <- if (isTRUE(exclude_outliers)) "MAD" else "Aucune"
    result$aggregation_source_parquet_ids <- monitoring_join_values(if ("aggregation_source_parquet_ids" %in% names(group)) {
      monitoring_character_column(group, "aggregation_source_parquet_ids")
    } else {
      monitoring_character_column(group, "parquet_id")
    })
    result$aggregation_source_files <- monitoring_join_values(if ("aggregation_source_files" %in% names(group)) {
      monitoring_character_column(group, "aggregation_source_files")
    } else {
      monitoring_character_column(group, "file")
    })
    result$aggregation_is_aggregated <- TRUE
    result_rows[[group_number]] <- result
  }

  results <- do.call(rbind, result_rows)
  audit <- do.call(rbind, audit_rows)
  if (!is.null(results) && nrow(results) > 0 && "monitoring_date" %in% names(results)) {
    results <- results[order(results$monitoring_date, results$aggregation_group, na.last = TRUE), , drop = FALSE]
  }
  list(results = results, audit = audit)
}

summarise_monitoring_duplicates <- function(x) {
  empty <- data.frame(
    periode = character(),
    duplicat = character(),
    injections = character(),
    n_fichiers = integer(),
    n_detected = integer(),
    n_signaux = integer(),
    moyenne = numeric(),
    mediane = numeric(),
    ecart_type = numeric(),
    cv_pct = numeric(),
    minimum = numeric(),
    maximum = numeric(),
    stringsAsFactors = FALSE
  )
  required <- c("parquet_id", "signal_value")
  if (is.null(x) || nrow(x) == 0 || !all(required %in% names(x))) {
    return(empty)
  }

  values <- x
  if ("file_type" %in% names(values)) {
    values <- values[is.na(values$file_type) | values$file_type != "Blanc", , drop = FALSE]
  }
  if (nrow(values) == 0) {
    return(empty)
  }
  values <- values[!duplicated(values$parquet_id), , drop = FALSE]
  values$periode <- if ("period_label" %in% names(values)) {
    as.character(values$period_label)
  } else {
    "Date inconnue"
  }
  values$periode[is.na(values$periode) | !nzchar(values$periode)] <- "Date inconnue"
  values$duplicat <- if ("duplicate_label" %in% names(values)) {
    as.character(values$duplicate_label)
  } else {
    "Non renseigne"
  }
  values$duplicat[is.na(values$duplicat) | !nzchar(values$duplicat)] <- "Non renseigne"
  values$signal_value <- suppressWarnings(as.numeric(values$signal_value))

  group_ids <- interaction(values$periode, values$duplicat, drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(values)), group_ids)
  rows <- lapply(groups, function(index) {
    group <- values[index, , drop = FALSE]
    signal <- group$signal_value[is.finite(group$signal_value)]
    n_signal <- length(signal)
    mean_signal <- if (n_signal > 0) mean(signal) else NA_real_
    sd_signal <- if (n_signal > 1) stats::sd(signal) else NA_real_
    cv_signal <- if (n_signal > 1 && is.finite(mean_signal) && mean_signal > 0) {
      sd_signal / mean_signal * 100
    } else {
      NA_real_
    }
    replicate_values <- if ("replicate_label" %in% names(group)) {
      sort(unique(as.character(group$replicate_label)))
    } else {
      character()
    }
    replicate_values <- replicate_values[!is.na(replicate_values) & nzchar(replicate_values)]

    data.frame(
      periode = group$periode[[1]],
      duplicat = group$duplicat[[1]],
      injections = if (length(replicate_values) > 0) paste(replicate_values, collapse = ", ") else "-",
      n_fichiers = length(unique(group$parquet_id)),
      n_detected = if ("status" %in% names(group)) sum(group$status == "Detected", na.rm = TRUE) else NA_integer_,
      n_signaux = n_signal,
      moyenne = mean_signal,
      mediane = if (n_signal > 0) stats::median(signal) else NA_real_,
      ecart_type = sd_signal,
      cv_pct = cv_signal,
      minimum = if (n_signal > 0) min(signal) else NA_real_,
      maximum = if (n_signal > 0) max(signal) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result[order(result$periode, result$duplicat), , drop = FALSE]
}

schema_to_table <- function(schema) {
  lines <- capture.output(print(schema))
  lines <- trimws(lines)
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  lines <- lines[!startsWith(lines, "See ")]
  lines <- lines[!startsWith(lines, "Schema")]

  if (length(lines) == 0) {
    return(data.frame(column = character(), type = character(), stringsAsFactors = FALSE))
  }

  parts <- strsplit(lines, ":", fixed = TRUE)
  data.frame(
    column = vapply(parts, function(x) trimws(x[[1]]), character(1)),
    type = vapply(parts, function(x) trimws(paste(x[-1], collapse = ":")), character(1)),
    stringsAsFactors = FALSE
  )
}

control_data_columns <- function(x, column, default = NA_character_) {
  if (!is.null(x) && column %in% names(x)) {
    return(x[[column]])
  }
  rep(default, if (is.null(x)) 0L else nrow(x))
}

control_data_value <- function(x, column, index = 1L, default = NA_character_) {
  values <- control_data_columns(x, column, default = default)
  if (length(values) < index) default else values[[index]]
}

control_has_value <- function(value) {
  length(value) == 1 && !is.na(value) && nzchar(trimws(as.character(value)))
}

control_display_value <- function(value, fallback = "-") {
  if (control_has_value(value)) as.character(value) else fallback
}

catalog_file_diagnostics <- function(files) {
  output_columns <- c(
    "fichier", "source", "mode", "json", "annee", "mois", "type",
    "duplicat", "injection", "statut", "details"
  )
  if (is.null(files) || nrow(files) == 0) {
    return(as.data.frame(setNames(rep(list(character()), length(output_columns)), output_columns), stringsAsFactors = FALSE))
  }

  paths <- as.character(control_data_columns(files, "relative_path", default = ""))
  sources <- tolower(as.character(control_data_columns(files, "source_type", default = "local")))
  modes <- tolower(as.character(control_data_columns(files, "parquet_mode", default = "")))
  path_modes <- tolower(as.character(control_data_columns(files, "path_mode", default = "")))
  metadata_modes <- tolower(as.character(control_data_columns(files, "metadata_mode", default = "")))
  metadata_matches <- as.logical(control_data_columns(files, "metadata_match", default = FALSE))
  years <- as.character(control_data_columns(files, "reference_year", default = ""))
  months <- as.character(control_data_columns(files, "reference_month", default = ""))
  file_types <- as.character(control_data_columns(files, "file_type", default = ""))
  duplicates <- as.character(control_data_columns(files, "duplicate_label", default = ""))
  replicates <- as.character(control_data_columns(files, "replicate_label", default = ""))

  rows <- lapply(seq_len(nrow(files)), function(index) {
    issues <- character()
    warnings <- character()
    mode <- modes[[index]]
    path_mode <- path_modes[[index]]
    metadata_mode <- metadata_modes[[index]]
    file_type <- file_types[[index]]
    is_sample <- identical(tolower(file_type), "echantillon")
    known_type <- tolower(file_type) %in% c("echantillon", "blanc")

    if (!valid_mode(mode)) {
      issues <- c(issues, "mode absent ou invalide")
    }
    if (valid_mode(path_mode) && valid_mode(metadata_mode) && path_mode != metadata_mode) {
      issues <- c(issues, "modes JSON et chemin differents")
    }
    if (!isTRUE(metadata_matches[[index]])) {
      warnings <- c(warnings, "JSON non associe")
    }
    if (!known_type) {
      warnings <- c(warnings, "type non determine")
    }
    if (is_sample && !control_has_value(years[[index]])) {
      warnings <- c(warnings, "annee absente")
    }
    if (is_sample && !control_has_value(months[[index]])) {
      warnings <- c(warnings, "mois absent")
    }
    if (is_sample && !control_has_value(duplicates[[index]])) {
      warnings <- c(warnings, "duplicat absent")
    }
    if (!control_has_value(replicates[[index]])) {
      warnings <- c(warnings, "injection absente")
    }

    status <- if (length(issues) > 0) {
      "A corriger"
    } else if (length(warnings) > 0) {
      "A verifier"
    } else {
      "OK"
    }
    source <- if (identical(sources[[index]], "nextcloud")) "Nextcloud" else "Local"
    data.frame(
      fichier = paths[[index]],
      source = source,
      mode = if (valid_mode(mode)) toupper(mode) else "-",
      json = if (isTRUE(metadata_matches[[index]])) "Associe" else "Non associe",
      annee = control_display_value(years[[index]]),
      mois = control_display_value(months[[index]]),
      type = control_display_value(file_type),
      duplicat = control_display_value(duplicates[[index]]),
      injection = control_display_value(replicates[[index]]),
      statut = status,
      details = if (length(c(issues, warnings)) == 0) {
        "Metadonnees exploitables"
      } else {
        paste(c(issues, warnings), collapse = "; ")
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

parquet_file_diagnostics <- function(info = NULL, file = NULL, error = NULL) {
  empty <- data.frame(
    controle = character(),
    valeur = character(),
    statut = character(),
    details = character(),
    stringsAsFactors = FALSE
  )
  if (control_has_value(error)) {
    return(data.frame(
      controle = "Lecture Parquet",
      valeur = "Impossible",
      statut = "Erreur",
      details = as.character(error),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(info)) {
    return(data.frame(
      controle = "Lecture Parquet",
      valeur = "En attente",
      statut = "En attente",
      details = "Selectionnez un fichier et lancez son analyse",
      stringsAsFactors = FALSE
    ))
  }

  columns <- as.character(info$columns %||% character())
  required_columns <- c("rt", "scanid", "mslevel", "mz", "intensity")
  missing_columns <- setdiff(required_columns, columns)
  summary <- info$summary %||% data.frame()
  rows_count <- suppressWarnings(as.numeric(control_data_value(summary, "rows")))
  ms_levels <- as.character(control_data_columns(info$ms_levels %||% data.frame(), "mslevel"))
  ms_levels <- ms_levels[!is.na(ms_levels) & nzchar(ms_levels)]
  file_name <- control_display_value(control_data_value(file, "relative_path"), fallback = "Fichier non identifie")
  source_type <- tolower(as.character(control_data_value(file, "source_type", default = "")))
  source_label <- if (identical(source_type, "nextcloud")) "Nextcloud" else "Local"
  metadata_match <- isTRUE(as.logical(control_data_value(file, "metadata_match", default = FALSE)))
  file_mode <- tolower(as.character(control_data_value(file, "parquet_mode", default = "")))
  calibration_declared <- parse_optional_logical(control_data_value(file, "has_ccs_calibration", default = NA))[[1]]
  calibration_c1 <- suppressWarnings(as.numeric(control_data_value(file, "ccs_calibration_c1", default = NA_real_)))
  calibration_c2 <- suppressWarnings(as.numeric(control_data_value(file, "ccs_calibration_c2", default = NA_real_)))
  calibration_details <- if (isTRUE(calibration_declared)) {
    if (is.finite(calibration_c1) && is.finite(calibration_c2)) {
      "C1/C2 disponibles pour la future conversion CCS -> DT"
    } else {
      "C1/C2 restent a fournir pour la conversion CCS -> DT"
    }
  } else if (identical(calibration_declared, FALSE)) {
    "Aucune calibration CCS declaree dans le JSON"
  } else {
    "Etat de calibration absent des metadonnees"
  }

  metadata_rows <- if (is.null(file) || nrow(file) == 0) {
    empty
  } else {
    data.frame(
      controle = c("Fichier", "Metadonnees JSON", "Mode", "Calibration CCS JSON"),
      valeur = c(
        file_name,
        if (metadata_match) "Associees" else "Non associees",
        if (valid_mode(file_mode)) toupper(file_mode) else "Inconnu",
        if (isTRUE(calibration_declared)) "Declaree" else if (identical(calibration_declared, FALSE)) "Non declaree" else "Inconnue"
      ),
      statut = c(
        "Information",
        if (metadata_match) "OK" else "A verifier",
        if (valid_mode(file_mode)) "OK" else "A corriger",
        "Information"
      ),
      details = c(
        source_label,
        if (metadata_match) "" else "Le nom ou le chemin ne correspond pas a l'index JSON",
        control_display_value(control_data_value(file, "mode_source"), fallback = "Aucune source de mode"),
        calibration_details
      ),
      stringsAsFactors = FALSE
    )
  }

  schema_rows <- rbind(
    data.frame(
      controle = "Colonnes requises",
      valeur = if (length(missing_columns) == 0) "Toutes presentes" else paste("Manquantes:", paste(missing_columns, collapse = ", ")),
      statut = if (length(missing_columns) == 0) "OK" else "A corriger",
      details = paste(required_columns, collapse = ", "),
      stringsAsFactors = FALSE
    ),
    data.frame(
      controle = "Lignes",
      valeur = if (is.finite(rows_count)) format(rows_count, big.mark = " ", scientific = FALSE) else "Inconnu",
      statut = if (is.finite(rows_count) && rows_count > 0) "OK" else "A verifier",
      details = "",
      stringsAsFactors = FALSE
    ),
    data.frame(
      controle = "Niveaux MS",
      valeur = if (length(ms_levels) > 0) paste(ms_levels, collapse = ", ") else "Aucun",
      statut = if ("1" %in% ms_levels) "OK" else "A verifier",
      details = "MS1 est utilise par defaut pour TIC, BPI et screening",
      stringsAsFactors = FALSE
    ),
    data.frame(
      controle = "Mobilite ionique (dt)",
      valeur = if ("dt" %in% columns) "Disponible" else "Non disponible",
      statut = "Information",
      details = "Optionnelle et non decisive pour la detection",
      stringsAsFactors = FALSE
    ),
    data.frame(
      controle = "CCS",
      valeur = if ("ccs" %in% columns) "Disponible" else "Non disponible",
      statut = "Information",
      details = "Compatibilite pour un Parquet enrichi ; le flux standard utilisera CCS -> DT",
      stringsAsFactors = FALSE
    ),
    data.frame(
      controle = "Nombre de colonnes",
      valeur = as.character(length(columns)),
      statut = "Information",
      details = paste(columns, collapse = ", "),
      stringsAsFactors = FALSE
    )
  )
  rbind(metadata_rows, schema_rows)
}

global_data_checks <- function(metadata, compounds, files) {
  metadata_paths <- if ("parquet_relative_path" %in% names(metadata)) {
    normalize_relative_path(metadata$parquet_relative_path)
  } else {
    character()
  }
  duplicate_metadata_paths <- sum(duplicated(metadata_paths) | duplicated(metadata_paths, fromLast = TRUE))
  metadata_mode_consistent <- if ("mode_consistent" %in% names(metadata) && nrow(metadata) > 0) {
    sum(as.logical(metadata$mode_consistent), na.rm = TRUE)
  } else {
    0L
  }
  catalog_checks <- catalog_file_diagnostics(files)
  catalog_count <- nrow(catalog_checks)
  catalog_json_matches <- if (catalog_count > 0) sum(catalog_checks$json == "Associe") else 0L
  catalog_ready <- if (catalog_count > 0) sum(catalog_checks$statut == "OK") else 0L
  compound_modes <- tolower(as.character(control_data_columns(compounds, "mode", default = "")))
  compound_mz <- suppressWarnings(as.numeric(control_data_columns(compounds, "mz", default = NA_real_)))
  compound_rt <- suppressWarnings(as.numeric(control_data_columns(compounds, "rt", default = NA_real_)))
  compound_ccs <- suppressWarnings(as.numeric(control_data_columns(compounds, "ccs", default = NA_real_)))
  valid_compounds <- vapply(compound_modes, valid_mode, logical(1)) & is.finite(compound_mz) & compound_mz > 0
  ccs_outliers <- sum(is.finite(compound_ccs) & compound_ccs < 50)
  missing_rt <- sum(!is.finite(compound_rt))

  data.frame(
    controle = c(
      "Fichiers JSON indexes",
      "Modes JSON coherents avec chemins",
      "Chemins Parquet JSON dupliques",
      "Parquet catalogue avec JSON associe",
      "Parquet prets pour screening",
      "Etalons avec mode et m/z valides",
      "Etalons sans RT",
      "CCS inferieur a 50"
    ),
    valeur = c(
      nrow(metadata),
      paste0(metadata_mode_consistent, "/", nrow(metadata)),
      duplicate_metadata_paths,
      paste0(catalog_json_matches, "/", catalog_count),
      paste0(catalog_ready, "/", catalog_count),
      paste0(sum(valid_compounds), "/", nrow(compounds)),
      missing_rt,
      ccs_outliers
    ),
    statut = c(
      if (nrow(metadata) > 0) "OK" else "A verifier",
      if (nrow(metadata) > 0 && metadata_mode_consistent == nrow(metadata)) "OK" else "A verifier",
      if (duplicate_metadata_paths == 0) "OK" else "A corriger",
      if (catalog_count == 0) "Information" else if (catalog_json_matches == catalog_count) "OK" else "A verifier",
      if (catalog_count == 0) "Information" else if (catalog_ready == catalog_count) "OK" else "A verifier",
      if (length(valid_compounds) > 0 && all(valid_compounds)) "OK" else "A corriger",
      if (missing_rt == 0) "OK" else "A verifier",
      if (ccs_outliers == 0) "OK" else "A verifier"
    ),
    stringsAsFactors = FALSE
  )
}

inspect_parquet_file <- function(path, http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(inspect_remote_parquet_file(path, http_headers = http_headers))
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Le package R 'arrow' est necessaire pour lire les fichiers Parquet.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Le package R 'dplyr' est necessaire pour resumer les fichiers Parquet.")
  }
  if (!file.exists(path)) {
    stop("Fichier introuvable: ", path)
  }

  ds <- with_arrow_metadata(arrow::open_dataset(path))
  columns <- names(ds)

  summary_expressions <- list(rows = rlang::expr(dplyr::n()))
  for (column in intersect(c("rt", "mz", "intensity", "dt", "ccs", "bin"), columns)) {
    symbol <- rlang::sym(column)
    summary_expressions[[paste0("min_", column)]] <- rlang::expr(min(!!symbol, na.rm = TRUE))
    summary_expressions[[paste0("max_", column)]] <- rlang::expr(max(!!symbol, na.rm = TRUE))
  }
  summary <- with_arrow_metadata(as.data.frame(dplyr::collect(dplyr::summarise(ds, !!!summary_expressions))))
  required_columns <- c("rt", "scanid", "mslevel", "mz", "intensity")
  summary$missing_required_columns <- if (length(setdiff(required_columns, columns)) == 0) {
    "aucune"
  } else {
    paste(setdiff(required_columns, columns), collapse = ", ")
  }

  ms_levels <- if ("mslevel" %in% columns) {
    with_arrow_metadata(as.data.frame(dplyr::collect(dplyr::summarise(
      dplyr::group_by(ds, mslevel),
      rows = dplyr::n()
    ))))
  } else {
    data.frame(mslevel = character(), rows = numeric(), stringsAsFactors = FALSE)
  }

  preview_cols <- intersect(c("rt", "scanid", "mslevel", "mz", "intensity", "bin", "dt", "ccs"), columns)
  preview <- if (length(preview_cols) > 0) {
    with_arrow_metadata(as.data.frame(dplyr::collect(utils::head(dplyr::select(ds, dplyr::all_of(preview_cols)), 8))))
  } else {
    data.frame()
  }

  list(
    path = path,
    size = file.info(path)$size,
    schema = schema_to_table(ds$schema),
    columns = columns,
    summary = summary,
    ms_levels = ms_levels,
    preview = preview
  )
}

empty_parquet_source <- function() {
  list(path = NA_character_, http_headers = NULL)
}

empty_table <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)[0, , drop = FALSE]
}

format_plot_number <- function(x, digits = 6) {
  x <- suppressWarnings(as.numeric(x))
  result <- rep("-", length(x))
  valid <- is.finite(x)
  result[valid] <- formatC(x[valid], format = "fg", digits = digits, flag = "")
  result
}

chromatogram_hover_text <- function(x, prefix = NULL) {
  scanid <- if ("scanid" %in% names(x)) format_plot_number(x$scanid, digits = 0) else rep("-", nrow(x))
  n_points <- if ("n_points" %in% names(x)) format_plot_number(x$n_points, digits = 0) else rep("-", nrow(x))
  prefix <- if (is.null(prefix)) rep("", nrow(x)) else paste0("<b>", prefix, "</b><br>")
  paste0(
    prefix,
    "RT: ", format_plot_number(x$rt, digits = 6), " min",
    "<br>Intensite: ", format_plot_number(x$intensity, digits = 8),
    "<br>Scan: ", scanid,
    "<br>Points agreges: ", n_points
  )
}

local_extrema_indices <- function(values, max_each = 20L) {
  values <- suppressWarnings(as.numeric(values))
  n <- length(values)
  if (n < 3) {
    return(list(maxima = integer(), minima = integer()))
  }

  middle <- seq.int(2L, n - 1L)
  valid <- is.finite(values[middle - 1L]) & is.finite(values[middle]) & is.finite(values[middle + 1L])
  maxima <- middle[valid & values[middle] > values[middle - 1L] & values[middle] >= values[middle + 1L]]
  minima <- middle[valid & values[middle] < values[middle - 1L] & values[middle] <= values[middle + 1L]]

  select_prominent <- function(indices, kind) {
    if (length(indices) == 0) {
      return(integer())
    }
    neighbours <- if (identical(kind, "max")) {
      pmax(values[indices - 1L], values[indices + 1L])
    } else {
      pmin(values[indices - 1L], values[indices + 1L])
    }
    prominence <- if (identical(kind, "max")) values[indices] - neighbours else neighbours - values[indices]
    indices <- indices[order(prominence, decreasing = TRUE, na.last = NA)]
    sort(utils::head(indices, max_each))
  }

  list(
    maxima = select_prominent(maxima, "max"),
    minima = select_prominent(minima, "min")
  )
}

configure_interactive_plot <- function(chart) {
  plotly::config(
    chart,
    displaylogo = FALSE,
    scrollZoom = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )
}

empty_interactive_plot <- function(title, message) {
  chart <- plotly::plot_ly(
    x = numeric(),
    y = numeric(),
    type = "scatter",
    mode = "markers",
    hoverinfo = "skip",
    showlegend = FALSE
  )
  chart <- plotly::layout(
    chart,
    title = list(text = title),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    annotations = list(list(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    ))
  )
  configure_interactive_plot(chart)
}

plot_chromatogram <- function(x, title, y_label = "Intensity") {
  if (is.null(x) || nrow(x) == 0) {
    return(empty_interactive_plot(title, "Aucune donnee calculee"))
  }

  x <- as.data.frame(x)
  x$hover_text <- chromatogram_hover_text(x)
  extrema <- local_extrema_indices(x$intensity)
  chart <- plotly::plot_ly(
    x,
    x = ~rt,
    y = ~intensity,
    type = "scatter",
    mode = "lines+markers",
    name = "Signal",
    text = ~hover_text,
    hoverinfo = "text",
    line = list(color = "#215a6d", width = 1.4),
    marker = list(color = "#215a6d", size = 4)
  )
  if (length(extrema$maxima) > 0) {
    maxima <- x[extrema$maxima, , drop = FALSE]
    maxima$hover_text <- chromatogram_hover_text(maxima, "Maximum local")
    chart <- plotly::add_markers(
      chart,
      data = maxima,
      inherit = FALSE,
      x = ~rt,
      y = ~intensity,
      type = "scatter",
      mode = "markers",
      name = "Maxima locaux",
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(color = "#c97b3d", symbol = "triangle-up", size = 10)
    )
  }
  if (length(extrema$minima) > 0) {
    minima <- x[extrema$minima, , drop = FALSE]
    minima$hover_text <- chromatogram_hover_text(minima, "Minimum local")
    chart <- plotly::add_markers(
      chart,
      data = minima,
      inherit = FALSE,
      x = ~rt,
      y = ~intensity,
      type = "scatter",
      mode = "markers",
      name = "Minima locaux",
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(color = "#607d8b", symbol = "triangle-down", size = 10)
    )
  }
  chart <- plotly::layout(
    chart,
    title = list(text = title),
    xaxis = list(title = list(text = "RT (min)")),
    yaxis = list(title = list(text = y_label), rangemode = "tozero"),
    hovermode = "closest",
    dragmode = "zoom"
  )
  configure_interactive_plot(chart)
}

plot_ms2_spectrum <- function(x, title, comparison = NULL) {
  if (is.null(x) || nrow(x) == 0) {
    return(empty_interactive_plot(title, "Aucun signal MS2 dans la fenetre RT"))
  }

  x <- as.data.frame(x)
  x$hover_text <- paste0(
    "m/z fragment: ", format_plot_number(x$mz, digits = 6),
    "<br>Intensite agregee: ", format_plot_number(x$intensity, digits = 8),
    "<br>Intensite maximale: ", format_plot_number(x$max_intensity, digits = 8),
    "<br>Points agreges: ", format_plot_number(x$n_points, digits = 0)
  )
  sticks <- data.frame(
    mz = as.vector(rbind(x$mz, x$mz, rep(NA_real_, nrow(x)))),
    intensity = as.vector(rbind(rep(0, nrow(x)), x$intensity, rep(NA_real_, nrow(x))))
  )
  chart <- plotly::plot_ly(
    sticks,
    x = ~mz,
    y = ~intensity,
    type = "scatter",
    mode = "lines",
    hoverinfo = "skip",
    showlegend = FALSE,
    line = list(color = "#215a6d", width = 1.2)
  )
  chart <- plotly::add_markers(
    chart,
    data = x,
    inherit = FALSE,
    x = ~mz,
    y = ~intensity,
    type = "scatter",
    mode = "markers",
    name = "Fragments MS2",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(color = "#215a6d", size = 5)
  )
  if (is.list(comparison) && !is.null(comparison$matches) && nrow(comparison$matches) > 0) {
    matches <- as.data.frame(comparison$matches, stringsAsFactors = FALSE)
    matched <- matches[!is.na(matches$matched) & matches$matched, , drop = FALSE]
    unmatched <- matches[is.na(matches$matched) | !matches$matched, , drop = FALSE]
    if (nrow(matched) > 0) {
      matched$hover_text <- paste0(
        "<b>Fragment reference concordant</b>",
        "<br>m/z reference: ", format_plot_number(matched$reference_fragment_mz, digits = 6),
        "<br>m/z observe: ", format_plot_number(matched$observed_fragment_mz, digits = 6),
        "<br>Erreur: ", format_plot_number(matched$mz_error_da, digits = 6), " Da",
        "<br>Intensite observee: ", format_plot_number(matched$observed_intensity, digits = 8)
      )
      chart <- plotly::add_markers(
        chart,
        data = matched,
        inherit = FALSE,
        x = ~observed_fragment_mz,
        y = ~observed_intensity,
        type = "scatter",
        mode = "markers",
        name = "Fragments reference concordants",
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(color = "#1e8f4d", symbol = "circle-open", size = 13, line = list(width = 2))
      )
    }
    if (nrow(unmatched) > 0) {
      unmatched$baseline <- 0
      unmatched$hover_text <- paste0(
        "<b>Fragment reference non observe</b>",
        "<br>m/z reference: ", format_plot_number(unmatched$reference_fragment_mz, digits = 6),
        "<br>Intensite relative reference: ", format_plot_number(unmatched$reference_relative_intensity, digits = 3), "%"
      )
      chart <- plotly::add_markers(
        chart,
        data = unmatched,
        inherit = FALSE,
        x = ~reference_fragment_mz,
        y = ~baseline,
        type = "scatter",
        mode = "markers",
        name = "Fragments reference non observes",
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(color = "#c83a32", symbol = "x", size = 10)
      )
    }
  }
  top_peak <- x[which.max(x$intensity), , drop = FALSE]
  top_peak$hover_text <- paste0("<b>Fragment le plus intense</b><br>", top_peak$hover_text)
  chart <- plotly::add_markers(
    chart,
    data = top_peak,
    inherit = FALSE,
    x = ~mz,
    y = ~intensity,
    type = "scatter",
    mode = "markers",
    name = "Fragment principal",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(color = "#c97b3d", symbol = "diamond", size = 10)
  )
  chart <- plotly::layout(
    chart,
    title = list(text = title),
    xaxis = list(title = list(text = "m/z fragment")),
    yaxis = list(title = list(text = "Intensite MS2 agregee"), rangemode = "tozero"),
    hovermode = "closest",
    dragmode = "zoom"
  )
  configure_interactive_plot(chart)
}

summarise_chromatogram <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(data.frame(
      metric = c("points", "max_intensity", "rt_at_max", "area_sum"),
      value = c("0", "-", "-", "0"),
      stringsAsFactors = FALSE
    ))
  }

  max_index <- which.max(x$intensity)
  data.frame(
    metric = c("points", "max_intensity", "rt_at_max", "area_sum"),
    value = c(
      format(nrow(x), big.mark = " ", scientific = FALSE),
      format(max(x$intensity, na.rm = TRUE), big.mark = " ", scientific = FALSE),
      format(round(x$rt[[max_index]], 4), nsmall = 4),
      format(round(sum(x$intensity, na.rm = TRUE), 2), big.mark = " ", scientific = FALSE)
    ),
    stringsAsFactors = FALSE
  )
}

input_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(default)
  }
  as.numeric(x[[1]])
}

normalize_compound_mode <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("+", "positive", "positif")] <- "pos"
  x[x %in% c("-", "negative", "negatif")] <- "neg"
  x
}

parse_compound_decimal <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\"", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(x))
}

canonical_compound_column <- function(column) {
  key <- tolower(gsub("[^A-Za-z0-9]", "", trimws(as.character(column))))
  aliases <- c(
    "compoundid" = "compound_id",
    "id" = "compound_id",
    "name" = "name",
    "nom" = "name",
    "compoundname" = "name",
    "mode" = "mode",
    "ionisationmode" = "mode",
    "ionizationmode" = "mode",
    "mz" = "mz",
    "exactmass" = "mz",
    "mass" = "mz",
    "rt" = "rt",
    "retentiontime" = "rt",
    "dt" = "dt",
    "drifttime" = "dt",
    "mobility" = "dt",
    "ccs" = "ccs",
    "compoundtype" = "compound_type",
    "type" = "compound_type"
  )
  if (key %in% names(aliases)) unname(aliases[[key]]) else key
}

normalize_compound_input_columns <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- vapply(names(x), canonical_compound_column, character(1))
  x[, !duplicated(names(x)), drop = FALSE]
}

read_compounds_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop("Le fichier CSV importe est introuvable.")
  }
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0 || !nzchar(trimws(first_line[[1]]))) {
    stop("Le fichier CSV est vide.")
  }
  separators <- c(
    ";" = lengths(regmatches(first_line, gregexpr(";", first_line, fixed = TRUE))),
    "," = lengths(regmatches(first_line, gregexpr(",", first_line, fixed = TRUE))),
    "\t" = lengths(regmatches(first_line, gregexpr("\t", first_line, fixed = TRUE)))
  )
  separator <- names(separators)[which.max(separators)]
  if (separators[[separator]] == 0) {
    stop("Le CSV doit etre separe par des virgules, des points-virgules ou des tabulations.")
  }
  imported <- tryCatch(
    utils::read.table(
      path,
      header = TRUE,
      sep = separator,
      quote = "\"",
      comment.char = "",
      fill = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA", "N/A")
    ),
    error = function(e) e
  )
  if (inherits(imported, "error")) {
    stop("Impossible de lire le CSV : ", conditionMessage(imported))
  }
  normalize_compound_input_columns(imported)
}

read_batch_screening_export <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop("Le fichier CSV importe est introuvable.")
  }
  imported <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA", "N/A")
    ),
    error = function(e) e
  )
  if (inherits(imported, "error")) {
    stop("Impossible de lire le CSV de screening : ", conditionMessage(imported))
  }
  if (nrow(imported) == 0) {
    stop("Le CSV de screening ne contient aucun resultat.")
  }

  required <- c("parquet_id", "file", "compound_id", "compound_name", "mode", "status")
  missing <- setdiff(required, names(imported))
  if (length(missing) > 0) {
    stop(
      "Le CSV ne ressemble pas a un export de screening de lot. Colonnes manquantes : ",
      paste(missing, collapse = ", "), "."
    )
  }

  defaults <- list(
    source_type = "Import CSV",
    file_relative_path = NA_character_,
    file_mode = NA_character_,
    file_mode_source = NA_character_,
    reference_year = NA_character_,
    reference_month = NA_character_,
    file_type = NA_character_,
    duplicate_label = NA_character_,
    replicate_label = NA_character_,
    sample_name = NA_character_,
    sample_base_name = NA_character_,
    sample_group = NA_character_,
    compound_type = NA_character_,
    confidence_level = NA_integer_,
    confidence_label = "Erreur",
    expected_mz = NA_real_,
    mz_tolerance = NA_real_,
    expected_rt = NA_real_,
    rt_tolerance = NA_real_,
    min_intensity = NA_real_,
    mslevel = NA_character_,
    rt_area_sum = NA_real_,
    rt_max_intensity = NA_real_,
    rt_at_max = NA_real_,
    total_intensity = NA_real_,
    total_intensity_n_points = NA_integer_,
    total_intensity_mslevel = NA_character_,
    total_intensity_error = "",
    total_intensity_status = "",
    has_ccs_calibration = NA,
    ccs_calibration_c1 = NA_real_,
    ccs_calibration_c2 = NA_real_,
    expected_dt_from_ccs = NA_real_,
    ccs_to_dt_error_ms = NA_real_,
    ccs_to_dt_error_pct = NA_real_,
    ccs_to_dt_match = NA,
    ccs_to_dt_status = "Non evalue",
    ccs_tolerance_pct = NA_real_,
    ccs_error = NA_real_,
    ccs_error_pct = NA_real_,
    use_ccs = NA,
    screened_at = NA_character_,
    error = ""
  )
  for (column in names(defaults)) {
    if (!column %in% names(imported)) {
      imported[[column]] <- rep(defaults[[column]], nrow(imported))
    }
  }

  character_columns <- c(
    "parquet_id", "file", "compound_id", "compound_name", "compound_type", "mode", "status",
    "source_type", "file_relative_path", "file_mode", "file_mode_source", "reference_year",
    "reference_month", "file_type", "duplicate_label", "replicate_label", "sample_name", "sample_base_name", "sample_group",
    "confidence_label", "mslevel", "total_intensity_mslevel", "total_intensity_error", "total_intensity_status", "ccs_to_dt_status", "screened_at", "error"
  )
  for (column in intersect(character_columns, names(imported))) {
    imported[[column]] <- as.character(imported[[column]])
  }
  imported$mode <- tolower(trimws(imported$mode))

  numeric_columns <- c(
    "confidence_level", "expected_mz", "mz_tolerance", "expected_rt", "rt_tolerance", "min_intensity",
    "rt_area_sum", "rt_max_intensity", "rt_at_max", "total_intensity",
    "total_intensity_n_points", "ccs_calibration_c1", "ccs_calibration_c2", "expected_dt_from_ccs",
    "ccs_to_dt_error_ms", "ccs_to_dt_error_pct", "ccs_tolerance_pct", "ccs_error", "ccs_error_pct"
  )
  for (column in intersect(numeric_columns, names(imported))) {
    imported[[column]] <- suppressWarnings(as.numeric(imported[[column]]))
  }
  if ("has_ccs_calibration" %in% names(imported)) {
    imported$has_ccs_calibration <- parse_optional_logical(imported$has_ccs_calibration)
  }
  if ("ccs_to_dt_match" %in% names(imported)) {
    imported$ccs_to_dt_match <- parse_optional_logical(imported$ccs_to_dt_match)
  }
  imported
}

batch_result_eic_parameters <- function(result) {
  if (is.null(result) || nrow(result) != 1) {
    return(list(ok = FALSE, message = "Selectionne exactement une ligne de resultat."))
  }

  row_value <- function(column, default = "") {
    if (!column %in% names(result)) {
      return(default)
    }
    value <- result[[column]][[1]]
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(default)
    }
    trimws(as.character(value))
  }
  numeric_value <- function(column, default = NA_real_) {
    value <- suppressWarnings(as.numeric(row_value(column, NA_character_)))
    if (!is.finite(value)) default else value
  }

  parquet_id <- row_value("parquet_id")
  target_mz <- numeric_value("expected_mz")
  if (!nzchar(parquet_id)) {
    return(list(ok = FALSE, message = "La ligne selectionnee ne contient pas d'identifiant Parquet."))
  }
  if (!is.finite(target_mz) || target_mz <= 0) {
    return(list(ok = FALSE, message = "La ligne selectionnee ne contient pas de m/z attendu exploitable."))
  }

  file_mode <- normalize_compound_mode(row_value("file_mode"))
  if (!valid_mode(file_mode)) {
    file_mode <- normalize_compound_mode(row_value("mode"))
  }
  if (!valid_mode(file_mode)) {
    file_mode <- NA_character_
  }

  mz_tolerance <- numeric_value("mz_tolerance", 0.01)
  if (!is.finite(mz_tolerance) || mz_tolerance <= 0) mz_tolerance <- 0.01
  rt_tolerance <- numeric_value("rt_tolerance", 0.5)
  if (!is.finite(rt_tolerance) || rt_tolerance < 0) rt_tolerance <- 0.5
  min_intensity <- numeric_value("min_intensity")
  if (!is.finite(min_intensity) || min_intensity < 0) min_intensity <- NA_real_
  mslevel <- row_value("mslevel", "1")
  if (!mslevel %in% c("1", "2")) mslevel <- "1"

  list(
    ok = TRUE,
    parquet_id = parquet_id,
    file_mode = file_mode,
    compound_id = row_value("compound_id"),
    compound_name = row_value("compound_name", "molecule"),
    target_mz = target_mz,
    mz_tolerance = mz_tolerance,
    expected_rt = numeric_value("expected_rt"),
    rt_tolerance = rt_tolerance,
    min_intensity = min_intensity,
    mslevel = mslevel
  )
}

batch_result_monitoring_parameters <- function(result) {
  if (is.null(result) || nrow(result) != 1) {
    return(list(ok = FALSE, message = "Selectionne exactement une ligne de resultat."))
  }

  row_value <- function(column, default = "") {
    if (!column %in% names(result)) {
      return(default)
    }
    value <- result[[column]][[1]]
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(default)
    }
    trimws(as.character(value))
  }

  compound_id <- row_value("compound_id")
  compound_name <- row_value("compound_name", compound_id)
  mode <- normalize_compound_mode(row_value("mode"))
  if (!valid_mode(mode)) {
    mode <- normalize_compound_mode(row_value("file_mode"))
  }
  if (!nzchar(compound_id)) {
    return(list(ok = FALSE, message = "La ligne selectionnee ne contient pas de molecule a suivre."))
  }
  if (!valid_mode(mode)) {
    return(list(ok = FALSE, message = "Le mode de la molecule selectionnee est absent ou invalide."))
  }

  list(
    ok = TRUE,
    compound_id = compound_id,
    compound_name = compound_name,
    mode = mode
  )
}

compound_input_character <- function(x, column, rows) {
  if (!column %in% names(x)) {
    return(rep(NA_character_, rows))
  }
  value <- trimws(as.character(x[[column]]))
  value[value == ""] <- NA_character_
  value
}

compound_input_numeric <- function(x, column, rows, required = FALSE, positive = FALSE) {
  value <- compound_input_character(x, column, rows)
  parsed <- parse_compound_decimal(value)
  invalid <- !is.na(value) & !is.finite(parsed)
  if (any(invalid)) {
    stop(
      "Valeur numerique invalide dans la colonne '", column,
      "' aux lignes : ", paste(which(invalid), collapse = ", "), "."
    )
  }
  if (isTRUE(required) && any(!is.finite(parsed))) {
    stop("La colonne '", column, "' est obligatoire et doit etre numerique.")
  }
  if (isTRUE(positive) && any(is.finite(parsed) & parsed <= 0)) {
    stop("La colonne '", column, "' doit contenir des valeurs strictement positives.")
  }
  parsed
}

prepare_custom_compounds <- function(raw, app_ids, source_label, source_file,
                                     default_compound_type = "internal_standard") {
  raw <- normalize_compound_input_columns(raw)
  rows <- nrow(raw)
  if (rows == 0) {
    stop("Le fichier ne contient aucun etalon.")
  }
  if (length(app_ids) != rows) {
    stop("Nombre d'identifiants incoherent pour les etalons ajoutes.")
  }

  required_columns <- c("name", "mode", "mz")
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0) {
    stop("Le CSV doit contenir les colonnes : ", paste(required_columns, collapse = ", "), ".")
  }

  name <- compound_input_character(raw, "name", rows)
  if (any(is.na(name))) {
    stop("Chaque etalon doit avoir un nom.")
  }
  mode <- normalize_compound_mode(compound_input_character(raw, "mode", rows))
  if (any(!mode %in% c("pos", "neg"))) {
    stop("Chaque etalon doit avoir un mode pos ou neg.")
  }

  mz <- compound_input_numeric(raw, "mz", rows, required = TRUE, positive = TRUE)
  rt <- compound_input_numeric(raw, "rt", rows)
  dt <- compound_input_numeric(raw, "dt", rows, positive = TRUE)
  ccs <- compound_input_numeric(raw, "ccs", rows, positive = TRUE)
  compound_id <- compound_input_character(raw, "compound_id", rows)
  compound_id[is.na(compound_id)] <- app_ids[is.na(compound_id)]
  compound_id <- make.unique(compound_id, sep = "_")
  compound_type <- normalize_compound_type(
    compound_input_character(raw, "compound_type", rows),
    default = default_compound_type
  )

  data.frame(
    compound_id = compound_id,
    name = name,
    mode = mode,
    mz = mz,
    rt = rt,
    dt = dt,
    ccs = ccs,
    mz_tolerance = NA_real_,
    rt_tolerance = NA_real_,
    dt_tolerance = NA_real_,
    ccs_tolerance = NA_real_,
    compound_type = compound_type,
    source_file = source_file,
    app_compound_id = app_ids,
    source_label = source_label,
    stringsAsFactors = FALSE
  )
}

normalize_compound_type <- function(value, default = NA_character_) {
  values <- tolower(trimws(as.character(value)))
  values[is.na(values) | !nzchar(values)] <- NA_character_
  default <- tolower(trimws(as.character(default[[1]])))
  aliases <- c(
    "internal_standard" = "internal_standard",
    "internal standard" = "internal_standard",
    "etalon" = "internal_standard",
    "etalon interne" = "internal_standard",
    "standard" = "internal_standard",
    "suspect" = "suspect",
    "suspects" = "suspect"
  )
  normalized <- unname(aliases[values])
  normalized[is.na(values)] <- default
  invalid <- !is.na(values) & is.na(normalized)
  if (any(invalid)) {
    stop(
      "Type de molecule invalide aux lignes : ",
      paste(which(invalid), collapse = ", "),
      ". Utilise internal_standard ou suspect."
    )
  }
  normalized
}

compound_type_label <- function(compound_type) {
  type <- normalize_compound_type(compound_type, default = NA_character_)
  ifelse(type == "internal_standard", "Etalon interne", ifelse(type == "suspect", "Suspect", "Non renseigne"))
}

filter_compounds_by_type <- function(x, compound_type) {
  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }
  types <- if ("compound_type" %in% names(x)) {
    normalize_compound_type(x$compound_type, default = NA_character_)
  } else {
    rep(NA_character_, nrow(x))
  }
  x[types == compound_type, , drop = FALSE]
}

filter_catalog_compounds <- function(x, compound_type, mode = "", search = "", rt_range = numeric()) {
  x <- filter_compounds_by_type(x, compound_type)
  mode <- tolower(trimws(as.character(mode[[1]])))
  if (nzchar(mode)) {
    x <- x[x$mode == mode, , drop = FALSE]
  }
  search <- tolower(trimws(as.character(search[[1]])))
  if (nzchar(search)) {
    x <- x[grepl(search, tolower(x$name), fixed = TRUE), , drop = FALSE]
  }
  if (length(rt_range) == 2 && all(is.finite(rt_range))) {
    x <- x[
      is.na(x$rt) | (x$rt >= rt_range[[1]] & x$rt <= rt_range[[2]]),
      ,
      drop = FALSE
    ]
  }
  x
}

make_test_suspects_from_internal_standards <- function(standards, app_ids) {
  standards <- filter_compounds_by_type(standards, "internal_standard")
  if (nrow(standards) == 0) {
    return(standards)
  }
  if (length(app_ids) != nrow(standards)) {
    stop("Nombre d'identifiants incoherent pour les suspects de test.")
  }

  copies <- standards
  source_ids <- as.character(copies$compound_id)
  source_ids[is.na(source_ids) | !nzchar(source_ids)] <- seq_len(nrow(copies))
  copies$compound_id <- make.unique(paste0("suspect_test_", source_ids), sep = "_")
  copies$name <- paste0("[test] ", as.character(copies$name))
  copies$compound_type <- "suspect"
  copies$source_file <- "Copie depuis etalons internes"
  copies$source_label <- "Etalon interne copie pour test"
  copies$app_compound_id <- app_ids
  copies
}

compound_identity_key <- function(x) {
  paste(
    tolower(trimws(as.character(x$name))),
    tolower(trimws(as.character(x$mode))),
    sprintf("%.10f", as.numeric(x$mz)),
    sep = "|"
  )
}

metadata_index <- read_app_csv(metadata_file)
compounds_reference <- read_app_csv(compounds_file)
ms2_reference_file <- file.path(project_root, "data", "processed", "ms2_reference_spectra.csv")
default_ms2_reference_spectra <- load_optional_ms2_reference_spectra(ms2_reference_file)
parquet_root <- default_parquet_root()

metadata_index$app_file_id <- paste0("file_", seq_len(nrow(metadata_index)))
compounds_reference$app_compound_id <- paste0("compound_", seq_len(nrow(compounds_reference)))
compounds_reference$source_label <- "Liste fournie"

as_choice <- function(x) {
  x <- sort(unique(x[!is.na(x) & x != ""]))
  c("Tous" = "", x)
}

subset_by_ids <- function(x, id_col, ids) {
  ids <- ids[ids %in% x[[id_col]]]
  if (length(ids) == 0) {
    return(x[0, , drop = FALSE])
  }
  x[match(ids, x[[id_col]]), , drop = FALSE]
}

display_value <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", "-", x)
}

file_selection_label <- function(x) {
  paste0(
    display_value(x$reference_year),
    " | ",
    display_value(x$mode_dir),
    " | mois ",
    display_value(x$reference_month),
    " | duplicat ",
    display_value(x$duplicate_label),
    " | echantillon ",
    display_value(x$replicate_label),
    " | ",
    display_value(x$file_type)
  )
}

make_file_choices <- function(x) {
  if (nrow(x) == 0) {
    return(c("Aucun fichier" = ""))
  }
  labels <- file_selection_label(x)
  c("Choisir" = "", stats::setNames(x$app_file_id, labels))
}

make_compound_choices <- function(x, empty_label = "Aucun etalon") {
  if (nrow(x) == 0) {
    return(stats::setNames("", empty_label))
  }
  labels <- paste0(x$name, " | ", x$mode, " | m/z ", x$mz)
  c(stats::setNames("", empty_label), stats::setNames(x$app_compound_id, labels))
}

sample_type_label <- function(is_blank) {
  ifelse(is_blank, "Blanc", "Echantillon")
}

metadata_index$file_type <- sample_type_label(metadata_index$is_blank)

parquet_catalog <- function(root, nextcloud_files = empty_nextcloud_contents()) {
  local_files <- list_parquet_files(root)
  remote_files <- nextcloud_to_parquet_files(nextcloud_files)
  enrich_parquet_files(rbind(local_files, remote_files), metadata_index)
}

# A Catalogue row comes from the JSON index, while a usable source comes from
# the local DATA_PATH or the current Nextcloud session. Match them by their
# stable year/mode/file key and prefer a local file when both sources exist.
matching_parquet_ids_for_metadata <- function(metadata_rows, available_files) {
  if (is.null(metadata_rows) || nrow(metadata_rows) == 0 ||
      is.null(available_files) || nrow(available_files) == 0 ||
      !"parquet_relative_path" %in% names(metadata_rows) ||
      !all(c("parquet_id", "relative_path") %in% names(available_files))) {
    return(character())
  }

  metadata_keys <- metadata_relative_key(metadata_rows$parquet_relative_path)
  available_keys <- metadata_relative_key(available_files$relative_path)
  source_priority <- if ("source_type" %in% names(available_files)) {
    ifelse(available_files$source_type == "local", 0L, 1L)
  } else {
    rep(0L, nrow(available_files))
  }
  order_index <- order(source_priority, seq_len(nrow(available_files)))
  match_index <- match(metadata_keys, available_keys[order_index])
  ids <- available_files$parquet_id[order_index][match_index]
  ids <- as.character(ids[!is.na(ids) & nzchar(ids)])
  unique(ids)
}

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#215a6d",
  secondary = "#6c757d"
)

ui <- page_navbar(
  title = "Observatoire HRMS",
  id = "main_nav",
  theme = theme,
  fillable = FALSE,
  header = tags$style(HTML("
    body { letter-spacing: 0; font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
    .navbar { border-bottom: 1px solid #d9e2e6; }
    .bslib-card { border-radius: 8px; }
    .metric { min-height: 92px; }
    .metric .value { font-size: 1.75rem; font-weight: 700; line-height: 1.1; }
    .metric .label { color: #5c6970; font-size: 0.9rem; margin-top: 0.35rem; }
    .metric-detected { border-left: 6px solid #1f8f4d; }
    .metric-detected .value { color: #1f8f4d; }
    .metric-not-detected { border-left: 6px solid #c9342f; }
    .metric-not-detected .value { color: #c9342f; }
    .metric-level-1 { border-left: 6px solid #a66b00; }
    .metric-level-1 .value { color: #a66b00; }
    .metric-level-2 { border-left: 6px solid #1a6f8f; }
    .metric-level-2 .value { color: #1a6f8f; }
    .metric-level-3 { border-left: 6px solid #1f8f4d; }
    .metric-level-3 .value { color: #1f8f4d; }
    .metric-error { border-left: 6px solid #a66b00; }
    .metric-error .value { color: #a66b00; }
    .filter-panel .form-group, .filter-panel .mb-3 { margin-bottom: 0.85rem; }
    .action-stack { display: grid; gap: 0.5rem; margin-top: 0.35rem; }
    .table-actions { align-items: end; margin-bottom: 0.75rem; }
    .table-actions .btn { width: 100%; }
    .path-output pre { min-height: 44px; margin-bottom: 0; white-space: pre-wrap; }
    .parquet-metrics { margin-bottom: 1.25rem; }
    .parquet-metrics .metric { min-height: 116px; }
    .parquet-metrics .metric .value { font-size: 1.45rem; overflow-wrap: anywhere; }
    .parquet-metrics .metric .label { font-size: 0.95rem; font-weight: 600; }
    .parquet-section { clear: both; margin-top: 0.75rem; }
    .table-card { margin-top: 0.75rem; }
    .table-card .card-body { overflow: auto; }
    table.dataTable { font-size: 0.88rem; }
  ")),

  nav_panel(
    title = "Catalogue",
    icon = icon("folder-open"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 310,
        selectInput("file_year", "Annee", choices = as_choice(metadata_index$year_dir)),
        selectInput("file_mode", "Mode", choices = as_choice(metadata_index$mode_dir)),
        selectInput("file_type", "Type", choices = c("Tous" = "", "Echantillon", "Blanc")),
        selectInput("file_month", "Mois", choices = as_choice(metadata_index$reference_month)),
        selectInput("file_duplicate", "Duplicat", choices = as_choice(metadata_index$duplicate_label)),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("add_selected_files", "Ajouter selection", icon = icon("plus"), class = "btn-primary"),
          actionButton("add_filtered_files", "Ajouter tout", icon = icon("plus"), class = "btn-outline-primary")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        card(class = "metric", card_body(div(class = "value", textOutput("n_files")), div(class = "label", "Fichiers"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_samples")), div(class = "label", "Echantillons"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_blanks")), div(class = "label", "Blancs"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_modes")), div(class = "label", "Modes")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Fichiers indexes"),
        card_body(DTOutput("files_table", height = "320px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Selection de travail - fichiers"),
        card_body(
          fluidRow(
            class = "table-actions",
            column(6, selectInput("file_to_remove", "Retirer un fichier", choices = c("Aucun fichier" = ""))),
            column(3, actionButton("remove_file", "Retirer", icon = icon("trash"), class = "btn-outline-secondary")),
            column(3, actionButton("clear_files", "Tout vider", icon = icon("trash"), class = "btn-outline-danger"))
          ),
          DTOutput("selected_files_table", height = "260px")
        )
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Repartition"),
        card_body(plotOutput("files_plot", height = "260px"))
      )
    )
  ),

  nav_panel(
    title = "Etalons internes",
    icon = icon("flask"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput("compound_mode", "Mode", choices = as_choice(compounds_reference$mode)),
        textInput("compound_search", "Recherche", value = ""),
        sliderInput(
          "compound_rt",
          "RT min",
          min = floor(min(compounds_reference$rt, na.rm = TRUE)),
          max = ceiling(max(compounds_reference$rt, na.rm = TRUE)),
          value = c(
            floor(min(compounds_reference$rt, na.rm = TRUE)),
            ceiling(max(compounds_reference$rt, na.rm = TRUE))
          ),
          step = 0.1
        ),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("add_selected_compounds", "Ajouter selection", icon = icon("plus"), class = "btn-primary"),
          actionButton("add_filtered_compounds", "Ajouter tout", icon = icon("plus"), class = "btn-outline-primary")
        ),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("open_manual_compound_dialog", "Ajouter manuellement", icon = icon("plus"), class = "btn-outline-primary")
        ),
        fileInput(
          "import_compounds_csv",
          "Importer un CSV",
          accept = c(".csv", "text/csv", "text/plain"),
          width = "100%"
        ),
        checkboxInput("import_compounds_auto_select", "Ajouter aussi a la selection", value = TRUE),
        div(
          class = "action-stack",
          downloadButton("download_compounds_template", "Modele CSV", icon = icon("download"), class = "btn-outline-secondary"),
          downloadButton("download_compounds_catalog", "Exporter la liste", icon = icon("download"), class = "btn-outline-secondary"),
          actionButton("remove_selected_custom_compounds", "Supprimer ajouts selectionnes", icon = icon("trash"), class = "btn-outline-danger"),
          actionButton("reset_custom_compounds", "Revenir a la liste fournie", icon = icon("rotate-left"), class = "btn-outline-danger")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        card(class = "metric", card_body(div(class = "value", textOutput("n_compounds")), div(class = "label", "Etalons internes"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_compounds_pos")), div(class = "label", "Mode pos"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_compounds_neg")), div(class = "label", "Mode neg"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_custom_compounds")), div(class = "label", "Ajouts session")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Liste des etalons internes"),
        card_body(DTOutput("compounds_table", height = "320px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Selection de travail - etalons internes"),
        card_body(
          fluidRow(
            class = "table-actions",
            column(6, selectInput("compound_to_remove", "Retirer un etalon", choices = c("Aucun etalon" = ""))),
            column(3, actionButton("remove_compound", "Retirer", icon = icon("trash"), class = "btn-outline-secondary")),
            column(3, actionButton("clear_compounds", "Tout vider", icon = icon("trash"), class = "btn-outline-danger"))
          ),
          DTOutput("selected_compounds_table", height = "260px")
        )
      )
    )
  ),

  nav_panel(
    title = "Suspects",
    icon = icon("bullseye"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput("suspect_mode", "Mode", choices = c("Tous" = "", "Positif" = "pos", "Negatif" = "neg")),
        textInput("suspect_search", "Recherche", value = ""),
        sliderInput("suspect_rt", "RT min", min = 0, max = 20, value = c(0, 20), step = 0.1),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("add_selected_suspects", "Ajouter selection", icon = icon("plus"), class = "btn-primary"),
          actionButton("add_filtered_suspects", "Ajouter tout", icon = icon("plus"), class = "btn-outline-primary"),
          actionButton("create_test_suspects_from_internal_standards", "Creer tests depuis etalons selectionnes", icon = icon("copy"), class = "btn-outline-secondary")
        ),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("open_manual_suspect_dialog", "Ajouter manuellement", icon = icon("plus"), class = "btn-outline-primary")
        ),
        fileInput(
          "import_suspects_csv",
          "Importer un CSV",
          accept = c(".csv", "text/csv", "text/plain"),
          width = "100%"
        ),
        checkboxInput("import_suspects_auto_select", "Ajouter aussi a la selection", value = TRUE),
        div(
          class = "action-stack",
          downloadButton("download_suspects_template", "Modele CSV", icon = icon("download"), class = "btn-outline-secondary"),
          downloadButton("download_suspects_catalog", "Exporter la liste", icon = icon("download"), class = "btn-outline-secondary"),
          actionButton("remove_selected_custom_suspects", "Supprimer ajouts selectionnes", icon = icon("trash"), class = "btn-outline-danger"),
          actionButton("reset_custom_suspects", "Effacer les ajouts session", icon = icon("rotate-left"), class = "btn-outline-danger")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        card(class = "metric", card_body(div(class = "value", textOutput("n_suspects")), div(class = "label", "Suspects"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_suspects_pos")), div(class = "label", "Mode pos"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_suspects_neg")), div(class = "label", "Mode neg"))),
        card(class = "metric", card_body(div(class = "value", textOutput("n_custom_suspects")), div(class = "label", "Ajouts session")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Liste des suspects"),
        card_body(DTOutput("suspects_table", height = "320px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Selection de travail - suspects"),
        card_body(
          fluidRow(
            class = "table-actions",
            column(6, selectInput("suspect_to_remove", "Retirer un suspect", choices = c("Aucun suspect" = ""))),
            column(3, actionButton("remove_suspect", "Retirer", icon = icon("trash"), class = "btn-outline-secondary")),
            column(3, actionButton("clear_suspects", "Tout vider", icon = icon("trash"), class = "btn-outline-danger"))
          ),
          DTOutput("selected_suspects_table", height = "260px")
        )
      )
    )
  ),

  nav_panel(
    title = "Nextcloud",
    icon = icon("cloud"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput(
          "nextcloud_access_mode",
          "Mode d'acces",
          choices = c("Lien de partage" = "public", "Compte Nextcloud" = "account"),
          selected = default_nextcloud_access_mode()
        ),
        textInput(
          "nextcloud_base_url",
          "URL Nextcloud, lien ou WebDAV",
          value = default_nextcloud_url(),
          placeholder = "https://nextcloud.example.org/apps/files/files/..."
        ),
        conditionalPanel(
          condition = "input.nextcloud_access_mode === 'public'",
          passwordInput(
            "nextcloud_share_token",
            "Jeton de partage",
            value = default_nextcloud_share_token()
          )
        ),
        conditionalPanel(
          condition = "input.nextcloud_access_mode === 'account'",
          textInput(
            "nextcloud_username",
            "Identifiant Nextcloud",
            value = default_nextcloud_username()
          ),
          passwordInput("nextcloud_app_password", "Mot de passe d'application")
        ),
        div(
          class = "action-stack",
          actionButton("nextcloud_refresh", "Lire le dossier", icon = icon("rotate"), class = "btn-primary"),
          actionButton("nextcloud_up", "Dossier parent", icon = icon("arrow-up"), class = "btn-outline-secondary")
        ),
        tags$hr(),
        div(class = "path-output", verbatimTextOutput("nextcloud_status")),
        div(class = "path-output", verbatimTextOutput("nextcloud_current_path")),
        tags$hr(),
        selectInput("nextcloud_file_to_remove", "Retirer un fichier distant", choices = c("Aucun fichier" = "")),
        div(
          class = "action-stack",
          actionButton("nextcloud_remove_file", "Retirer", icon = icon("trash"), class = "btn-outline-secondary"),
          actionButton("nextcloud_clear_files", "Tout vider", icon = icon("trash"), class = "btn-outline-danger")
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        card(class = "metric", card_body(div(class = "value", textOutput("nextcloud_items_count")), div(class = "label", "Elements du dossier"))),
        card(class = "metric", card_body(div(class = "value", textOutput("nextcloud_selected_count")), div(class = "label", "Parquet distants"))),
        card(class = "metric", card_body(div(class = "value", textOutput("nextcloud_catalog_count")), div(class = "label", "Ajoutes au catalogue")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Contenu Nextcloud"),
        card_body(DTOutput("nextcloud_contents_table", height = "560px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Fichiers distants ajoutes"),
        card_body(DTOutput("nextcloud_selected_table", height = "320px"))
      )
    )
  ),

  nav_panel(
    title = "Parquet",
    icon = icon("database"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 360,
        div(class = "path-output", verbatimTextOutput("parquet_data_path")),
        actionButton("refresh_parquet_files", "Rafraichir", icon = icon("rotate"), class = "btn-outline-secondary"),
        selectInput("parquet_file_id", "Fichier Parquet", choices = c("Aucun fichier Parquet" = "")),
        selectInput(
          "parquet_mode_override",
          "Mode du fichier",
          choices = c("Automatique (JSON ou chemin)" = "auto", "Positif" = "pos", "Negatif" = "neg"),
          selected = "auto"
        ),
        div(class = "path-output", verbatimTextOutput("parquet_file_context")),
        actionButton("inspect_parquet", "Lire les infos", icon = icon("eye"), class = "btn-primary"),
        tags$hr(),
        selectInput("chrom_mslevel", "MS level", choices = c("1", "2"), selected = "1"),
        actionButton("compute_tic_bpi", "Calculer TIC/BPI", icon = icon("chart-line"), class = "btn-primary"),
        tags$hr(),
        selectInput("eic_compound_id", "Etalon ou suspect (facultatif)", choices = make_compound_choices(compounds_reference, empty_label = "Aucune molecule (recherche libre)")),
        div(class = "path-output", verbatimTextOutput("eic_compound_details")),
        numericInput("quick_eic_target_mz", "m/z recherche rapide", value = 235.1477, min = 0, step = 0.0001),
        actionButton("run_quick_eic", "Rechercher m/z et afficher EIC (MS1)", icon = icon("bolt"), class = "btn-primary"),
        div(class = "path-output", verbatimTextOutput("quick_eic_status")),
        tags$hr(),
        numericInput("eic_target_mz", "m/z cible", value = 235.1477, min = 0, step = 0.0001),
        numericInput("eic_mz_tolerance", "Tolerance m/z", value = 0.01, min = 0.0001, step = 0.001),
        numericInput("eic_expected_rt", "RT attendu", value = 11.7, min = 0, step = 0.01),
        numericInput("eic_rt_tolerance", "Tolerance RT", value = 0.5, min = 0, step = 0.05),
        numericInput("eic_min_intensity", "Intensite min EIC", value = 0, min = 0, step = 100),
        actionButton("compute_eic", "Calculer EIC", icon = icon("wave-square"), class = "btn-outline-primary"),
        tags$hr(),
        numericInput("ms2_bin_width", "Regroupement m/z MS2", value = 0.01, min = 0.001, step = 0.001),
        numericInput("ms2_min_intensity", "Intensite min MS2", value = 0, min = 0, step = 100),
        numericInput("ms2_top_n", "Pics MS2 affiches", value = 150, min = 10, max = 1000, step = 10),
        actionButton("compute_ms2_spectrum", "Afficher spectre MS2", icon = icon("chart-bar"), class = "btn-outline-primary"),
        tags$hr(),
        selectInput("ms2_reference_id", "Spectre MS2 de reference", choices = c("Aucun spectre de reference importe" = "")),
        fileInput(
          "import_ms2_reference_csv",
          "Importer spectres MS2 CSV",
          accept = c(".csv", "text/csv", "text/plain"),
          width = "100%"
        ),
        div(class = "path-output", textOutput("ms2_reference_library_status")),
        div(
          class = "action-stack",
          downloadButton("download_ms2_reference_template", "Modele CSV MS2", icon = icon("download"), class = "btn-outline-secondary"),
          actionButton("reset_ms2_reference_library", "Revenir aux references locales", icon = icon("rotate-left"), class = "btn-outline-secondary")
        ),
        numericInput("ms2_match_mz_tolerance", "Tolerance fragments m/z", value = 0.01, min = 0.0001, step = 0.001),
        numericInput("ms2_min_matched_fragments", "Fragments concordants minimum", value = 3, min = 1, max = 100, step = 1),
        numericInput("ms2_min_cosine_similarity", "Score cosinus minimum", value = 0.7, min = 0, max = 1, step = 0.05),
        actionButton(
          "compare_ms2_spectrum",
          "Comparer au spectre de reference",
          icon = icon("magnifying-glass-chart"),
          class = "btn-outline-primary",
          title = "Comparer les fragments observes au spectre de reference selectionne"
        ),
        tags$hr(),
        checkboxInput("screen_selected_compounds_only", "Selection de screening seulement", value = FALSE),
        checkboxInput("screen_require_rt_match", "Exiger RT coherent", value = TRUE),
        numericInput("screening_min_intensity", "Intensite min screening", value = 1000, min = 0, step = 100),
        checkboxInput("screen_use_dt", "Afficher controle DT (exploratoire)", value = FALSE),
        numericInput("screen_dt_tolerance_pct", "Tolerance DT (%)", value = 10, min = 0, step = 1),
        checkboxInput("screen_use_ccs", "Verifier la mobilite CCS/DT", value = TRUE),
        numericInput("screen_ccs_tolerance_pct", "Tolerance CCS (%)", value = 10, min = 0, step = 1),
        actionButton("run_current_file_screening", "Screening fichier courant", icon = icon("magnifying-glass-chart"), class = "btn-primary")
      ),
      fillable = FALSE,
      fill = FALSE,
      div(
        class = "parquet-metrics",
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          fillable = FALSE,
          card(fill = FALSE, class = "metric", card_body(div(class = "value", textOutput("n_parquet_files")), div(class = "label", "Fichiers detectes"))),
          card(fill = FALSE, class = "metric", card_body(div(class = "value", textOutput("parquet_selected_size")), div(class = "label", "Taille selection"))),
          card(fill = FALSE, class = "metric", card_body(div(class = "value", textOutput("parquet_loaded_rows")), div(class = "label", "Lignes lues"))),
          card(fill = FALSE, class = "metric", card_body(div(class = "value", textOutput("parquet_loaded_columns")), div(class = "label", "Colonnes")))
        )
      ),
      card(
        full_screen = TRUE,
        fill = FALSE,
        class = "parquet-section table-card",
        card_header("Fichiers Parquet disponibles"),
        card_body(DTOutput("parquet_files_table", height = "560px"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        fillable = FALSE,
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("TIC"),
          card_body(
            plotly::plotlyOutput("tic_plot", height = "460px"),
            DTOutput("tic_summary_table", height = "210px")
          )
        ),
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("BPI"),
          card_body(
            plotly::plotlyOutput("bpi_plot", height = "460px"),
            DTOutput("bpi_summary_table", height = "210px")
          )
        )
      ),
      layout_columns(
        col_widths = c(12),
        fill = FALSE,
        fillable = FALSE,
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("EIC"),
          card_body(
            plotly::plotlyOutput("eic_plot", height = "620px"),
            DTOutput("eic_table", height = "360px")
          )
        ),
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("Resume EIC"),
          card_body(DTOutput("eic_summary_table", height = "260px"))
        ),
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("Spectre MS2 brut (fenetre RT)"),
          card_body(
            plotly::plotlyOutput("ms2_spectrum_plot", height = "520px"),
            DTOutput("ms2_spectrum_table", height = "320px")
          )
        ),
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("Comparaison MS2 au spectre de reference (exploratoire)"),
          card_body(
            DTOutput("ms2_comparison_summary_table", height = "150px"),
            DTOutput("ms2_comparison_table", height = "360px")
          )
        )
      ),
      layout_column_wrap(
        width = 1 / 5,
        fill = FALSE,
        fillable = FALSE,
        card(fill = FALSE, class = "metric metric-not-detected", card_body(div(class = "value", textOutput("screening_not_detected_count")), div(class = "label", "Preuve 0"))),
        card(fill = FALSE, class = "metric metric-level-1", card_body(div(class = "value", textOutput("screening_level_one_count")), div(class = "label", "Preuve 1 - m/z"))),
        card(fill = FALSE, class = "metric metric-level-2", card_body(div(class = "value", textOutput("screening_level_two_count")), div(class = "label", "Preuve 2 - m/z + RT"))),
          card(fill = FALSE, class = "metric metric-level-3", card_body(div(class = "value", textOutput("screening_level_three_count")), div(class = "label", "Preuve 3 - mobilite"))),
        card(fill = FALSE, class = "metric metric-detected", card_body(div(class = "value", textOutput("screening_detected_count")), div(class = "label", "Detected retenus")))
      ),
      card(
        full_screen = TRUE,
        fill = FALSE,
        class = "table-card",
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Resultats screening"),
            downloadButton("download_screening_results", "Exporter CSV", icon = icon("download"), class = "btn-outline-secondary btn-sm")
          )
        ),
        card_body(DTOutput("screening_results_table", height = "520px"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        fillable = FALSE,
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("Schema"),
          card_body(DTOutput("parquet_schema_table", height = "300px"))
        ),
        card(
          full_screen = TRUE,
          fill = FALSE,
          class = "table-card",
          card_header("Niveaux MS"),
          card_body(DTOutput("parquet_ms_table", height = "300px"))
        )
      ),
      card(
        full_screen = TRUE,
        fill = FALSE,
        class = "table-card",
        card_header("Resume numerique"),
        card_body(DTOutput("parquet_summary_table", height = "300px"))
      ),
      card(
        full_screen = TRUE,
        fill = FALSE,
        class = "table-card",
        card_header("Apercu"),
        card_body(DTOutput("parquet_preview_table", height = "320px"))
      )
    )
  ),

  nav_panel(
    title = "Plan screening",
    icon = icon("magnifying-glass-chart"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput("screen_mode", "Mode", choices = as_choice(compounds_reference$mode)),
        checkboxInput("screen_include_blanks", "Inclure les blancs", value = TRUE),
        numericInput("screen_mz_tol", "Tolerance m/z", value = 0.01, min = 0.001, step = 0.001),
        numericInput("screen_batch_rt_tol", "Tolerance RT", value = 0.5, min = 0, step = 0.05),
        numericInput("screen_min_intensity", "Intensite min", value = 1000, min = 0, step = 100),
        selectInput("screen_batch_mslevel", "Niveau MS", choices = c("MS1" = "1", "MS2" = "2"), selected = "1"),
        div(
          title = "Necessaire uniquement pour la normalisation par intensite totale du niveau MS.",
          checkboxInput("screen_batch_compute_total_intensity", "Calculer l'intensite totale MS", value = FALSE)
        ),
        checkboxInput("screen_batch_require_rt", "Exiger RT coherent", value = TRUE),
        checkboxInput("screen_batch_use_dt", "Afficher controle DT (exploratoire)", value = FALSE),
        numericInput("screen_batch_dt_tolerance_pct", "Tolerance DT (%)", value = 10, min = 0, step = 1),
        checkboxInput("screen_batch_use_ccs", "Verifier la mobilite CCS/DT", value = TRUE),
        numericInput("screen_batch_ccs_tolerance_pct", "Tolerance CCS (%)", value = 10, min = 0, step = 1),
        tags$hr(),
        selectInput(
          "screening_source_origin",
          "Origine des fichiers",
          choices = c("Toutes" = "", "Local" = "local", "Nextcloud" = "nextcloud")
        ),
        selectInput(
          "screening_source_mode",
          "Mode des fichiers",
          choices = c("Tous" = "", "Positif" = "pos", "Negatif" = "neg")
        ),
        textInput("screening_source_search", "Rechercher un chemin"),
        div(
          class = "action-stack",
          actionButton("add_screening_sources", "Ajouter la selection", icon = icon("plus"), class = "btn-primary"),
          actionButton("add_filtered_screening_sources", "Ajouter les filtres", icon = icon("plus"), class = "btn-outline-secondary")
        ),
        tags$hr(),
        selectInput("screening_source_to_remove", "Retirer du lot", choices = c("Aucun fichier" = "")),
        div(
          class = "action-stack",
          actionButton("remove_screening_source", "Retirer", icon = icon("trash"), class = "btn-outline-secondary"),
          actionButton("clear_screening_sources", "Tout vider", icon = icon("trash"), class = "btn-outline-danger")
        ),
        tags$hr(),
        div(
          class = "action-stack",
          actionButton("open_batch_screening_confirmation", "Lancer le screening du lot", icon = icon("play"), class = "btn-primary"),
          actionButton("clear_batch_screening_results", "Effacer les resultats", icon = icon("trash"), class = "btn-outline-secondary")
        ),
        tags$hr(),
        fileInput(
          "import_batch_screening_csv",
          "Importer un export de lot",
          accept = c(".csv", "text/csv", "text/plain"),
          width = "100%"
        )
      ),
      layout_column_wrap(
        width = 1 / 6,
        card(class = "metric", card_body(div(class = "value", textOutput("plan_source_selection_count")), div(class = "label", "Parquet du lot"))),
        card(class = "metric", card_body(div(class = "value", textOutput("plan_files")), div(class = "label", "Fichiers retenus"))),
        card(class = "metric", card_body(div(class = "value", textOutput("plan_internal_standards")), div(class = "label", "Etalons internes"))),
        card(class = "metric", card_body(div(class = "value", textOutput("plan_suspects")), div(class = "label", "Suspects"))),
        card(class = "metric", card_body(div(class = "value", textOutput("plan_queries")), div(class = "label", "Requetes"))),
        card(class = "metric", card_body(div(class = "value", textOutput("plan_modes")), div(class = "label", "Modes")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Parquet disponibles pour le lot"),
        card_body(DTOutput("screening_source_catalog_table", height = "420px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Selection de fichiers pour le lot"),
        card_body(DTOutput("selected_screening_sources_table", height = "300px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Plan de calcul fichier x molecule"),
        card_body(DTOutput("screening_plan_table", height = "340px"))
      ),
      layout_column_wrap(
        width = 1 / 5,
        card(class = "metric", card_body(div(class = "value", textOutput("batch_result_files")), div(class = "label", "Fichiers traites"))),
        card(class = "metric", card_body(div(class = "value", textOutput("batch_result_rows")), div(class = "label", "Resultats"))),
        card(class = "metric metric-not-detected", card_body(div(class = "value", textOutput("batch_level_zero_count")), div(class = "label", "Preuve 0"))),
        card(class = "metric metric-level-1", card_body(div(class = "value", textOutput("batch_level_one_count")), div(class = "label", "Preuve 1 - m/z"))),
        card(class = "metric metric-level-2", card_body(div(class = "value", textOutput("batch_level_two_count")), div(class = "label", "Preuve 2 - m/z + RT"))),
        card(class = "metric metric-level-3", card_body(div(class = "value", textOutput("batch_level_three_count")), div(class = "label", "Preuve 3 - mobilite"))),
        card(class = "metric metric-detected", card_body(div(class = "value", textOutput("batch_detected_count")), div(class = "label", "Detected retenus"))),
        card(class = "metric metric-error", card_body(div(class = "value", textOutput("batch_error_count")), div(class = "label", "Erreurs")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Resultats du screening de lot"),
            div(
              class = "d-flex flex-wrap gap-2 align-items-center",
              textOutput("batch_screening_status"),
              actionButton(
                "prepare_batch_result_eic",
                "Preparer EIC",
                icon = icon("wave-square"),
                class = "btn-outline-primary btn-sm",
                title = "Selectionner une ligne, puis ouvrir le fichier et ses parametres EIC dans l'onglet Parquet"
              ),
              actionButton(
                "follow_batch_result_molecule",
                "Suivre molecule",
                icon = icon("chart-line"),
                class = "btn-outline-secondary btn-sm",
                title = "Selectionner une ligne, puis ouvrir le suivi temporel de cette molecule"
              ),
              downloadButton("download_batch_screening_results", "Exporter CSV", icon = icon("download"), class = "btn-outline-secondary btn-sm")
            )
          )
        ),
        card_body(DTOutput("batch_screening_results_table", height = "560px"))
      )
    )
  ),

  nav_panel(
    title = "Suivi molecules",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput("monitoring_compound_id", "Molecule suivie", choices = c("Aucun resultat de screening" = "")),
        selectInput("monitoring_mode", "Mode", choices = c("Tous" = "", "Positif" = "pos", "Negatif" = "neg")),
        selectInput("monitoring_metric", "Mesure", choices = monitoring_metric_choices, selected = "rt_area_sum"),
        selectInput(
          "monitoring_treatment",
          "Traitement",
          choices = c("Signal brut" = "raw", "Signal corrige du blanc" = "blank_corrected"),
          selected = "raw"
        ),
        selectInput(
          "monitoring_normalization",
          "Normalisation",
          choices = c(
            "Aucune" = "none",
            "Par etalon interne choisi" = "internal_standard",
            "Par etalon interne le plus proche en RT" = "closest_internal_standard",
            "Par intensite totale du niveau MS" = "total_intensity"
          ),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.monitoring_normalization === 'internal_standard'",
          selectInput(
            "monitoring_normalization_compound_id",
            "Etalon de reference",
            choices = c("Aucun etalon de reference disponible" = "")
          )
        ),
        conditionalPanel(
          condition = "input.monitoring_normalization === 'closest_internal_standard'",
          div(class = "path-output", textOutput("monitoring_closest_reference_details"))
        ),
        tags$hr(),
        selectInput(
          "monitoring_injection_aggregation",
          "Agregation injections",
          choices = monitoring_aggregation_choices,
          selected = "none"
        ),
        checkboxInput("monitoring_injection_exclude_outliers", "Exclure injections atypiques (MAD)", value = FALSE),
        selectInput(
          "monitoring_duplicate_aggregation",
          "Agregation duplicats",
          choices = monitoring_aggregation_choices,
          selected = "none"
        ),
        checkboxInput("monitoring_duplicate_exclude_outliers", "Exclure duplicats atypiques (MAD)", value = FALSE),
        conditionalPanel(
          condition = "input.monitoring_injection_exclude_outliers || input.monitoring_duplicate_exclude_outliers",
          numericInput("monitoring_outlier_threshold", "Seuil atypique MAD", value = 3.5, min = 0.1, step = 0.1)
        ),
        selectInput(
          "monitoring_scale",
          "Echelle",
          choices = c("Lineaire" = "linear", "Logarithmique" = "log"),
          selected = "linear"
        ),
        selectInput(
          "monitoring_status",
          "Statut",
          choices = c("Tous" = "", "Detected" = "Detected", "Not Detected" = "Not Detected", "Erreur" = "Error")
        ),
        checkboxInput("monitoring_include_blanks", "Afficher les blancs", value = TRUE),
        tags$hr(),
        selectInput("monitoring_blank_file_id", "Blanc de reference", choices = c("Aucun blanc disponible" = "")),
        div(
          class = "action-stack",
          downloadButton("download_monitoring_results", "Exporter la vue", icon = icon("download"), class = "btn-outline-secondary"),
          downloadButton("download_monitoring_raw_results", "Exporter valeurs brutes", icon = icon("download"), class = "btn-outline-secondary")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        card(class = "metric", card_body(div(class = "value", textOutput("monitoring_files_count")), div(class = "label", "Valeurs affichees"))),
        card(class = "metric metric-detected", card_body(div(class = "value", textOutput("monitoring_detected_count")), div(class = "label", "Detected affiches"))),
        card(class = "metric", card_body(div(class = "value", textOutput("monitoring_signal_median")), div(class = "label", "Mediane signal"))),
        card(class = "metric", card_body(div(class = "value", textOutput("monitoring_blank_signal")), div(class = "label", "Signal blanc choisi")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Evolution du signal"),
        card_body(plotOutput("monitoring_plot", height = "480px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Resultats apres agregation"),
        card_body(DTOutput("monitoring_results_table", height = "480px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Valeurs individuelles avant agregation"),
        card_body(DTOutput("monitoring_raw_results_table", height = "420px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Variabilite brute par duplicat"),
        card_body(DTOutput("monitoring_duplicates_table", height = "360px"))
      )
    )
  ),

  nav_panel(
    title = "Controle",
    icon = icon("clipboard-check"),
    layout_sidebar(
      sidebar = sidebar(
        class = "filter-panel",
        width = 330,
        selectInput(
          "control_file_id",
          "Fichier Parquet a analyser",
          choices = c("Choisir" = "")
        ),
        div(
          class = "action-stack",
          actionButton("run_control_file_checks", "Analyser le fichier", icon = icon("magnifying-glass"), class = "btn-primary")
        )
      ),
      layout_column_wrap(
        width = 1 / 4,
        card(class = "metric", card_body(div(class = "value", textOutput("control_files_count")), div(class = "label", "Parquet catalogue"))),
        card(class = "metric", card_body(div(class = "value", textOutput("control_json_count")), div(class = "label", "JSON associes"))),
        card(class = "metric metric-detected", card_body(div(class = "value", textOutput("control_ready_count")), div(class = "label", "Prets screening"))),
        card(class = "metric metric-level-1", card_body(div(class = "value", textOutput("control_review_count")), div(class = "label", "A verifier")))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Verifications globales"),
        card_body(DTOutput("checks_table", height = "360px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Diagnostic du catalogue"),
        card_body(DTOutput("control_catalog_table", height = "480px"))
      ),
      card(
        full_screen = TRUE,
        class = "table-card",
        card_header("Diagnostic du fichier selectionne"),
        card_body(DTOutput("control_file_checks_table", height = "420px"))
      )
    )
  )
)

server <- function(input, output, session) {
  selected_file_ids <- reactiveVal(character())
  selected_compound_ids <- reactiveVal(character())
  compound_catalog <- reactiveVal(compounds_reference)
  custom_compound_counter <- reactiveVal(0L)
  selected_screening_source_ids <- reactiveVal(character())
  nextcloud_contents <- reactiveVal(empty_nextcloud_contents())
  nextcloud_current_path <- reactiveVal("")
  nextcloud_selected_files <- reactiveVal(empty_nextcloud_contents())
  nextcloud_connection_status <- reactiveVal("Non connecte")
  nextcloud_session_config <- reactiveVal(NULL)
  # The remote selection is empty at session start. Reading the reactive value
  # here would be outside a Shiny reactive context and disconnect the client.
  parquet_files <- reactiveVal(parquet_catalog(parquet_root))
  parquet_info <- reactiveVal(NULL)
  tic_data <- reactiveVal(NULL)
  bpi_data <- reactiveVal(NULL)
  eic_data <- reactiveVal(NULL)
  eic_summary <- reactiveVal(NULL)
  eic_context <- reactiveVal(NULL)
  quick_eic_result <- reactiveVal(NULL)
  ms2_spectrum_data <- reactiveVal(NULL)
  ms2_spectrum_context <- reactiveVal(NULL)
  ms2_reference_catalog <- reactiveVal(default_ms2_reference_spectra)
  ms2_reference_comparison <- reactiveVal(NULL)
  screening_results <- reactiveVal(NULL)
  control_parquet_info <- reactiveVal(NULL)
  control_parquet_error <- reactiveVal(NULL)
  batch_screening_results <- reactiveVal(NULL)
  batch_screening_status <- reactiveVal("Aucun lot lance")

  selected_batch_screening_result <- reactive({
    results <- batch_screening_results()
    rows <- input$batch_screening_results_table_rows_selected %||% integer()
    if (is.null(results) || nrow(results) == 0 || length(rows) != 1) {
      return(data.frame())
    }
    row <- suppressWarnings(as.integer(rows[[1]]))
    if (!is.finite(row) || row < 1 || row > nrow(results)) {
      return(data.frame())
    }
    results[row, , drop = FALSE]
  })

  monitoring_results_with_compound_metadata <- reactive({
    results <- batch_screening_results()
    if (is.null(results) || nrow(results) == 0) {
      return(data.frame())
    }
    add_monitoring_compound_metadata(results, compound_catalog())
  })

  monitoring_candidates <- reactive({
    results <- monitoring_results_with_compound_metadata()
    required <- c("compound_id", "mode")
    if (is.null(results) || nrow(results) == 0 || !all(required %in% names(results))) {
      return(data.frame())
    }
    x <- results
    compound_id <- input$monitoring_compound_id %||% ""
    mode <- input$monitoring_mode %||% ""
    if (nzchar(compound_id)) x <- x[x$compound_id == compound_id, , drop = FALSE]
    if (nzchar(mode)) x <- x[x$mode == mode, , drop = FALSE]
    x
  })

  monitoring_blank_rows <- reactive({
    x <- monitoring_candidates()
    if (nrow(x) == 0 || !"file_type" %in% names(x)) {
      return(data.frame())
    }
    x[!is.na(x$file_type) & x$file_type == "Blanc", , drop = FALSE]
  })

  monitoring_selected_blank <- reactive({
    x <- monitoring_blank_rows()
    blank_id <- input$monitoring_blank_file_id %||% ""
    if (nrow(x) == 0 || !nzchar(blank_id)) {
      return(x[0, , drop = FALSE])
    }
    x[x$parquet_id == blank_id, , drop = FALSE]
  })

  monitoring_normalization_reference_candidates <- reactive({
    results <- monitoring_results_with_compound_metadata()
    required <- c("compound_id", "compound_name", "mode", "compound_type")
    if (is.null(results) || nrow(results) == 0 || !all(required %in% names(results))) {
      return(data.frame())
    }

    x <- results
    x$mode <- tolower(trimws(as.character(x$mode)))
    x$compound_type <- tolower(trimws(as.character(x$compound_type)))
    target_id <- input$monitoring_compound_id %||% ""
    selected_mode <- input$monitoring_mode %||% ""
    if (nzchar(target_id)) {
      target_modes <- unique(as.character(x$mode[x$compound_id == target_id]))
      target_modes <- target_modes[vapply(target_modes, valid_mode, logical(1))]
      if (length(target_modes) == 1) {
        selected_mode <- target_modes[[1]]
      }
      x <- x[x$compound_id != target_id, , drop = FALSE]
    }
    if (nzchar(selected_mode)) {
      x <- x[x$mode == selected_mode, , drop = FALSE]
    }
    x[x$compound_type == "internal_standard", , drop = FALSE]
  })

  monitoring_reference_rows_by_id <- function(reference_id) {
    x <- monitoring_normalization_reference_candidates()
    reference_id <- trimws(as.character(reference_id[[1]]))
    if (nrow(x) == 0 || !nzchar(reference_id)) {
      return(x[0, , drop = FALSE])
    }
    x[x$compound_id == reference_id, , drop = FALSE]
  }

  monitoring_normalization_reference <- reactive({
    reference_id <- input$monitoring_normalization_compound_id %||% ""
    monitoring_reference_rows_by_id(reference_id)
  })

  monitoring_closest_normalization_reference <- reactive({
    closest_internal_standard_by_rt(
      monitoring_results_with_compound_metadata(),
      target_id = input$monitoring_compound_id %||% "",
      target_mode = input$monitoring_mode %||% ""
    )
  })

  monitoring_view_results <- reactive({
    x <- monitoring_candidates()
    if (nrow(x) == 0) {
      return(x)
    }

    status <- input$monitoring_status %||% ""
    if (nzchar(status) && "status" %in% names(x)) {
      x <- x[x$status == status, , drop = FALSE]
    }
    if (!isTRUE(input$monitoring_include_blanks) && "file_type" %in% names(x)) {
      x <- x[is.na(x$file_type) | x$file_type != "Blanc", , drop = FALSE]
    }

    selected_blank <- monitoring_selected_blank()
    if (nrow(selected_blank) == 1 && !selected_blank$parquet_id[[1]] %in% x$parquet_id) {
      x <- rbind(x, selected_blank)
    }

    metric <- input$monitoring_metric %||% "rt_area_sum"
    if (!metric %in% names(x)) {
      x$signal_brut <- NA_real_
    } else {
      x$signal_brut <- suppressWarnings(as.numeric(x[[metric]]))
    }
    selected_blank <- monitoring_selected_blank()
    blank_signal <- if (nrow(selected_blank) == 1 && metric %in% names(selected_blank)) {
      suppressWarnings(as.numeric(selected_blank[[metric]][[1]]))
    } else {
      NA_real_
    }
    x$signal_blanc_selectionne <- blank_signal
    x$signal_corrige_blanc <- if (is.finite(blank_signal)) {
      x$signal_brut - blank_signal
    } else {
      NA_real_
    }
    treatment <- input$monitoring_treatment %||% "raw"
    x$monitoring_treatment <- treatment
    x$signal_traite <- if (identical(treatment, "blank_corrected")) {
      x$signal_corrige_blanc
    } else {
      x$signal_brut
    }

    normalization <- input$monitoring_normalization %||% "none"
    x$normalisation_method <- normalization
    x$normalisation_label <- monitoring_normalization_label(normalization)
    x$normalisation_reference <- NA_character_
    x$normalisation_reference_id <- NA_character_
    x$normalisation_reference_expected_rt <- NA_real_
    x$normalisation_reference_rt_distance <- NA_real_
    x$normalisation_divisor <- NA_real_
    x$normalisation_status <- "Non appliquee"
    x$signal_normalise <- NA_real_

    if (monitoring_uses_internal_standard(normalization)) {
      is_closest_reference <- identical(normalization, "closest_internal_standard")
      closest_reference <- monitoring_closest_normalization_reference()
      reference_id <- if (is_closest_reference) {
        if (nrow(closest_reference) == 1 &&
          identical(closest_reference$selection_status[[1]], "Etalon interne le plus proche selectionne")) {
          as.character(closest_reference$compound_id[[1]])
        } else {
          ""
        }
      } else {
        input$monitoring_normalization_compound_id %||% ""
      }
      reference_id <- trimws(as.character(reference_id[[1]]))
      reference <- if (is_closest_reference) {
        monitoring_reference_rows_by_id(reference_id)
      } else {
        monitoring_normalization_reference()
      }
      if (!nzchar(reference_id)) {
        x$normalisation_status <- if (is_closest_reference && nrow(closest_reference) == 1) {
          as.character(closest_reference$selection_status[[1]])
        } else {
          "Etalon non selectionne"
        }
      } else if (nrow(reference) == 0) {
        x$normalisation_status <- "Etalon absent du lot"
      } else {
        reference <- reference[!duplicated(reference$parquet_id), , drop = FALSE]
        reference_name <- unique(as.character(reference$compound_name))
        reference_name <- reference_name[!is.na(reference_name) & nzchar(reference_name)]
        x$normalisation_reference <- if (length(reference_name) > 0) reference_name[[1]] else reference_id
        x$normalisation_reference_id <- reference_id

        reference_rt <- unique(suppressWarnings(as.numeric(reference$expected_rt)))
        reference_rt <- reference_rt[is.finite(reference_rt)]
        target_rt <- unique(suppressWarnings(as.numeric(x$expected_rt)))
        target_rt <- target_rt[is.finite(target_rt)]
        if (is_closest_reference && nrow(closest_reference) == 1) {
          x$normalisation_reference_expected_rt <- closest_reference$reference_expected_rt[[1]]
          x$normalisation_reference_rt_distance <- closest_reference$rt_distance[[1]]
        } else if (length(reference_rt) > 0) {
          x$normalisation_reference_expected_rt <- reference_rt[[1]]
          if (length(target_rt) > 0) {
            x$normalisation_reference_rt_distance <- abs(reference_rt[[1]] - target_rt[[1]])
          }
        }

        reference_raw <- if (metric %in% names(reference)) {
          suppressWarnings(as.numeric(reference[[metric]]))
        } else {
          rep(NA_real_, nrow(reference))
        }
        reference_blank_signal <- NA_real_
        blank_id <- input$monitoring_blank_file_id %||% ""
        if (identical(treatment, "blank_corrected") && nzchar(blank_id)) {
          reference_blank <- reference[reference$parquet_id == blank_id, , drop = FALSE]
          if (nrow(reference_blank) == 1 && metric %in% names(reference_blank)) {
            reference_blank_signal <- suppressWarnings(as.numeric(reference_blank[[metric]][[1]]))
          }
        }
        reference_value <- if (identical(treatment, "blank_corrected")) {
          if (is.finite(reference_blank_signal)) reference_raw - reference_blank_signal else rep(NA_real_, nrow(reference))
        } else {
          reference_raw
        }

        match_index <- match(x$parquet_id, reference$parquet_id)
        matched <- !is.na(match_index)
        reference_detected <- rep(FALSE, nrow(x))
        reference_detected[matched] <- !is.na(reference$status[match_index[matched]]) &
          reference$status[match_index[matched]] == "Detected"
        x$normalisation_divisor[matched] <- reference_value[match_index[matched]]

        if (identical(treatment, "blank_corrected") && !is.finite(reference_blank_signal)) {
          x$normalisation_status <- "Blanc de l'etalon absent"
        } else {
          x$normalisation_status <- ifelse(
            !matched,
            "Etalon absent du fichier",
            ifelse(
              !reference_detected,
              "Etalon non detecte",
              ifelse(
                !is.finite(x$normalisation_divisor) | x$normalisation_divisor <= 0,
                "Diviseur non positif",
                "Appliquee"
              )
            )
          )
        }
        valid <- x$normalisation_status == "Appliquee" & is.finite(x$signal_traite)
        x$signal_normalise[valid] <- x$signal_traite[valid] / x$normalisation_divisor[valid]
      }
    } else if (identical(normalization, "total_intensity")) {
      x$normalisation_reference <- if ("total_intensity_mslevel" %in% names(x)) {
        paste0("Intensite totale MS", x$total_intensity_mslevel)
      } else {
        "Intensite totale"
      }
      total_status <- if ("total_intensity_status" %in% names(x)) {
        as.character(x$total_intensity_status)
      } else {
        rep("", nrow(x))
      }
      if (!"total_intensity" %in% names(x)) {
        x$normalisation_status <- ifelse(
          total_status == "Non demandee",
          "Intensite totale non calculee pour ce lot",
          "Intensite totale absente"
        )
      } else {
        x$normalisation_divisor <- suppressWarnings(as.numeric(x$total_intensity))
        x$normalisation_status <- ifelse(
          total_status == "Non demandee",
          "Intensite totale non calculee pour ce lot",
          ifelse(
            !is.finite(x$normalisation_divisor),
            "Intensite totale absente",
            ifelse(x$normalisation_divisor <= 0, "Diviseur non positif", "Appliquee")
          )
        )
        valid <- x$normalisation_status == "Appliquee" & is.finite(x$signal_traite)
        x$signal_normalise[valid] <- x$signal_traite[valid] / x$normalisation_divisor[valid]
      }
    }
    x$signal_value <- if (identical(normalization, "none")) x$signal_traite else x$signal_normalise
    x$monitoring_date <- monitoring_reference_dates(
      x$reference_year %||% NA_character_,
      x$reference_month %||% NA_character_,
      x$screened_at %||% NA_character_
    )
    is_blank <- rep(FALSE, nrow(x))
    if ("file_type" %in% names(x)) {
      is_blank <- !is.na(x$file_type) & x$file_type == "Blanc"
    }
    has_reference_period <- !is.na(x$reference_year) & nzchar(x$reference_year) &
      !is.na(x$reference_month) & nzchar(x$reference_month)
    blank_without_reference_period <- is_blank & !has_reference_period
    # An annual blank without a reference month is a horizontal baseline, not a
    # measurement at the date on which the screening happened.
    x$monitoring_date[blank_without_reference_period] <- as.Date(NA)
    x$period_label <- ifelse(is.na(x$monitoring_date), "Date inconnue", format(x$monitoring_date, "%Y-%m"))
    x$period_label[blank_without_reference_period] <- "Blanc de reference"
    x$selected_blank <- x$parquet_id == (input$monitoring_blank_file_id %||% "")
    x
  })

  monitoring_duplicates_summary <- reactive({
    summarise_monitoring_duplicates(monitoring_view_results())
  })

  monitoring_injection_aggregation <- reactive({
    method <- input$monitoring_injection_aggregation %||% "none"
    result <- aggregate_monitoring_stage(
      monitoring_view_results(),
      stage = "injections",
      method = method,
      exclude_outliers = isTRUE(input$monitoring_injection_exclude_outliers),
      outlier_threshold = input_number(input$monitoring_outlier_threshold, 3.5)
    )
    if (!is.null(result$results) && nrow(result$results) > 0) {
      result$results$injection_aggregation_method <- monitoring_aggregation_label(method)
      result$results$injection_aggregation_n_total <- result$results$aggregation_n_total
      result$results$injection_aggregation_n_used <- result$results$aggregation_n_used
      result$results$injection_aggregation_n_excluded <- result$results$aggregation_n_excluded
    }
    result
  })

  monitoring_duplicate_aggregation <- reactive({
    method <- input$monitoring_duplicate_aggregation %||% "none"
    result <- aggregate_monitoring_stage(
      monitoring_injection_aggregation()$results,
      stage = "duplicates",
      method = method,
      exclude_outliers = isTRUE(input$monitoring_duplicate_exclude_outliers),
      outlier_threshold = input_number(input$monitoring_outlier_threshold, 3.5)
    )
    if (!is.null(result$results) && nrow(result$results) > 0) {
      result$results$duplicate_aggregation_method <- monitoring_aggregation_label(method)
      result$results$duplicate_aggregation_n_total <- result$results$aggregation_n_total
      result$results$duplicate_aggregation_n_used <- result$results$aggregation_n_used
      result$results$duplicate_aggregation_n_excluded <- result$results$aggregation_n_excluded
    }
    result
  })

  monitoring_display_results <- reactive({
    monitoring_duplicate_aggregation()$results
  })

  monitoring_raw_audit <- reactive({
    monitoring_injection_aggregation()$audit
  })

  refresh_parquet_catalog <- function() {
    catalog <- parquet_catalog(parquet_root, nextcloud_selected_files())
    parquet_files(catalog)
    selected_screening_source_ids(intersect(selected_screening_source_ids(), catalog$parquet_id))
  }

  add_catalogue_files_to_workspace <- function(files) {
    ids <- matching_parquet_ids_for_metadata(files, parquet_files())
    if (length(ids) == 0) {
      showNotification(
        "Les fichiers sont indexes dans Catalogue, mais aucun Parquet correspondant n'est accessible. Verifie DATA_PATH ou ajoute les fichiers via Nextcloud.",
        type = "warning",
        duration = 10
      )
      return(invisible(character()))
    }

    selected_screening_source_ids(unique(c(selected_screening_source_ids(), ids)))
    updateSelectInput(
      session,
      "parquet_file_id",
      choices = make_parquet_choices(parquet_files()),
      selected = ids[[1]]
    )

    unavailable <- nrow(files) - length(ids)
    message <- paste0(
      length(ids), " fichier(s) ajoute(s) au lot ; le premier est selectionne dans Parquet."
    )
    if (unavailable > 0) {
      message <- paste0(message, " ", unavailable, " fichier(s) indexe(s) ne sont pas accessibles.")
    }
    showNotification(message, type = if (unavailable > 0) "warning" else "message", duration = 8)
    invisible(ids)
  }

  remove_catalogue_files_from_workspace <- function(files) {
    ids <- matching_parquet_ids_for_metadata(files, parquet_files())
    if (length(ids) > 0) {
      selected_screening_source_ids(setdiff(selected_screening_source_ids(), ids))
    }
    invisible(ids)
  }

  reset_parquet_outputs <- function() {
    parquet_info(NULL)
    tic_data(NULL)
    bpi_data(NULL)
    eic_data(NULL)
    eic_summary(NULL)
    eic_context(NULL)
    quick_eic_result(NULL)
    ms2_spectrum_data(NULL)
    ms2_spectrum_context(NULL)
    ms2_reference_comparison(NULL)
    screening_results(NULL)
  }

  next_custom_compound_ids <- function(count) {
    count <- as.integer(count)
    if (is.na(count) || count < 1) {
      return(character())
    }
    previous <- custom_compound_counter()
    sequence <- seq.int(previous + 1L, previous + count)
    custom_compound_counter(previous + count)
    sprintf("custom_%06d", sequence)
  }

  append_custom_compounds <- function(rows) {
    catalog <- compound_catalog()
    existing_keys <- compound_identity_key(catalog)
    candidate_keys <- compound_identity_key(rows)
    keep <- !candidate_keys %in% existing_keys & !duplicated(candidate_keys)
    added <- rows[keep, , drop = FALSE]
    skipped <- sum(!keep)
    if (nrow(added) > 0) {
      compound_catalog(rbind(catalog, added))
    }
    list(rows = added, skipped = skipped)
  }

  reset_custom_compounds <- function(compound_type) {
    catalog <- compound_catalog()
    types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    remove <- catalog$source_label != "Liste fournie" & types == compound_type
    removed_ids <- catalog$app_compound_id[remove]
    compound_catalog(catalog[!remove, , drop = FALSE])
    selected_compound_ids(setdiff(selected_compound_ids(), removed_ids))
    remaining_custom <- any(catalog$source_label != "Liste fournie" & !remove)
    if (!remaining_custom) {
      custom_compound_counter(0L)
    }
    length(removed_ids)
  }

  batch_file_value <- function(file, column, default = NA_character_) {
    if (!column %in% names(file) || nrow(file) == 0) {
      return(default)
    }
    value <- file[[column]][[1]]
    if (length(value) == 0 || is.na(value)) default else value
  }

  batch_source_for_file <- function(file) {
    if (nrow(file) != 1) {
      stop("Le lot doit contenir un seul fichier Parquet par etape.")
    }
    if (identical(batch_file_value(file, "source_type", "local"), "nextcloud")) {
      config <- nextcloud_session_config()
      if (is.null(config)) {
        stop("La session Nextcloud n'est plus disponible. Reconnecte Nextcloud avant de lancer le lot.")
      }
      path <- batch_file_value(file, "source_url", NA_character_)
      if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
        stop("La source Nextcloud de ce fichier est indisponible.")
      }
      return(list(path = path, http_headers = nextcloud_http_headers(config)))
    }

    path <- batch_file_value(file, "full_path", NA_character_)
    if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
      stop("La source locale de ce fichier est indisponible.")
    }
    list(path = path, http_headers = NULL)
  }

  batch_source_path_for_error <- function(file) {
    if (identical(batch_file_value(file, "source_type", "local"), "nextcloud")) {
      return(batch_file_value(file, "source_url", NA_character_))
    }
    batch_file_value(file, "full_path", NA_character_)
  }

  annotate_batch_results <- function(results, file, parameters, screened_at, total_signal = NULL, total_signal_error = "") {
    catalog <- compound_catalog()
    compound_match <- if ("compound_id" %in% names(results) && "compound_id" %in% names(catalog)) {
      match(as.character(results$compound_id), as.character(catalog$compound_id))
    } else {
      rep(NA_integer_, nrow(results))
    }
    results$compound_type <- if ("compound_type" %in% names(catalog)) {
      as.character(catalog$compound_type[compound_match])
    } else {
      rep(NA_character_, nrow(results))
    }
    results$parquet_id <- batch_file_value(file, "parquet_id")
    results$source_type <- batch_file_value(file, "source_type")
    results$file_relative_path <- batch_file_value(file, "relative_path")
    results$file_mode <- batch_file_value(file, "parquet_mode")
    results$file_mode_source <- batch_file_value(file, "mode_source")
    results$reference_year <- batch_file_value(file, "reference_year")
    results$reference_month <- batch_file_value(file, "reference_month")
    results$file_type <- batch_file_value(file, "file_type")
    results$duplicate_label <- batch_file_value(file, "duplicate_label")
    results$replicate_label <- batch_file_value(file, "replicate_label")
    results$sample_name <- batch_file_value(file, "sample_name")
    results$sample_base_name <- batch_file_value(file, "sample_base_name")
    results$sample_group <- batch_file_value(file, "sample_group")
    results$has_ccs_calibration <- parse_optional_logical(batch_file_value(file, "has_ccs_calibration", NA))[[1]]
    results$ccs_calibration_c1 <- ccs_calibration_for_file(file)$C1
    results$ccs_calibration_c2 <- ccs_calibration_for_file(file)$C2
    results$mslevel <- parameters$mslevel
    results$min_intensity <- parameters$min_intensity
    results$total_intensity <- if (!is.null(total_signal) && nrow(total_signal) > 0 && "total_intensity" %in% names(total_signal)) {
      suppressWarnings(as.numeric(total_signal$total_intensity[[1]]))
    } else {
      NA_real_
    }
    results$total_intensity_n_points <- if (!is.null(total_signal) && nrow(total_signal) > 0 && "n_points" %in% names(total_signal)) {
      suppressWarnings(as.integer(total_signal$n_points[[1]]))
    } else {
      NA_integer_
    }
    results$total_intensity_mslevel <- parameters$mslevel
    results$total_intensity_error <- total_signal_error %||% ""
    results$total_intensity_status <- if (!isTRUE(parameters$compute_total_intensity)) {
      "Non demandee"
    } else if (!is.null(total_signal) && nrow(total_signal) > 0 &&
      "total_intensity" %in% names(total_signal) && is.finite(results$total_intensity[[1]])) {
      "Calculee"
    } else if (nzchar(results$total_intensity_error[[1]])) {
      "Erreur"
    } else {
      "Indisponible"
    }
    results$screened_at <- screened_at
    results
  }

  batch_error_rows <- function(file, compounds, source_path, parameters, error) {
    error_message <- if (identical(batch_file_value(file, "source_type", "local"), "nextcloud")) {
      "Erreur de lecture du fichier Nextcloud. Verifie la connexion et les droits d'acces."
    } else {
      conditionMessage(error)
    }
    rows <- lapply(seq_len(nrow(compounds)), function(index) {
      screening_error_row(
        source_path,
        compounds[index, , drop = FALSE],
        mz_tolerance = parameters$mz_tolerance,
        rt_tolerance = parameters$rt_tolerance,
        require_rt_match = parameters$require_rt_match,
        use_dt = parameters$use_dt,
        dt_tolerance_pct = parameters$dt_tolerance_pct,
        use_ccs = parameters$use_ccs,
        ccs_tolerance_pct = parameters$ccs_tolerance_pct,
        error_message = error_message
      )
    })
    annotate_batch_results(
      do.call(rbind, rows),
      file,
      parameters,
      format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  }

  validate_batch_parameters <- function(parameters) {
    if (!is.finite(parameters$mz_tolerance) || parameters$mz_tolerance <= 0) {
      stop("La tolerance m/z du lot doit etre strictement positive.")
    }
    if (!is.finite(parameters$rt_tolerance) || parameters$rt_tolerance < 0) {
      stop("La tolerance RT du lot doit etre positive ou nulle.")
    }
    if (!is.finite(parameters$min_intensity) || parameters$min_intensity < 0) {
      stop("L'intensite minimale du lot doit etre positive ou nulle.")
    }
    if (!is.finite(parameters$dt_tolerance_pct) || parameters$dt_tolerance_pct < 0) {
      stop("La tolerance DT du lot doit etre positive ou nulle.")
    }
    if (!is.finite(parameters$ccs_tolerance_pct) || parameters$ccs_tolerance_pct < 0) {
      stop("La tolerance CCS du lot doit etre positive ou nulle.")
    }
    if (!parameters$mslevel %in% c("1", "2")) {
      stop("Le niveau MS du lot doit etre MS1 ou MS2.")
    }
    invisible(TRUE)
  }

  filtered_files <- reactive({
    x <- metadata_index
    file_year <- input$file_year %||% ""
    file_mode <- input$file_mode %||% ""
    file_type <- input$file_type %||% ""
    file_month <- input$file_month %||% ""
    file_duplicate <- input$file_duplicate %||% ""
    if (nzchar(file_year)) x <- x[x$year_dir == file_year, , drop = FALSE]
    if (nzchar(file_mode)) x <- x[x$mode_dir == file_mode, , drop = FALSE]
    if (nzchar(file_type)) x <- x[x$file_type == file_type, , drop = FALSE]
    if (nzchar(file_month)) x <- x[x$reference_month == file_month, , drop = FALSE]
    if (nzchar(file_duplicate)) x <- x[x$duplicate_label == file_duplicate, , drop = FALSE]
    x
  })

  filtered_compounds <- reactive({
    filter_catalog_compounds(
      compound_catalog(),
      compound_type = "internal_standard",
      mode = input$compound_mode %||% "",
      search = input$compound_search %||% "",
      rt_range = input$compound_rt %||% numeric()
    )
  })

  filtered_suspects <- reactive({
    filter_catalog_compounds(
      compound_catalog(),
      compound_type = "suspect",
      mode = input$suspect_mode %||% "",
      search = input$suspect_search %||% "",
      rt_range = input$suspect_rt %||% numeric()
    )
  })

  selected_files <- reactive({
    subset_by_ids(metadata_index, "app_file_id", selected_file_ids())
  })

  selected_compounds <- reactive({
    subset_by_ids(compound_catalog(), "app_compound_id", selected_compound_ids())
  })

  selected_internal_standards <- reactive({
    filter_compounds_by_type(selected_compounds(), "internal_standard")
  })

  selected_suspects <- reactive({
    filter_compounds_by_type(selected_compounds(), "suspect")
  })

  filtered_screening_sources <- reactive({
    x <- parquet_files()
    source_origin <- input$screening_source_origin %||% ""
    source_mode <- input$screening_source_mode %||% ""
    source_search <- tolower(input$screening_source_search %||% "")
    if (nzchar(source_origin)) x <- x[x$source_type == source_origin, , drop = FALSE]
    if (nzchar(source_mode)) x <- x[x$parquet_mode == source_mode, , drop = FALSE]
    if (nzchar(source_search)) {
      x <- x[grepl(source_search, tolower(x$relative_path), fixed = TRUE), , drop = FALSE]
    }
    x
  })

  selected_screening_sources <- reactive({
    subset_by_ids(parquet_files(), "parquet_id", selected_screening_source_ids())
  })

  selected_parquet_file <- reactive({
    files <- parquet_files()
    id <- input$parquet_file_id %||% ""
    if (!nzchar(id) || nrow(files) == 0) {
      return(files[0, , drop = FALSE])
    }
    files[files$parquet_id == id, , drop = FALSE]
  })

  parquet_source_from_file <- function(selected) {
    if (nrow(selected) == 0) {
      return(empty_parquet_source())
    }
    if (identical(selected$source_type[[1]], "nextcloud")) {
      config <- nextcloud_session_config()
      if (is.null(config)) {
        return(empty_parquet_source())
      }
      return(list(
        path = selected$source_url[[1]],
        http_headers = nextcloud_http_headers(config)
      ))
    }
    list(path = selected$full_path[[1]], http_headers = NULL)
  }

  selected_parquet_source <- reactive({
    parquet_source_from_file(selected_parquet_file())
  })

  control_selected_parquet_file <- reactive({
    files <- parquet_files()
    id <- input$control_file_id %||% ""
    if (!nzchar(id) || nrow(files) == 0) {
      return(files[0, , drop = FALSE])
    }
    files[files$parquet_id == id, , drop = FALSE]
  })

  control_selected_parquet_source <- reactive({
    parquet_source_from_file(control_selected_parquet_file())
  })

  selected_parquet_context <- reactive({
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      return(list(mode = NA_character_, source = "Aucun fichier", metadata_match = FALSE))
    }

    override <- input$parquet_mode_override %||% "auto"
    automatic_mode <- selected$parquet_mode[[1]]
    if (override %in% c("pos", "neg")) {
      return(list(mode = override, source = "Manuel", metadata_match = selected$metadata_match[[1]]))
    }
    list(
      mode = if (valid_mode(automatic_mode)) tolower(automatic_mode) else NA_character_,
      source = selected$mode_source[[1]],
      metadata_match = selected$metadata_match[[1]]
    )
  })

  selected_eic_compound <- reactive({
    catalog <- compound_catalog()
    id <- input$eic_compound_id %||% ""
    if (!nzchar(id)) {
      return(catalog[0, , drop = FALSE])
    }
    catalog[catalog$app_compound_id == id, , drop = FALSE]
  })

  selected_ms2_reference <- reactive({
    ms2_reference_spectrum(
      ms2_reference_catalog(),
      input$ms2_reference_id %||% ""
    )
  })

  calculate_eic_result <- function(source, target_mz, expected_rt, mz_tolerance, rt_tolerance,
                                   min_intensity, mslevel, restrict_to_rt_window = TRUE) {
    eic <- suppressWarnings(compute_eic(
      source$path,
      target_mz = target_mz,
      mz_tolerance = mz_tolerance,
      mslevel = mslevel,
      min_intensity = min_intensity,
      http_headers = source$http_headers
    ))
    eic <- as.data.frame(eic)
    list(
      data = eic,
      summary = summarise_eic_detection(
        eic,
        expected_rt = expected_rt,
        rt_tolerance = rt_tolerance,
        restrict_to_rt_window = restrict_to_rt_window
      )
    )
  }

  eic_context_label <- function(compound, target_mz) {
    compound_mz <- if (nrow(compound) == 1) suppressWarnings(as.numeric(compound$mz[[1]])) else NA_real_
    if (nrow(compound) == 1 && is.finite(compound_mz) && isTRUE(all.equal(compound_mz, target_mz))) {
      return(as.character(compound$name[[1]]))
    }
    "Recherche libre"
  }

  quick_eic_message <- function(target_mz, summary) {
    mz_label <- format(target_mz, trim = TRUE)
    detected <- nrow(summary) == 1 && identical(summary$status[[1]], "Detected")
    if (detected) {
      return(paste0("Signal m/z observe : m/z ", mz_label))
    }
    paste0("Aucun signal m/z observe : m/z ", mz_label)
  }

  screening_compounds_for_current_file <- reactive({
    catalog <- compound_catalog()
    file_mode <- selected_parquet_context()$mode
    if (!valid_mode(file_mode)) {
      return(catalog[0, , drop = FALSE])
    }
    if (isTRUE(input$screen_selected_compounds_only)) {
      selected <- selected_compounds()
      if (nrow(selected) > 0) {
        return(selected[selected$mode == file_mode, , drop = FALSE])
      }
      return(catalog[0, , drop = FALSE])
    }
    catalog[catalog$mode == file_mode, , drop = FALSE]
  })

  observe({
    updateSelectInput(session, "file_to_remove", choices = make_file_choices(selected_files()))
  })

  observe({
    updateSelectInput(
      session,
      "compound_to_remove",
      choices = make_compound_choices(selected_internal_standards(), empty_label = "Aucun etalon")
    )
  })

  observe({
    updateSelectInput(
      session,
      "suspect_to_remove",
      choices = make_compound_choices(selected_suspects(), empty_label = "Aucun suspect")
    )
  })

  observe({
    files <- parquet_files()
    choices <- make_parquet_choices(files)
    available_ids <- unname(choices)
    requested <- input$control_file_id %||% input$parquet_file_id %||% ""
    selected <- if (requested %in% available_ids) requested else ""
    updateSelectInput(session, "control_file_id", choices = choices, selected = selected)
  })

  observe({
    catalog <- compound_catalog()
    current <- input$eic_compound_id %||% ""
    selected <- if (current %in% catalog$app_compound_id) current else ""
    updateSelectInput(
      session,
      "eic_compound_id",
      choices = make_compound_choices(catalog, empty_label = "Aucune molecule"),
      selected = selected
    )
  })

  observe({
    references <- ms2_reference_catalog()
    choices <- make_ms2_reference_choices(references)
    current <- input$ms2_reference_id %||% ""
    selected <- if (current %in% unname(choices)) current else ""
    updateSelectInput(session, "ms2_reference_id", choices = choices, selected = selected)
  })

  observeEvent(input$eic_compound_id, {
    matches <- ms2_reference_ids_for_compound(
      ms2_reference_catalog(),
      selected_eic_compound()
    )
    if (length(matches) == 1) {
      updateSelectInput(session, "ms2_reference_id", selected = matches[[1]])
    }
  }, ignoreInit = TRUE)

  update_compound_rt_slider <- function(input_id, x) {
    rt_values <- x$rt[is.finite(x$rt)]
    if (length(rt_values) == 0) {
      updateSliderInput(session, input_id, min = 0, max = 1, value = c(0, 1))
      return(invisible(NULL))
    }
    lower <- floor(min(rt_values))
    upper <- ceiling(max(rt_values))
    if (lower == upper) {
      lower <- max(0, lower - 1)
      upper <- upper + 1
    }
    updateSliderInput(session, input_id, min = lower, max = upper, value = c(lower, upper))
  }

  observeEvent(compound_catalog(), {
    catalog <- compound_catalog()
    selected_compound_ids(intersect(selected_compound_ids(), catalog$app_compound_id))
    update_compound_rt_slider("compound_rt", filter_compounds_by_type(catalog, "internal_standard"))
    update_compound_rt_slider("suspect_rt", filter_compounds_by_type(catalog, "suspect"))
  }, ignoreInit = TRUE)

  observe({
    updateSelectInput(session, "parquet_file_id", choices = make_parquet_choices(parquet_files()))
  })

  observe({
    updateSelectInput(
      session,
      "screening_source_to_remove",
      choices = make_screening_source_choices(selected_screening_sources())
    )
  })

  observe({
    results <- batch_screening_results()
    choices <- make_monitoring_compound_choices(results)
    current <- input$monitoring_compound_id %||% ""
    available <- unname(choices)
    available <- available[nzchar(available)]
    selected <- if (current %in% available) current else if (length(available) > 0) available[[1]] else ""
    updateSelectInput(session, "monitoring_compound_id", choices = choices, selected = selected)
  })

  observe({
    blanks <- monitoring_blank_rows()
    choices <- make_monitoring_blank_choices(blanks)
    current <- input$monitoring_blank_file_id %||% ""
    available <- unname(choices)
    available <- available[nzchar(available)]
    selected <- if (current %in% available) current else ""
    updateSelectInput(session, "monitoring_blank_file_id", choices = choices, selected = selected)
  })

  observe({
    references <- monitoring_normalization_reference_candidates()
    choices <- make_monitoring_reference_choices(references)
    current <- input$monitoring_normalization_compound_id %||% ""
    available <- unname(choices)
    available <- available[nzchar(available)]
    selected <- if (current %in% available) current else ""
    updateSelectInput(session, "monitoring_normalization_compound_id", choices = choices, selected = selected)
  })

  observeEvent(input$add_screening_sources, {
    rows <- input$screening_source_catalog_table_rows_selected %||% integer()
    candidates <- filtered_screening_sources()
    ids <- candidates$parquet_id[rows]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) {
      showNotification("Selectionne au moins un fichier Parquet pour le lot.", type = "warning")
      return()
    }
    selected_screening_source_ids(unique(c(selected_screening_source_ids(), ids)))
  })

  observeEvent(input$add_filtered_screening_sources, {
    ids <- filtered_screening_sources()$parquet_id
    if (length(ids) == 0) {
      showNotification("Aucun fichier Parquet ne correspond aux filtres.", type = "warning")
      return()
    }
    selected_screening_source_ids(unique(c(selected_screening_source_ids(), ids)))
  })

  observeEvent(input$remove_screening_source, {
    id <- input$screening_source_to_remove %||% ""
    if (!nzchar(id)) {
      return()
    }
    selected_screening_source_ids(setdiff(selected_screening_source_ids(), id))
  })

  observeEvent(input$clear_screening_sources, {
    selected_screening_source_ids(character())
  })

  observeEvent(input$eic_compound_id, {
    compound <- selected_eic_compound()
    if (nrow(compound) == 0) {
      return()
    }
    if (is.finite(compound$mz[[1]])) {
      updateNumericInput(session, "eic_target_mz", value = compound$mz[[1]])
      updateNumericInput(session, "quick_eic_target_mz", value = compound$mz[[1]])
    }
    if (is.finite(compound$rt[[1]])) {
      updateNumericInput(session, "eic_expected_rt", value = compound$rt[[1]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$parquet_file_id, {
    updateSelectInput(session, "parquet_mode_override", selected = "auto")
    reset_parquet_outputs()
  }, ignoreInit = TRUE)

  load_nextcloud_contents <- function(reset_path = FALSE) {
    config <- tryCatch(
      nextcloud_connection_config(
        input$nextcloud_access_mode %||% "public",
        input$nextcloud_base_url,
        share_token = resolve_nextcloud_share_token(input$nextcloud_share_token),
        username = input$nextcloud_username %||% "",
        app_password = input$nextcloud_app_password %||% ""
      ),
      error = function(e) e
    )
    if (inherits(config, "error")) {
      nextcloud_connection_status(conditionMessage(config))
      showNotification(conditionMessage(config), type = "warning", duration = 8)
      return(invisible(FALSE))
    }

    if (isTRUE(reset_path)) {
      nextcloud_current_path(config$initial_path)
    }

    withProgress(message = "Lecture du dossier Nextcloud", value = 0, {
      incProgress(0.2, detail = "Requete WebDAV")
      contents <- tryCatch(
        list_nextcloud_contents(config, nextcloud_current_path()),
        error = function(e) e
      )
      if (inherits(contents, "error")) {
        nextcloud_connection_status(conditionMessage(contents))
        showNotification(conditionMessage(contents), type = "error", duration = 10)
        return(invisible(FALSE))
      }

      previous_config <- nextcloud_session_config()
      connection_changed <- is.null(previous_config) ||
        !identical(previous_config$access_mode, config$access_mode) ||
        !identical(previous_config$base_url, config$base_url) ||
        !identical(previous_config$authorization_header, config$authorization_header)
      if (isTRUE(connection_changed) && nrow(nextcloud_selected_files()) > 0) {
        nextcloud_selected_files(empty_nextcloud_contents())
        refresh_parquet_catalog()
        reset_parquet_outputs()
      }
      nextcloud_session_config(config)
      nextcloud_contents(contents)
      nextcloud_connection_status(paste0(nrow(contents), " element(s) lus - ", config$access_mode))
      invisible(TRUE)
    })
  }

  nextcloud_parent_path <- function(path) {
    parts <- strsplit(normalize_nextcloud_subpath(path), "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) <= 1) "" else paste(parts[-length(parts)], collapse = "/")
  }

  observeEvent(input$nextcloud_refresh, {
    load_nextcloud_contents(reset_path = TRUE)
  })

  observeEvent(input$nextcloud_up, {
    current_path <- nextcloud_current_path()
    if (!nzchar(current_path)) {
      return()
    }
    nextcloud_current_path(nextcloud_parent_path(current_path))
    load_nextcloud_contents()
  })

  observeEvent(input$nextcloud_contents_table_rows_selected, {
    rows <- input$nextcloud_contents_table_rows_selected %||% integer()
    contents <- nextcloud_contents()
    if (length(rows) != 1 || rows[[1]] > nrow(contents)) {
      return()
    }
    item <- contents[rows[[1]], , drop = FALSE]
    if (isTRUE(item$is_folder[[1]])) {
      nextcloud_current_path(item$path[[1]])
      load_nextcloud_contents()
      return()
    }
    if (!grepl("\\.parquet$", item$name[[1]], ignore.case = TRUE)) {
      showNotification("Seuls les fichiers Parquet peuvent etre ajoutes au catalogue.", type = "warning")
      return()
    }

    selected <- nextcloud_selected_files()
    if (item$path[[1]] %in% selected$path) {
      showNotification("Ce fichier distant est deja ajoute.", type = "message")
      return()
    }
    nextcloud_selected_files(rbind(selected, item))
    refresh_parquet_catalog()
    showNotification("Fichier Nextcloud ajoute au catalogue Parquet.", type = "message")
  })

  observe({
    updateSelectInput(session, "nextcloud_file_to_remove", choices = make_nextcloud_file_choices(nextcloud_selected_files()))
  })

  observeEvent(input$nextcloud_remove_file, {
    path <- input$nextcloud_file_to_remove %||% ""
    if (!nzchar(path)) {
      return()
    }
    selected <- nextcloud_selected_files()
    nextcloud_selected_files(selected[selected$path != path, , drop = FALSE])
    refresh_parquet_catalog()
    reset_parquet_outputs()
  })

  observeEvent(input$nextcloud_clear_files, {
    nextcloud_selected_files(empty_nextcloud_contents())
    refresh_parquet_catalog()
    reset_parquet_outputs()
  })

  observeEvent(input$refresh_parquet_files, {
    refresh_parquet_catalog()
    reset_parquet_outputs()
    showNotification("Liste des fichiers Parquet rafraichie.", type = "message")
  })

  observeEvent(input$inspect_parquet, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    withProgress(message = "Lecture des informations Parquet", value = 0, {
      incProgress(0.2, detail = "Ouverture du dataset")
      info <- tryCatch(
        inspect_parquet_file(source_path, http_headers = source$http_headers),
        error = function(e) e
      )
      if (inherits(info, "error")) {
        showNotification(conditionMessage(info), type = "error", duration = 10)
        return()
      }
      incProgress(0.8, detail = "Resume pret")
      parquet_info(info)
    })
  })

  observeEvent(input$run_control_file_checks, {
    selected <- control_selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- control_selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      control_parquet_info(NULL)
      control_parquet_error("La source de ce fichier Parquet est indisponible.")
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    withProgress(message = "Controle du fichier Parquet", value = 0, {
      incProgress(0.2, detail = "Lecture du schema et des statistiques")
      info <- tryCatch(
        inspect_parquet_file(source_path, http_headers = source$http_headers),
        error = function(e) e
      )
      if (inherits(info, "error")) {
        control_parquet_info(NULL)
        control_parquet_error(conditionMessage(info))
        showNotification(conditionMessage(info), type = "error", duration = 10)
        return()
      }
      incProgress(0.8, detail = "Diagnostic pret")
      control_parquet_info(info)
      control_parquet_error(NULL)
    })
  })

  observeEvent(input$compute_tic_bpi, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    withProgress(message = "Calcul TIC/BPI", value = 0, {
      incProgress(0.15, detail = "Lecture et aggregation des scans")
      chromatograms <- tryCatch(
        suppressWarnings(compute_tic_bpi(
          source_path,
          mslevel = input$chrom_mslevel,
          http_headers = source$http_headers
        )),
        error = function(e) e
      )
      if (inherits(chromatograms, "error")) {
        showNotification(conditionMessage(chromatograms), type = "error", duration = 10)
        return()
      }

      incProgress(0.85, detail = "Resultats prets")
      tic_data(as.data.frame(chromatograms$tic))
      bpi_data(as.data.frame(chromatograms$bpi))
    })
  })

  observeEvent(input$compute_eic, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    compound <- selected_eic_compound()
    target_mz <- input_number(input$eic_target_mz, if (nrow(compound) > 0) compound$mz[[1]] else NA_real_)
    expected_rt <- input_number(input$eic_expected_rt, if (nrow(compound) > 0) compound$rt[[1]] else NA_real_)
    mz_tolerance <- input_number(input$eic_mz_tolerance, 0.01)
    rt_tolerance <- input_number(input$eic_rt_tolerance, 0.5)
    min_intensity <- input_number(input$eic_min_intensity, 0)
    quick_eic_result(NULL)

    withProgress(message = "Calcul EIC", value = 0, {
      incProgress(0.3, detail = "Filtrage m/z")
      calculation <- tryCatch(
        calculate_eic_result(
          source,
          target_mz = target_mz,
          expected_rt = expected_rt,
          mz_tolerance = mz_tolerance,
          rt_tolerance = rt_tolerance,
          min_intensity = min_intensity,
          mslevel = input$chrom_mslevel
        ),
        error = function(e) e
      )
      if (inherits(calculation, "error")) {
        showNotification(conditionMessage(calculation), type = "error", duration = 10)
        return()
      }

      incProgress(0.7, detail = "Resume")
      eic_data(calculation$data)
      eic_summary(calculation$summary)
      eic_context(list(
        label = eic_context_label(compound, target_mz),
        target_mz = target_mz,
        mz_tolerance = mz_tolerance
      ))
    })
  })

  observeEvent(input$run_quick_eic, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    compound <- selected_eic_compound()
    target_mz <- input_number(input$quick_eic_target_mz, NA_real_)
    mz_tolerance <- input_number(input$eic_mz_tolerance, 0.01)
    if (!is.finite(target_mz) || target_mz < 0) {
      showNotification("Renseigne un m/z valide avant la recherche rapide.", type = "warning")
      return()
    }
    if (!is.finite(mz_tolerance) || mz_tolerance <= 0) {
      showNotification("La tolerance m/z doit etre valide avant la recherche rapide.", type = "warning")
      return()
    }

    updateSelectInput(session, "chrom_mslevel", selected = "1")
    updateNumericInput(session, "eic_target_mz", value = target_mz)
    quick_eic_result(NULL)

    withProgress(message = "Recherche rapide EIC", value = 0, {
      incProgress(0.3, detail = "Recherche du m/z en MS1")
      calculation <- tryCatch(
        calculate_eic_result(
          source,
          target_mz = target_mz,
          expected_rt = NA_real_,
          mz_tolerance = mz_tolerance,
          rt_tolerance = NA_real_,
          min_intensity = 0,
          mslevel = "1",
          restrict_to_rt_window = FALSE
        ),
        error = function(e) e
      )
      if (inherits(calculation, "error")) {
        showNotification(conditionMessage(calculation), type = "error", duration = 10)
        return()
      }

      incProgress(0.7, detail = "EIC pret")
      eic_data(calculation$data)
      eic_summary(calculation$summary)
      eic_context(list(
        label = eic_context_label(compound, target_mz),
        target_mz = target_mz,
        mz_tolerance = mz_tolerance
      ))
      message <- quick_eic_message(target_mz, calculation$summary)
      quick_eic_result(list(message = message, detected = identical(calculation$summary$status[[1]], "Detected")))
      showNotification(message, type = if (isTRUE(quick_eic_result()$detected)) "message" else "warning", duration = 7)
    })
  })

  observeEvent(input$import_ms2_reference_csv, {
    uploaded <- input$import_ms2_reference_csv
    if (is.null(uploaded) || is.null(uploaded$datapath) || !nzchar(uploaded$datapath)) {
      return()
    }
    references <- tryCatch(read_ms2_reference_csv(uploaded$datapath), error = function(e) e)
    if (inherits(references, "error")) {
      showNotification(conditionMessage(references), type = "warning", duration = 10)
      return()
    }
    ms2_reference_catalog(references)
    ms2_reference_comparison(NULL)
    matching_references <- ms2_reference_ids_for_compound(references, selected_eic_compound())
    if (length(matching_references) == 1) {
      updateSelectInput(session, "ms2_reference_id", selected = matching_references[[1]])
    }
    showNotification(
      paste0(nrow(ms2_reference_summary(references)), " spectre(s) MS2 de reference importe(s)."),
      type = "message",
      duration = 8
    )
  })

  observeEvent(input$reset_ms2_reference_library, {
    ms2_reference_catalog(default_ms2_reference_spectra)
    ms2_reference_comparison(NULL)
    showNotification("References MS2 locales restaurees.", type = "message", duration = 6)
  })

  observeEvent(input$compute_ms2_spectrum, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    compound <- selected_eic_compound()
    expected_rt <- input_number(input$eic_expected_rt, if (nrow(compound) > 0) compound$rt[[1]] else NA_real_)
    rt_tolerance <- input_number(input$eic_rt_tolerance, 0.5)
    if (!is.finite(expected_rt) || !is.finite(rt_tolerance) || rt_tolerance < 0) {
      showNotification("Renseigne un RT attendu et une tolerance RT valide pour afficher le spectre MS2.", type = "warning")
      return()
    }
    bin_width <- input_number(input$ms2_bin_width, 0.01)
    min_intensity <- input_number(input$ms2_min_intensity, 0)
    top_n <- input_number(input$ms2_top_n, 150)
    ms2_spectrum_data(NULL)
    ms2_spectrum_context(NULL)
    ms2_reference_comparison(NULL)

    withProgress(message = "Lecture du spectre MS2", value = 0, {
      incProgress(0.2, detail = "Filtrage de la fenetre RT")
      spectrum <- tryCatch(
        suppressWarnings(compute_ms2_spectrum(
          source_path,
          expected_rt = expected_rt,
          rt_tolerance = rt_tolerance,
          bin_width = bin_width,
          min_intensity = min_intensity,
          top_n = top_n,
          http_headers = source$http_headers
        )),
        error = function(e) e
      )
      if (inherits(spectrum, "error")) {
        showNotification(conditionMessage(spectrum), type = "error", duration = 10)
        return()
      }

      incProgress(0.8, detail = "Spectre pret")
      ms2_spectrum_data(as.data.frame(spectrum))
      ms2_spectrum_context(list(
        compound_label = if (nrow(compound) == 0) "molecule selectionnee" else compound$name[[1]],
        expected_rt = expected_rt,
        rt_tolerance = rt_tolerance
      ))
    })
  })

  observeEvent(input$compare_ms2_spectrum, {
    spectrum <- ms2_spectrum_data()
    if (is.null(spectrum) || nrow(spectrum) == 0) {
      showNotification("Affiche d'abord un spectre MS2 avant de le comparer.", type = "warning", duration = 8)
      return()
    }
    reference <- selected_ms2_reference()
    if (nrow(reference) == 0) {
      showNotification("Selectionne ou importe un spectre MS2 de reference.", type = "warning", duration = 8)
      return()
    }

    file_mode <- selected_parquet_context()$mode
    reference_mode <- reference$mode[[1]]
    if (valid_mode(file_mode) && nzchar(reference_mode) && !identical(file_mode, reference_mode)) {
      showNotification("Le mode du spectre de reference ne correspond pas au mode du fichier Parquet.", type = "warning", duration = 10)
      return()
    }

    target_mz <- input_number(input$eic_target_mz, NA_real_)
    precursor_mz <- suppressWarnings(as.numeric(reference$precursor_mz[[1]]))
    mz_tolerance <- input_number(input$eic_mz_tolerance, 0.01)
    if (is.finite(target_mz) && is.finite(precursor_mz) && is.finite(mz_tolerance) &&
      abs(target_mz - precursor_mz) > mz_tolerance) {
      showNotification(
        "Attention : le precurseur du spectre de reference ne correspond pas au m/z EIC actuel. La comparaison reste exploratoire.",
        type = "warning",
        duration = 10
      )
    }

    comparison <- tryCatch(
      compare_ms2_spectrum_to_reference(
        spectrum,
        reference,
        mz_tolerance = input_number(input$ms2_match_mz_tolerance, 0.01),
        min_matched_fragments = input_number(input$ms2_min_matched_fragments, 3),
        min_cosine_similarity = input_number(input$ms2_min_cosine_similarity, 0.7)
      ),
      error = function(e) e
    )
    if (inherits(comparison, "error")) {
      showNotification(conditionMessage(comparison), type = "error", duration = 10)
      return()
    }
    ms2_reference_comparison(comparison)
    showNotification(comparison$summary$technical_status[[1]], type = "message", duration = 8)
  })

  observeEvent(input$run_current_file_screening, {
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      showNotification("Aucun fichier Parquet selectionne.", type = "warning")
      return()
    }
    source <- selected_parquet_source()
    source_path <- source$path
    if (is.na(source_path) || !nzchar(source_path)) {
      showNotification("La source de ce fichier Parquet est indisponible.", type = "error")
      return()
    }

    file_context <- selected_parquet_context()
    if (!valid_mode(file_context$mode)) {
      showNotification("Mode du fichier inconnu. Choisis Positif ou Negatif, ou utilise un Parquet associe a ses metadonnees JSON.", type = "warning", duration = 10)
      return()
    }

    compounds <- screening_compounds_for_current_file()
    if (nrow(compounds) == 0) {
      showNotification(paste0("Aucune molecule ", file_context$mode, " compatible avec la selection."), type = "warning", duration = 8)
      return()
    }

    mz_tolerance <- input_number(input$eic_mz_tolerance, 0.01)
    rt_tolerance <- input_number(input$eic_rt_tolerance, 0.5)
    min_intensity <- input_number(input$screening_min_intensity, 1000)
    dt_tolerance_pct <- input_number(input$screen_dt_tolerance_pct, 10)
    ccs_tolerance_pct <- input_number(input$screen_ccs_tolerance_pct, 10)

    withProgress(message = "Screening du fichier courant", value = 0, {
      incProgress(0.15, detail = "Lecture des fenetres m/z compatibles")
      results <- tryCatch(
        suppressWarnings(screen_compounds_in_file(
          source_path,
          compounds,
          mz_tolerance = mz_tolerance,
          rt_tolerance = rt_tolerance,
          mslevel = input$chrom_mslevel,
          min_intensity = min_intensity,
          require_rt_match = isTRUE(input$screen_require_rt_match),
          use_dt = isTRUE(input$screen_use_dt),
          dt_tolerance_pct = dt_tolerance_pct,
          use_ccs = isTRUE(input$screen_use_ccs),
          ccs_tolerance_pct = ccs_tolerance_pct,
          ccs_calibration = ccs_calibration_for_file(selected),
          ccs_to_drifttime = arcms_ccs_to_drifttime_converter(),
          http_headers = source$http_headers
        )),
        error = function(e) e
      )
      if (inherits(results, "error")) {
        showNotification(conditionMessage(results), type = "error", duration = 10)
        return()
      }
      incProgress(0.85, detail = "Evaluation des molecules")
      results <- as.data.frame(results)
      results$compound_type <- if ("compound_type" %in% names(compounds)) {
        as.character(compounds$compound_type[match(results$compound_id, compounds$compound_id)])
      } else {
        NA_character_
      }
      results$file_relative_path <- selected$relative_path[[1]]
      results$file_mode <- file_context$mode
      results$file_mode_source <- file_context$source
      results$has_ccs_calibration <- parse_optional_logical(selected$has_ccs_calibration[[1]])[[1]]
      results$ccs_calibration_c1 <- ccs_calibration_for_file(selected)$C1
      results$ccs_calibration_c2 <- ccs_calibration_for_file(selected)$C2
      results$mslevel <- input$chrom_mslevel
      results$min_intensity <- min_intensity
      results$screened_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      screening_results(results)
    })
  })

  observeEvent(input$add_selected_files, {
    rows <- input$files_table_rows_selected %||% integer()
    files <- filtered_files()[rows, , drop = FALSE]
    ids <- files$app_file_id
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) {
      showNotification("Aucun fichier selectionne.", type = "warning")
      return()
    }
    selected_file_ids(unique(c(selected_file_ids(), ids)))
    add_catalogue_files_to_workspace(files)
  })

  observeEvent(input$add_filtered_files, {
    files <- filtered_files()
    ids <- files$app_file_id
    if (length(ids) == 0) {
      showNotification("Aucun fichier avec ces filtres.", type = "warning")
      return()
    }
    selected_file_ids(unique(c(selected_file_ids(), ids)))
    add_catalogue_files_to_workspace(files)
  })

  observeEvent(input$remove_file, {
    id <- input$file_to_remove %||% ""
    if (!nzchar(id)) return()
    files <- subset_by_ids(metadata_index, "app_file_id", id)
    selected_file_ids(setdiff(selected_file_ids(), id))
    remove_catalogue_files_from_workspace(files)
  })

  observeEvent(input$clear_files, {
    remove_catalogue_files_from_workspace(selected_files())
    selected_file_ids(character())
  })

  observeEvent(input$add_selected_compounds, {
    rows <- input$compounds_table_rows_selected %||% integer()
    ids <- filtered_compounds()$app_compound_id[rows]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) {
      showNotification("Aucun etalon selectionne.", type = "warning")
      return()
    }
    selected_compound_ids(unique(c(selected_compound_ids(), ids)))
  })

  observeEvent(input$add_filtered_compounds, {
    ids <- filtered_compounds()$app_compound_id
    if (length(ids) == 0) {
      showNotification("Aucun etalon avec ces filtres.", type = "warning")
      return()
    }
    selected_compound_ids(unique(c(selected_compound_ids(), ids)))
  })

  observeEvent(input$remove_compound, {
    id <- input$compound_to_remove %||% ""
    if (!nzchar(id)) return()
    selected_compound_ids(setdiff(selected_compound_ids(), id))
  })

  observeEvent(input$clear_compounds, {
    selected_compound_ids(setdiff(selected_compound_ids(), selected_internal_standards()$app_compound_id))
  })

  observeEvent(input$open_manual_compound_dialog, {
    showModal(modalDialog(
      title = "Ajouter un etalon",
      textInput("manual_compound_name", "Nom"),
      selectInput("manual_compound_mode", "Mode", choices = c("Positif" = "pos", "Negatif" = "neg")),
      fluidRow(
        column(6, textInput("manual_compound_mz", "m/z exact", placeholder = "235.1477")),
        column(6, textInput("manual_compound_rt", "RT attendu (optionnel)", placeholder = "11.70"))
      ),
      fluidRow(
        column(6, textInput("manual_compound_dt", "DT attendu (optionnel)", placeholder = "3.24")),
        column(6, textInput("manual_compound_ccs", "CCS attendu (optionnel)", placeholder = "158.51"))
      ),
      checkboxInput("manual_compound_auto_select", "Ajouter aussi a la selection", value = TRUE),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_manual_compound", "Ajouter", icon = icon("plus"), class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_manual_compound, {
    raw <- data.frame(
      name = input$manual_compound_name %||% "",
      mode = input$manual_compound_mode %||% "",
      mz = input$manual_compound_mz %||% "",
      rt = input$manual_compound_rt %||% "",
      dt = input$manual_compound_dt %||% "",
      ccs = input$manual_compound_ccs %||% "",
      compound_type = "internal_standard",
      stringsAsFactors = FALSE
    )
    rows <- tryCatch(
      prepare_custom_compounds(
        raw,
        app_ids = next_custom_compound_ids(1),
        source_label = "Saisie manuelle",
        source_file = "Saisie manuelle"
      ),
      error = function(e) e
    )
    if (inherits(rows, "error")) {
      showNotification(conditionMessage(rows), type = "warning", duration = 8)
      return()
    }
    addition <- append_custom_compounds(rows)
    if (nrow(addition$rows) == 0) {
      showNotification("Cet etalon est deja present dans la liste.", type = "warning", duration = 8)
      return()
    }
    if (isTRUE(input$manual_compound_auto_select)) {
      selected_compound_ids(unique(c(selected_compound_ids(), addition$rows$app_compound_id)))
    }
    removeModal()
    showNotification("Etalon ajoute a la liste.", type = "message", duration = 6)
  })

  observeEvent(input$import_compounds_csv, {
    uploaded <- input$import_compounds_csv
    if (is.null(uploaded) || is.null(uploaded$datapath) || !nzchar(uploaded$datapath)) {
      return()
    }
    raw <- tryCatch(read_compounds_csv(uploaded$datapath), error = function(e) e)
    if (inherits(raw, "error")) {
      showNotification(conditionMessage(raw), type = "warning", duration = 10)
      return()
    }
    raw$compound_type <- "internal_standard"
    source_name <- uploaded$name %||% "CSV importe"
    rows <- tryCatch(
      prepare_custom_compounds(
        raw,
        app_ids = next_custom_compound_ids(nrow(raw)),
        source_label = paste0("CSV: ", source_name),
        source_file = source_name
      ),
      error = function(e) e
    )
    if (inherits(rows, "error")) {
      showNotification(conditionMessage(rows), type = "warning", duration = 10)
      return()
    }
    addition <- append_custom_compounds(rows)
    if (nrow(addition$rows) == 0) {
      showNotification("Aucun etalon ajoute : tous sont deja presents.", type = "warning", duration = 8)
      return()
    }
    if (isTRUE(input$import_compounds_auto_select)) {
      selected_compound_ids(unique(c(selected_compound_ids(), addition$rows$app_compound_id)))
    }
    message <- paste0(nrow(addition$rows), " etalon(s) importe(s).")
    if (addition$skipped > 0) {
      message <- paste0(message, " ", addition$skipped, " doublon(s) ignore(s).")
    }
    showNotification(message, type = "message", duration = 8)
  }, ignoreInit = TRUE)

  observeEvent(input$remove_selected_custom_compounds, {
    rows <- input$compounds_table_rows_selected %||% integer()
    candidates <- filtered_compounds()
    ids <- candidates$app_compound_id[rows]
    catalog <- compound_catalog()
    catalog_types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    custom_ids <- catalog$app_compound_id[
      catalog$source_label != "Liste fournie" & catalog_types == "internal_standard"
    ]
    ids <- intersect(ids[!is.na(ids)], custom_ids)
    if (length(ids) == 0) {
      showNotification("Selectionne au moins un ajout manuel ou CSV a supprimer.", type = "warning", duration = 8)
      return()
    }
    compound_catalog(catalog[!catalog$app_compound_id %in% ids, , drop = FALSE])
    selected_compound_ids(setdiff(selected_compound_ids(), ids))
    showNotification(paste0(length(ids), " ajout(s) supprime(s)."), type = "message", duration = 6)
  })

  observeEvent(input$reset_custom_compounds, {
    catalog <- compound_catalog()
    catalog_types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    custom_count <- sum(catalog$source_label != "Liste fournie" & catalog_types == "internal_standard")
    if (custom_count == 0) {
      showNotification("La liste ne contient aucun ajout de session.", type = "message", duration = 6)
      return()
    }
    reset_custom_compounds("internal_standard")
    showNotification("Retour a la liste fournie.", type = "message", duration = 6)
  })

  output$download_compounds_template <- downloadHandler(
    filename = function() "modele_etalons.csv",
    content = function(file) {
      template <- data.frame(
        name = "exemple-etalon",
        mode = "pos",
        mz = 235.1477,
        rt = 11.7,
        dt = 3.24,
        ccs = 158.51,
        compound_type = "internal_standard",
        stringsAsFactors = FALSE
      )
      utils::write.csv2(sanitize_csv_for_export(template), file, row.names = FALSE, na = "")
    }
  )

  output$download_compounds_catalog <- downloadHandler(
    filename = function() {
      paste0("liste_etalons_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      export <- filter_compounds_by_type(compound_catalog(), "internal_standard")
      export$app_compound_id <- NULL
      utils::write.csv2(sanitize_csv_for_export(export), file, row.names = FALSE, na = "")
    }
  )

  observeEvent(input$add_selected_suspects, {
    rows <- input$suspects_table_rows_selected %||% integer()
    ids <- filtered_suspects()$app_compound_id[rows]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) {
      showNotification("Aucun suspect selectionne.", type = "warning")
      return()
    }
    selected_compound_ids(unique(c(selected_compound_ids(), ids)))
  })

  observeEvent(input$add_filtered_suspects, {
    ids <- filtered_suspects()$app_compound_id
    if (length(ids) == 0) {
      showNotification("Aucun suspect avec ces filtres.", type = "warning")
      return()
    }
    selected_compound_ids(unique(c(selected_compound_ids(), ids)))
  })

  observeEvent(input$create_test_suspects_from_internal_standards, {
    standards <- selected_internal_standards()
    if (nrow(standards) == 0) {
      showNotification("Ajoute d'abord un ou plusieurs etalons internes a leur selection.", type = "warning", duration = 8)
      return()
    }
    rows <- tryCatch(
      make_test_suspects_from_internal_standards(
        standards,
        app_ids = next_custom_compound_ids(nrow(standards))
      ),
      error = function(e) e
    )
    if (inherits(rows, "error")) {
      showNotification(conditionMessage(rows), type = "warning", duration = 8)
      return()
    }
    addition <- append_custom_compounds(rows)
    if (nrow(addition$rows) == 0) {
      showNotification("Les suspects de test correspondants sont deja presents.", type = "message", duration = 6)
      return()
    }
    selected_compound_ids(unique(c(selected_compound_ids(), addition$rows$app_compound_id)))
    message <- paste0(nrow(addition$rows), " suspect(s) de test cree(s) depuis les etalons internes.")
    if (addition$skipped > 0) {
      message <- paste0(message, " ", addition$skipped, " doublon(s) ignore(s).")
    }
    showNotification(message, type = "message", duration = 8)
  })

  observeEvent(input$remove_suspect, {
    id <- input$suspect_to_remove %||% ""
    if (!nzchar(id)) return()
    selected_compound_ids(setdiff(selected_compound_ids(), id))
  })

  observeEvent(input$clear_suspects, {
    selected_compound_ids(setdiff(selected_compound_ids(), selected_suspects()$app_compound_id))
  })

  observeEvent(input$open_manual_suspect_dialog, {
    showModal(modalDialog(
      title = "Ajouter un suspect",
      textInput("manual_suspect_name", "Nom"),
      selectInput("manual_suspect_mode", "Mode", choices = c("Positif" = "pos", "Negatif" = "neg")),
      fluidRow(
        column(6, textInput("manual_suspect_mz", "m/z exact", placeholder = "235.1477")),
        column(6, textInput("manual_suspect_rt", "RT attendu (optionnel)", placeholder = "11.70"))
      ),
      fluidRow(
        column(6, textInput("manual_suspect_dt", "DT attendu (optionnel)", placeholder = "3.24")),
        column(6, textInput("manual_suspect_ccs", "CCS attendu (optionnel)", placeholder = "158.51"))
      ),
      checkboxInput("manual_suspect_auto_select", "Ajouter aussi a la selection", value = TRUE),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_manual_suspect", "Ajouter", icon = icon("plus"), class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_manual_suspect, {
    raw <- data.frame(
      name = input$manual_suspect_name %||% "",
      mode = input$manual_suspect_mode %||% "",
      mz = input$manual_suspect_mz %||% "",
      rt = input$manual_suspect_rt %||% "",
      dt = input$manual_suspect_dt %||% "",
      ccs = input$manual_suspect_ccs %||% "",
      compound_type = "suspect",
      stringsAsFactors = FALSE
    )
    rows <- tryCatch(
      prepare_custom_compounds(
        raw,
        app_ids = next_custom_compound_ids(1),
        source_label = "Saisie manuelle - suspect",
        source_file = "Saisie manuelle - suspect",
        default_compound_type = "suspect"
      ),
      error = function(e) e
    )
    if (inherits(rows, "error")) {
      showNotification(conditionMessage(rows), type = "warning", duration = 8)
      return()
    }
    addition <- append_custom_compounds(rows)
    if (nrow(addition$rows) == 0) {
      showNotification("Ce suspect est deja present dans la liste.", type = "warning", duration = 8)
      return()
    }
    if (isTRUE(input$manual_suspect_auto_select)) {
      selected_compound_ids(unique(c(selected_compound_ids(), addition$rows$app_compound_id)))
    }
    removeModal()
    showNotification("Suspect ajoute a la liste.", type = "message", duration = 6)
  })

  observeEvent(input$import_suspects_csv, {
    uploaded <- input$import_suspects_csv
    if (is.null(uploaded) || is.null(uploaded$datapath) || !nzchar(uploaded$datapath)) {
      return()
    }
    raw <- tryCatch(read_compounds_csv(uploaded$datapath), error = function(e) e)
    if (inherits(raw, "error")) {
      showNotification(conditionMessage(raw), type = "warning", duration = 10)
      return()
    }
    raw$compound_type <- "suspect"
    source_name <- uploaded$name %||% "CSV importe"
    rows <- tryCatch(
      prepare_custom_compounds(
        raw,
        app_ids = next_custom_compound_ids(nrow(raw)),
        source_label = paste0("CSV suspect: ", source_name),
        source_file = source_name,
        default_compound_type = "suspect"
      ),
      error = function(e) e
    )
    if (inherits(rows, "error")) {
      showNotification(conditionMessage(rows), type = "warning", duration = 10)
      return()
    }
    addition <- append_custom_compounds(rows)
    if (nrow(addition$rows) == 0) {
      showNotification("Aucun suspect ajoute : tous sont deja presents.", type = "warning", duration = 8)
      return()
    }
    if (isTRUE(input$import_suspects_auto_select)) {
      selected_compound_ids(unique(c(selected_compound_ids(), addition$rows$app_compound_id)))
    }
    message <- paste0(nrow(addition$rows), " suspect(s) importe(s).")
    if (addition$skipped > 0) {
      message <- paste0(message, " ", addition$skipped, " doublon(s) ignore(s).")
    }
    showNotification(message, type = "message", duration = 8)
  }, ignoreInit = TRUE)

  observeEvent(input$remove_selected_custom_suspects, {
    rows <- input$suspects_table_rows_selected %||% integer()
    candidates <- filtered_suspects()
    ids <- candidates$app_compound_id[rows]
    catalog <- compound_catalog()
    catalog_types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    custom_ids <- catalog$app_compound_id[
      catalog$source_label != "Liste fournie" & catalog_types == "suspect"
    ]
    ids <- intersect(ids[!is.na(ids)], custom_ids)
    if (length(ids) == 0) {
      showNotification("Selectionne au moins un ajout manuel ou CSV a supprimer.", type = "warning", duration = 8)
      return()
    }
    compound_catalog(catalog[!catalog$app_compound_id %in% ids, , drop = FALSE])
    selected_compound_ids(setdiff(selected_compound_ids(), ids))
    showNotification(paste0(length(ids), " ajout(s) supprime(s)."), type = "message", duration = 6)
  })

  observeEvent(input$reset_custom_suspects, {
    catalog <- compound_catalog()
    catalog_types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    custom_count <- sum(catalog$source_label != "Liste fournie" & catalog_types == "suspect")
    if (custom_count == 0) {
      showNotification("La liste ne contient aucun ajout de session.", type = "message", duration = 6)
      return()
    }
    reset_custom_compounds("suspect")
    showNotification("Ajouts suspects effaces.", type = "message", duration = 6)
  })

  output$download_suspects_template <- downloadHandler(
    filename = function() "modele_suspects.csv",
    content = function(file) {
      template <- data.frame(
        name = "exemple-suspect",
        mode = "pos",
        mz = 235.1477,
        rt = 11.7,
        dt = NA_real_,
        ccs = NA_real_,
        compound_type = "suspect",
        stringsAsFactors = FALSE
      )
      utils::write.csv2(sanitize_csv_for_export(template), file, row.names = FALSE, na = "")
    }
  )

  output$download_suspects_catalog <- downloadHandler(
    filename = function() {
      paste0("liste_suspects_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      export <- filter_compounds_by_type(compound_catalog(), "suspect")
      export$app_compound_id <- NULL
      utils::write.csv2(sanitize_csv_for_export(export), file, row.names = FALSE, na = "")
    }
  )

  output$n_files <- renderText(nrow(filtered_files()))
  output$n_samples <- renderText(sum(!filtered_files()$is_blank))
  output$n_blanks <- renderText(sum(filtered_files()$is_blank))
  output$n_modes <- renderText(length(unique(filtered_files()$mode_dir)))

  output$files_table <- renderDT({
    x <- filtered_files()[, c(
      "app_file_id",
      "parquet_relative_path",
      "mode_dir",
      "file_type",
      "reference_year",
      "reference_month",
      "site",
      "duplicate_label",
      "replicate_label",
      "acquisition_start_time"
    ), drop = FALSE]
    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "multiple",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
  }, server = FALSE)

  output$selected_files_table <- renderDT({
    x <- selected_files()
    x$resume <- file_selection_label(x)
    x <- x[, c(
      "resume",
      "sample_name",
      "parquet_relative_path",
      "acquisition_start_time"
    ), drop = FALSE]
    datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  output$files_plot <- renderPlot({
    x <- filtered_files()
    if (nrow(x) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucun fichier")
      return()
    }
    counts <- table(x$mode_dir, x$file_type)
    barplot(
      counts,
      beside = TRUE,
      col = c("#215a6d", "#c97b3d"),
      border = NA,
      ylab = "Nombre de fichiers",
      xlab = "Type",
      legend.text = TRUE,
      args.legend = list(x = "topright", bty = "n")
    )
  })

  output$n_compounds <- renderText(nrow(filtered_compounds()))
  output$n_compounds_pos <- renderText(sum(filtered_compounds()$mode == "pos"))
  output$n_compounds_neg <- renderText(sum(filtered_compounds()$mode == "neg"))
  output$n_custom_compounds <- renderText({
    catalog <- compound_catalog()
    types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    sum(catalog$source_label != "Liste fournie" & types == "internal_standard")
  })
  output$n_suspects <- renderText(nrow(filtered_suspects()))
  output$n_suspects_pos <- renderText(sum(filtered_suspects()$mode == "pos"))
  output$n_suspects_neg <- renderText(sum(filtered_suspects()$mode == "neg"))
  output$n_custom_suspects <- renderText({
    catalog <- compound_catalog()
    types <- normalize_compound_type(catalog$compound_type, default = NA_character_)
    sum(catalog$source_label != "Liste fournie" & types == "suspect")
  })

  output$compounds_table <- renderDT({
    x <- filtered_compounds()[, c(
      "app_compound_id",
      "compound_id",
      "source_label",
      "name",
      "mode",
      "mz",
      "rt",
      "dt",
      "ccs",
      "compound_type"
    ), drop = FALSE]
    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "multiple",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
  }, server = FALSE)

  output$selected_compounds_table <- renderDT({
    x <- selected_internal_standards()[, c(
      "compound_id",
      "source_label",
      "name",
      "mode",
      "mz",
      "rt",
      "dt",
      "ccs",
      "compound_type"
    ), drop = FALSE]
    datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  output$suspects_table <- renderDT({
    x <- filtered_suspects()[, c(
      "app_compound_id",
      "compound_id",
      "source_label",
      "name",
      "mode",
      "mz",
      "rt",
      "dt",
      "ccs",
      "compound_type"
    ), drop = FALSE]
    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "multiple",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
  }, server = FALSE)

  output$selected_suspects_table <- renderDT({
    x <- selected_suspects()[, c(
      "compound_id",
      "source_label",
      "name",
      "mode",
      "mz",
      "rt",
      "dt",
      "ccs",
      "compound_type"
    ), drop = FALSE]
    datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  output$nextcloud_status <- renderText(nextcloud_connection_status())

  output$nextcloud_current_path <- renderText({
    path <- nextcloud_current_path()
    if (!nzchar(path)) "Dossier: /" else paste0("Dossier: /", path)
  })

  output$nextcloud_items_count <- renderText(nrow(nextcloud_contents()))
  output$nextcloud_selected_count <- renderText(nrow(nextcloud_selected_files()))
  output$nextcloud_catalog_count <- renderText(sum(parquet_files()$source_type == "nextcloud"))

  output$nextcloud_contents_table <- renderDT({
    x <- nextcloud_contents()
    if (nrow(x) == 0) {
      return(datatable(
        data.frame(message = "Aucun element", stringsAsFactors = FALSE),
        rownames = FALSE,
        selection = "none",
        options = list(dom = "t")
      ))
    }
    path_modes <- vapply(x$path, mode_from_parquet_path, character(1))
    display <- data.frame(
      type = ifelse(x$is_folder, "Dossier", ifelse(grepl("\\.parquet$", x$name, ignore.case = TRUE), "Parquet", "Fichier")),
      mode = ifelse(vapply(path_modes, valid_mode, logical(1)), toupper(path_modes), "-"),
      nom = x$name,
      taille = vapply(x$size, format_bytes, character(1)),
      modifie = x$modified,
      stringsAsFactors = FALSE
    )
    datatable(
      display,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 18, scrollX = TRUE)
    )
  }, server = FALSE)

  output$nextcloud_selected_table <- renderDT({
    x <- nextcloud_selected_files()
    if (nrow(x) == 0) {
      x <- data.frame(
        chemin = character(),
        mode = character(),
        source_mode = character(),
        taille = character(),
        modifie = character(),
        stringsAsFactors = FALSE
      )
    } else {
      catalog <- parquet_files()
      ids <- paste0("nextcloud::", normalize_relative_path(x$path))
      match_index <- match(ids, catalog$parquet_id)
      path_modes <- vapply(x$path, mode_from_parquet_path, character(1))
      modes <- path_modes
      sources <- ifelse(vapply(path_modes, valid_mode, logical(1)), "Chemin", "Inconnu")
      matched <- !is.na(match_index)
      modes[matched] <- catalog$parquet_mode[match_index[matched]]
      sources[matched] <- catalog$mode_source[match_index[matched]]
      x <- data.frame(
        chemin = x$path,
        mode = ifelse(vapply(modes, valid_mode, logical(1)), toupper(modes), "Inconnu"),
        source_mode = sources,
        taille = vapply(x$size, format_bytes, character(1)),
        modifie = x$modified,
        stringsAsFactors = FALSE
      )
    }
    datatable(x, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
  })

  output$parquet_data_path <- renderText({
    root_name <- basename(normalizePath(parquet_root, mustWork = FALSE))
    local_count <- sum(parquet_files()$source_type == "local")
    status <- if (!dir.exists(parquet_root)) {
      "Dossier local introuvable"
    } else if (local_count == 0) {
      "Aucun Parquet local detecte"
    } else {
      paste0(local_count, " Parquet local(aux) detecte(s)")
    }
    paste0(
      "Dossier de donnees local : ", if (nzchar(root_name)) root_name else "configure",
      "\n", status
    )
  })

  output$parquet_file_context <- renderText({
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) {
      return("Aucun fichier selectionne")
    }
    context <- selected_parquet_context()
    mode_label <- if (valid_mode(context$mode)) context$mode else "inconnu"
    metadata_label <- if (isTRUE(context$metadata_match)) "JSON associe" else "JSON non associe"
    source_label <- if (identical(selected$source_type[[1]], "nextcloud")) "Nextcloud" else "Local"
    paste(
      paste0("Source: ", source_label),
      paste0("Mode: ", mode_label, " (", context$source, ")"),
      paste0("Metadonnees: ", metadata_label),
      sep = "\n"
    )
  })

  output$eic_compound_details <- renderText({
    compound <- selected_eic_compound()
    if (nrow(compound) == 0) {
      return("Recherche libre : saisir un m/z ci-dessous")
    }
    paste(
      paste0("Nom: ", compound$name[[1]]),
      paste0("Mode: ", compound$mode[[1]]),
      paste0("m/z: ", compound$mz[[1]]),
      paste0("RT: ", compound$rt[[1]]),
      paste0("DT: ", compound$dt[[1]]),
      paste0("CCS: ", compound$ccs[[1]]),
      sep = "\n"
    )
  })

  output$quick_eic_status <- renderText({
    result <- quick_eic_result()
    if (is.null(result)) {
      return("Aucun apercu EIC rapide calcule")
    }
    result$message
  })

  output$n_parquet_files <- renderText(nrow(parquet_files()))

  output$parquet_selected_size <- renderText({
    selected <- selected_parquet_file()
    if (nrow(selected) == 0) return("-")
    selected$size_label[[1]]
  })

  output$parquet_loaded_rows <- renderText({
    info <- parquet_info()
    if (is.null(info)) return("-")
    format(info$summary$rows[[1]], big.mark = " ", scientific = FALSE)
  })

  output$parquet_loaded_columns <- renderText({
    info <- parquet_info()
    if (is.null(info)) return("-")
    nrow(info$schema)
  })

  output$parquet_files_table <- renderDT({
    x <- parquet_files()[, c(
      "source_type", "relative_path", "parquet_mode", "mode_source", "reference_year",
      "reference_month", "file_type", "duplicate_label", "replicate_label",
      "metadata_match", "size_label", "modified"
    ), drop = FALSE]
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })

  output$parquet_schema_table <- renderDT({
    info <- parquet_info()
    x <- if (is.null(info)) {
      data.frame(column = character(), type = character(), stringsAsFactors = FALSE)
    } else {
      info$schema
    }
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  output$parquet_ms_table <- renderDT({
    info <- parquet_info()
    x <- if (is.null(info)) {
      data.frame(mslevel = character(), rows = numeric(), stringsAsFactors = FALSE)
    } else {
      info$ms_levels
    }
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 8))
  })

  output$parquet_summary_table <- renderDT({
    info <- parquet_info()
    x <- if (is.null(info)) {
      data.frame(metric = character(), value = character(), stringsAsFactors = FALSE)
    } else {
      summary <- info$summary[1, , drop = FALSE]
      data.frame(
        metric = names(summary),
        value = vapply(summary, function(value) as.character(value[[1]]), character(1)),
        stringsAsFactors = FALSE
      )
    }
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  output$parquet_preview_table <- renderDT({
    info <- parquet_info()
    x <- if (is.null(info)) data.frame() else info$preview
    datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  output$tic_plot <- plotly::renderPlotly({
    plot_chromatogram(tic_data(), "TIC")
  })

  output$bpi_plot <- plotly::renderPlotly({
    plot_chromatogram(bpi_data(), "BPI")
  })

  output$eic_plot <- plotly::renderPlotly({
    context <- eic_context()
    compound <- selected_eic_compound()
    target_mz <- if (is.null(context)) {
      input_number(input$eic_target_mz, if (nrow(compound) > 0) compound$mz[[1]] else NA_real_)
    } else {
      context$target_mz
    }
    mz_tolerance <- if (is.null(context)) input_number(input$eic_mz_tolerance, 0.01) else context$mz_tolerance
    compound_label <- if (is.null(context)) eic_context_label(compound, target_mz) else context$label
    title <- paste0("EIC ", compound_label, " (m/z ", target_mz, " +/- ", mz_tolerance, ")")
    plot_chromatogram(eic_data(), title)
  })

  output$tic_summary_table <- renderDT({
    datatable(summarise_chromatogram(tic_data()), rownames = FALSE, options = list(dom = "t", pageLength = 8))
  })

  output$bpi_summary_table <- renderDT({
    datatable(summarise_chromatogram(bpi_data()), rownames = FALSE, options = list(dom = "t", pageLength = 8))
  })

  output$eic_summary_table <- renderDT({
    x <- eic_summary()
    if (is.null(x)) {
      x <- data.frame(
        status = "Non calcule",
        max_intensity = NA_real_,
        rt_at_max = NA_real_,
        n_scans = 0,
        area_sum = NA_real_,
        rt_window_used = NA,
        rt_match = NA,
        max_intensity_total = NA_real_,
        rt_at_global_max = NA_real_,
        n_scans_total = 0,
        area_sum_total = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 8, scrollX = TRUE))
  })

  output$eic_table <- renderDT({
    x <- eic_data()
    if (is.null(x)) {
      x <- empty_table(
        scanid = integer(),
        rt = numeric(),
        intensity = numeric(),
        n_points = integer(),
        min_mz = numeric(),
        max_mz = numeric(),
        observed_mz = numeric(),
        observed_dt = numeric()
      )
    }
    datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$ms2_spectrum_plot <- plotly::renderPlotly({
    context <- ms2_spectrum_context()
    title <- "Spectre MS2 brut (fenetre RT)"
    if (!is.null(context)) {
      title <- paste0(
        "Spectre MS2 ", context$compound_label,
        " (RT ", format(context$expected_rt, trim = TRUE),
        " +/- ", format(context$rt_tolerance, trim = TRUE), " min)"
      )
    }
    plot_ms2_spectrum(ms2_spectrum_data(), title, comparison = ms2_reference_comparison())
  })

  output$ms2_spectrum_table <- renderDT({
    x <- ms2_spectrum_data()
    if (is.null(x)) {
      x <- empty_table(
        mz = numeric(),
        intensity = numeric(),
        max_intensity = numeric(),
        n_points = integer()
      )
    }
    x <- as.data.frame(x)
    names(x) <- c("m/z", "Intensite agregee", "Intensite max", "Nombre de points")
    datatable(x, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$ms2_reference_library_status <- renderText({
    references <- ms2_reference_summary(ms2_reference_catalog())
    if (nrow(references) == 0) {
      return("Aucun spectre de reference charge")
    }
    paste0(nrow(references), " spectre(s) de reference, ", sum(references$fragments), " fragment(s)")
  })

  output$download_ms2_reference_template <- downloadHandler(
    filename = function() "modele_spectres_ms2.csv",
    content = function(file) {
      utils::write.csv2(ms2_reference_template(), file, row.names = FALSE, na = "")
    }
  )

  output$ms2_comparison_summary_table <- renderDT({
    comparison <- ms2_reference_comparison()
    summary <- if (is.null(comparison)) {
      empty_ms2_comparison_summary("Non calcule")
    } else {
      comparison$summary
    }
    x <- data.frame(
      "Statut technique" = summary$technical_status,
      "Reference" = summary$compound_name,
      "Mode" = summary$mode,
      "Fragments ref." = summary$reference_fragments,
      "Fragments concordants" = summary$matched_fragments,
      "Couverture (%)" = summary$fragment_coverage_pct,
      "Couverture ponderee (%)" = summary$weighted_coverage_pct,
      "Score cosinus" = summary$cosine_similarity,
      "Signal observe explique (%)" = summary$observed_signal_fraction_pct,
      "Tolerance m/z" = summary$mz_tolerance,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    datatable(x, rownames = FALSE, options = list(dom = "t", pageLength = 8, scrollX = TRUE))
  })

  output$ms2_comparison_table <- renderDT({
    comparison <- ms2_reference_comparison()
    matches <- if (is.null(comparison)) empty_ms2_reference_matches() else comparison$matches
    x <- data.frame(
      "m/z reference" = matches$reference_fragment_mz,
      "Intensite relative ref. (%)" = matches$reference_relative_intensity,
      "m/z observe" = matches$observed_fragment_mz,
      "Intensite observee" = matches$observed_intensity,
      "Erreur (Da)" = matches$mz_error_da,
      "Erreur (ppm)" = matches$mz_error_ppm,
      "Concordant" = ifelse(matches$matched, "Oui", "Non"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    table <- datatable(x, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
    formatStyle(
      table,
      "Concordant",
      target = "cell",
      backgroundColor = styleEqual(c("Oui", "Non"), c("#1e8f4d", "#c83a32")),
      color = styleEqual(c("Oui", "Non"), c("white", "white")),
      fontWeight = "bold"
    )
  })

  output$screening_detected_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    sum(x$is_detected %in% TRUE, na.rm = TRUE)
  })

  output$screening_not_detected_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    sum(x$confidence_level == 0, na.rm = TRUE)
  })

  output$screening_level_one_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    sum(x$confidence_level == 1, na.rm = TRUE)
  })

  output$screening_level_two_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    sum(x$confidence_level == 2, na.rm = TRUE)
  })

  output$screening_level_three_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    sum(x$confidence_level == 3, na.rm = TRUE)
  })

  output$screening_total_count <- renderText({
    x <- screening_results()
    if (is.null(x)) return("-")
    nrow(x)
  })

  output$screening_results_table <- renderDT({
    x <- screening_results()
    if (is.null(x)) {
      x <- empty_table(
        file = character(),
        compound_name = character(),
        mode = character(),
        status = character(),
        confidence_level = integer(),
        confidence_label = character(),
        is_detected = logical(),
        minimum_confidence_level = integer(),
        expected_mz = numeric(),
        observed_mz = numeric(),
        mz_error_da = numeric(),
        mz_error_ppm = numeric(),
        mz_match = logical(),
        expected_rt = numeric(),
        max_intensity = numeric(),
        rt_max_intensity = numeric(),
        rt_at_max = numeric(),
        rt_error = numeric(),
        rt_match = logical(),
        rt_intensity_match = logical(),
        expected_dt = numeric(),
        observed_dt = numeric(),
        dt_error_pct = numeric(),
        dt_match = logical(),
        dt_status = character(),
        expected_ccs = numeric(),
        observed_ccs = numeric(),
        ccs_error_pct = numeric(),
        ccs_match = logical(),
        ccs_status = character(),
        expected_dt_from_ccs = numeric(),
        ccs_calibration_c1 = numeric(),
        ccs_calibration_c2 = numeric(),
        ccs_to_dt_error_ms = numeric(),
        ccs_to_dt_error_pct = numeric(),
        ccs_to_dt_match = logical(),
        ccs_to_dt_status = character(),
        n_scans = integer(),
        area_sum = numeric()
      )
    } else {
      keep <- c(
        "file",
        "file_relative_path",
        "file_mode",
        "compound_name",
        "mode",
        "status",
        "confidence_level",
        "confidence_label",
        "is_detected",
        "minimum_confidence_level",
        "expected_mz",
        "observed_mz",
        "expected_rt",
        "mz_tolerance",
        "mz_error_da",
        "mz_error_ppm",
        "mz_match",
        "rt_tolerance",
        "max_intensity",
        "rt_max_intensity",
        "rt_at_max",
        "rt_error",
        "rt_match",
        "rt_intensity_match",
        "rt_window_used",
        "rt_window_available",
        "intensity_match",
        "expected_dt",
        "observed_dt",
        "dt_tolerance_pct",
        "dt_error",
        "dt_error_pct",
        "dt_match",
        "dt_status",
        "expected_ccs",
        "observed_ccs",
        "ccs_tolerance_pct",
        "ccs_error",
        "ccs_error_pct",
        "ccs_match",
        "ccs_status",
        "expected_dt_from_ccs",
        "ccs_calibration_c1",
        "ccs_calibration_c2",
        "ccs_to_dt_error_ms",
        "ccs_to_dt_error_pct",
        "ccs_to_dt_match",
        "ccs_to_dt_status",
        "use_ccs",
        "n_scans",
        "rt_n_scans",
        "area_sum",
        "rt_area_sum",
        "max_intensity_total",
        "rt_at_global_max",
        "n_scans_total",
        "area_sum_total",
        "error"
      )
      x <- x[, intersect(keep, names(x)), drop = FALSE]
    }
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE)) |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#1f8f4d", "#c9342f", "#a66b00")
        ),
        color = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#ffffff", "#ffffff", "#ffffff")
        ),
        fontWeight = "bold"
      ) |>
      formatStyle(
        "confidence_label",
        backgroundColor = styleEqual(
          c(
            "Preuve 0 - aucun signal",
            "Preuve 1 - m/z",
            "Preuve 2 - m/z + RT",
            "Preuve 3 - m/z + RT + mobilite",
            "Erreur"
          ),
          c("#fff1f0", "#fff8e1", "#e8f4f8", "#eef8f1", "#fff8e1")
        ),
        fontWeight = "bold"
      )
  })

  output$download_screening_results <- downloadHandler(
    filename = function() {
      paste0("screening_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      results <- screening_results()
      req(!is.null(results))
      export <- results
      export$file_path <- NULL
      utils::write.csv(sanitize_csv_for_export(export), file, row.names = FALSE, na = "")
    }
  )

  screening_files <- reactive({
    x <- selected_screening_sources()
    screen_mode <- input$screen_mode %||% ""
    if (nzchar(screen_mode)) x <- x[x$parquet_mode == screen_mode, , drop = FALSE]
    if (!isTRUE(input$screen_include_blanks)) {
      is_blank <- !is.na(x$file_type) & x$file_type == "Blanc"
      x <- x[!is_blank, , drop = FALSE]
    }
    x
  })

  screening_compounds <- reactive({
    x <- selected_compounds()
    screen_mode <- input$screen_mode %||% ""
    if (nzchar(screen_mode)) x <- x[x$mode == screen_mode, , drop = FALSE]
    x
  })

  screening_internal_standards <- reactive({
    filter_compounds_by_type(screening_compounds(), "internal_standard")
  })

  screening_suspects <- reactive({
    filter_compounds_by_type(screening_compounds(), "suspect")
  })

  batch_parameters <- reactive({
    list(
      mz_tolerance = input_number(input$screen_mz_tol, 0.01),
      rt_tolerance = input_number(input$screen_batch_rt_tol, 0.5),
      mslevel = as.character(input$screen_batch_mslevel %||% "1"),
      min_intensity = input_number(input$screen_min_intensity, 1000),
      compute_total_intensity = isTRUE(input$screen_batch_compute_total_intensity),
      require_rt_match = isTRUE(input$screen_batch_require_rt),
      use_dt = isTRUE(input$screen_batch_use_dt),
      dt_tolerance_pct = input_number(input$screen_batch_dt_tolerance_pct, 10),
      use_ccs = isTRUE(input$screen_batch_use_ccs),
      ccs_tolerance_pct = input_number(input$screen_batch_ccs_tolerance_pct, 10)
    )
  })

  screening_plan <- reactive({
    files <- screening_files()
    compounds <- screening_compounds()
    if (nrow(files) == 0 || nrow(compounds) == 0) {
      return(data.frame())
    }
    plan <- merge(files, compounds, by.x = "parquet_mode", by.y = "mode", all = FALSE)
    plan <- plan[, c(
      "parquet_id",
      "source_type",
      "relative_path",
      "parquet_mode",
      "reference_year",
      "reference_month",
      "file_type",
      "duplicate_label",
      "replicate_label",
      "sample_name",
      "has_ccs_calibration",
      "ccs_calibration_c1",
      "ccs_calibration_c2",
      "compound_id",
      "name",
      "compound_type",
      "mz",
      "rt",
      "dt",
      "ccs"
    ), drop = FALSE]
    parameters <- batch_parameters()
    plan$mz_tolerance <- parameters$mz_tolerance
    plan$rt_tolerance <- parameters$rt_tolerance
    plan$mslevel <- parameters$mslevel
    plan$min_intensity <- parameters$min_intensity
    plan$compute_total_intensity <- parameters$compute_total_intensity
    plan$require_rt_match <- parameters$require_rt_match
    plan$use_dt <- parameters$use_dt
    plan$dt_tolerance_pct <- parameters$dt_tolerance_pct
    plan$use_ccs <- parameters$use_ccs
    plan$ccs_tolerance_pct <- parameters$ccs_tolerance_pct
    plan
  })

  batch_execution_files <- reactive({
    files <- screening_files()
    plan <- screening_plan()
    if (nrow(plan) == 0) {
      return(files[0, , drop = FALSE])
    }
    ids <- unique(plan$parquet_id)
    files[match(ids, files$parquet_id), , drop = FALSE]
  })

  output$plan_source_selection_count <- renderText(nrow(selected_screening_sources()))
  output$plan_files <- renderText(nrow(batch_execution_files()))
  output$plan_compounds <- renderText(nrow(screening_compounds()))
  output$plan_internal_standards <- renderText(nrow(screening_internal_standards()))
  output$plan_suspects <- renderText(nrow(screening_suspects()))
  output$plan_queries <- renderText(nrow(screening_plan()))
  output$plan_modes <- renderText(length(unique(screening_plan()$parquet_mode)))

  output$screening_source_catalog_table <- renderDT({
    x <- filtered_screening_sources()
    x <- x[, c(
      "source_type", "relative_path", "parquet_mode", "mode_source", "reference_year",
      "reference_month", "file_type", "duplicate_label", "replicate_label", "has_ccs_calibration",
      "metadata_match", "size_label", "modified"
    ), drop = FALSE]
    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "multiple",
      options = list(pageLength = 12, scrollX = TRUE)
    )
  }, server = FALSE)

  output$selected_screening_sources_table <- renderDT({
    x <- selected_screening_sources()
    x <- x[, c(
      "source_type", "relative_path", "parquet_mode", "mode_source", "reference_year",
      "reference_month", "file_type", "duplicate_label", "replicate_label",
      "metadata_match", "size_label"
    ), drop = FALSE]
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  output$screening_plan_table <- renderDT({
    datatable(screening_plan(), rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE))
  })

  batch_preflight <- function() {
    parameters <- batch_parameters()
    validate_batch_parameters(parameters)
    files <- batch_execution_files()
    compounds <- screening_compounds()
    plan <- screening_plan()
    if (nrow(selected_screening_sources()) == 0) {
      stop("Ajoute au moins un fichier dans la selection du lot.")
    }
    if (nrow(compounds) == 0) {
      stop("Ajoute au moins une molecule dans les onglets Etalons internes ou Suspects.")
    }
    if (nrow(files) == 0 || nrow(plan) == 0) {
      stop("Aucune combinaison fichier x molecule compatible. Verifie les modes pos/neg et les filtres.")
    }
    if (any(files$source_type == "nextcloud") && is.null(nextcloud_session_config())) {
      stop("Reconnecte Nextcloud avant de lancer un lot contenant des fichiers distants.")
    }
    list(
      parameters = parameters,
      files = files,
      compounds = compounds,
      plan = plan,
      excluded_sources = nrow(screening_files()) - nrow(files)
    )
  }

  observeEvent(input$open_batch_screening_confirmation, {
    preflight <- tryCatch(batch_preflight(), error = function(e) e)
    if (inherits(preflight, "error")) {
      showNotification(conditionMessage(preflight), type = "warning", duration = 10)
      return()
    }

    excluded_notice <- if (preflight$excluded_sources > 0) {
      tags$p(
        class = "text-warning mb-0",
        paste0(preflight$excluded_sources, " fichier(s) selectionne(s) ne possedent pas de combinaison compatible et seront ignores.")
      )
    }
    showModal(modalDialog(
      title = "Lancer le screening du lot",
      tags$dl(
        tags$dt("Fichiers"), tags$dd(nrow(preflight$files)),
        tags$dt("Etalons internes"), tags$dd(nrow(screening_internal_standards())),
        tags$dt("Suspects"), tags$dd(nrow(screening_suspects())),
        tags$dt("Requetes"), tags$dd(nrow(preflight$plan)),
        tags$dt("Intensite totale MS"), tags$dd(if (isTRUE(preflight$parameters$compute_total_intensity)) "Calculee" else "Non calculee"),
        tags$dt("Execution"), tags$dd("Un fichier a la fois")
      ),
      excluded_notice,
      easyClose = FALSE,
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_batch_screening", "Lancer", icon = icon("play"), class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_batch_screening, {
    removeModal()
    preflight <- tryCatch(batch_preflight(), error = function(e) e)
    if (inherits(preflight, "error")) {
      showNotification(conditionMessage(preflight), type = "warning", duration = 10)
      return()
    }

    parameters <- preflight$parameters
    files <- preflight$files
    compounds <- preflight$compounds
    batch_screening_results(NULL)
    batch_screening_status("Lot en cours")

    completed <- tryCatch({
      completed_results <- withProgress(message = "Screening du lot", value = 0, {
        total_files <- nrow(files)
        file_results <- vector("list", total_files)
        for (index in seq_len(total_files)) {
          file <- files[index, , drop = FALSE]
          file_compounds <- compounds[compounds$mode == file$parquet_mode[[1]], , drop = FALSE]
          incProgress(
            0,
            detail = paste0("Fichier ", index, "/", total_files, " : ", batch_file_value(file, "relative_path", "Parquet"))
          )

          source <- tryCatch(batch_source_for_file(file), error = function(e) e)
          if (inherits(source, "error")) {
            file_results[[index]] <- batch_error_rows(
              file,
              file_compounds,
              batch_source_path_for_error(file),
              parameters,
              source
            )
          } else {
            total_signal <- NULL
            total_signal_error <- ""
            if (isTRUE(parameters$compute_total_intensity)) {
              total_signal_result <- tryCatch(
                suppressWarnings(compute_total_intensity(
                  source$path,
                  mslevel = parameters$mslevel,
                  http_headers = source$http_headers
                )),
                error = function(e) e
              )
              total_signal <- if (inherits(total_signal_result, "error")) {
                data.frame(total_intensity = NA_real_, n_points = NA_integer_, stringsAsFactors = FALSE)
              } else {
                as.data.frame(total_signal_result)
              }
              total_signal_error <- if (inherits(total_signal_result, "error")) {
                conditionMessage(total_signal_result)
              } else {
                ""
              }
            }
            result <- tryCatch(
              suppressWarnings(screen_compounds_in_file(
                source$path,
                file_compounds,
                mz_tolerance = parameters$mz_tolerance,
                rt_tolerance = parameters$rt_tolerance,
                mslevel = parameters$mslevel,
                min_intensity = parameters$min_intensity,
                require_rt_match = parameters$require_rt_match,
                use_dt = parameters$use_dt,
                dt_tolerance_pct = parameters$dt_tolerance_pct,
                use_ccs = parameters$use_ccs,
                ccs_tolerance_pct = parameters$ccs_tolerance_pct,
                ccs_calibration = ccs_calibration_for_file(file),
                ccs_to_drifttime = arcms_ccs_to_drifttime_converter(),
                http_headers = source$http_headers
              )),
              error = function(e) e
            )
            file_results[[index]] <- if (inherits(result, "error")) {
              batch_error_rows(file, file_compounds, source$path, parameters, result)
            } else {
              annotate_batch_results(
                as.data.frame(result),
                file,
                parameters,
                format(Sys.time(), tz = "UTC", usetz = TRUE),
                total_signal = total_signal,
                total_signal_error = total_signal_error
              )
            }
          }
          incProgress(1 / total_files)
        }
        do.call(rbind, file_results)
      })

      batch_screening_results(completed_results)
      errors <- sum(completed_results$status == "Error", na.rm = TRUE)
      batch_screening_status(paste0("Termine : ", nrow(files), " fichier(s), ", errors, " erreur(s)"))
      showNotification("Screening de lot termine.", type = "message", duration = 6)
      TRUE
    }, error = function(e) {
      batch_screening_status("Echec du lot")
      showNotification("Le screening de lot s'est arrete sur une erreur interne.", type = "error", duration = 10)
      FALSE
    })
    invisible(completed)
  })

  observeEvent(input$clear_batch_screening_results, {
    batch_screening_results(NULL)
    batch_screening_status("Aucun lot lance")
  })

  observeEvent(input$prepare_batch_result_eic, {
    parameters <- batch_result_eic_parameters(selected_batch_screening_result())
    if (!isTRUE(parameters$ok)) {
      showNotification(parameters$message, type = "warning", duration = 8)
      return()
    }

    available_files <- parquet_files()
    if (!parameters$parquet_id %in% available_files$parquet_id) {
      showNotification(
        "Le fichier de cette ligne n'est plus disponible dans le catalogue. Reconnecte Nextcloud ou ajoute le Parquet avant de preparer son EIC.",
        type = "warning",
        duration = 10
      )
      return()
    }

    catalog <- compound_catalog()
    compound_match <- match(parameters$compound_id, catalog$compound_id)
    selected_compound_id <- if (!is.na(compound_match)) catalog$app_compound_id[[compound_match]] else ""

    updateSelectInput(session, "parquet_file_id", selected = parameters$parquet_id)
    updateSelectInput(
      session,
      "parquet_mode_override",
      selected = if (valid_mode(parameters$file_mode)) parameters$file_mode else "auto"
    )
    updateSelectInput(session, "chrom_mslevel", selected = parameters$mslevel)
    updateSelectInput(session, "eic_compound_id", selected = selected_compound_id)
    updateNumericInput(session, "eic_target_mz", value = parameters$target_mz)
    updateNumericInput(session, "eic_mz_tolerance", value = parameters$mz_tolerance)
    updateNumericInput(session, "eic_expected_rt", value = parameters$expected_rt)
    updateNumericInput(session, "eic_rt_tolerance", value = parameters$rt_tolerance)
    if (is.finite(parameters$min_intensity)) {
      updateNumericInput(session, "eic_min_intensity", value = parameters$min_intensity)
    }
    updateNavbarPage(session, "main_nav", selected = "Parquet")

    rt_notice <- if (is.finite(parameters$expected_rt)) {
      ""
    } else {
      " La RT attendue est absente : renseigne-la avant le calcul."
    }
    showNotification(
      paste0(
        "Fichier et parametres EIC prepares pour ", parameters$compound_name,
        ". Clique sur 'Lire les infos', puis 'Calculer EIC'.", rt_notice
      ),
      type = "message",
      duration = 10
    )
  })

  observeEvent(input$follow_batch_result_molecule, {
    parameters <- batch_result_monitoring_parameters(selected_batch_screening_result())
    if (!isTRUE(parameters$ok)) {
      showNotification(parameters$message, type = "warning", duration = 8)
      return()
    }

    updateSelectInput(session, "monitoring_compound_id", selected = parameters$compound_id)
    updateSelectInput(session, "monitoring_mode", selected = parameters$mode)
    updateNavbarPage(session, "main_nav", selected = "Suivi molecules")
    showNotification(
      paste0("Suivi temporel prepare pour ", parameters$compound_name, " (", toupper(parameters$mode), ")."),
      type = "message",
      duration = 7
    )
  })

  observeEvent(input$import_batch_screening_csv, {
    uploaded <- input$import_batch_screening_csv
    if (is.null(uploaded) || is.null(uploaded$datapath) || !nzchar(uploaded$datapath)) {
      return()
    }
    results <- tryCatch(read_batch_screening_export(uploaded$datapath), error = function(e) e)
    if (inherits(results, "error")) {
      showNotification(conditionMessage(results), type = "warning", duration = 10)
      return()
    }
    batch_screening_results(results)
    batch_screening_status(paste0(
      "Resultats importes : ", length(unique(results$parquet_id)),
      " fichier(s), ", nrow(results), " resultat(s)"
    ))
    showNotification("Export de screening importe.", type = "message", duration = 6)
  }, ignoreInit = TRUE)

  output$batch_screening_status <- renderText(batch_screening_status())

  output$batch_result_files <- renderText({
    results <- batch_screening_results()
    if (is.null(results) || nrow(results) == 0) return("-")
    length(unique(results$parquet_id))
  })

  output$batch_result_rows <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    nrow(results)
  })

  output$batch_detected_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$status == "Detected", na.rm = TRUE)
  })

  output$batch_level_zero_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$confidence_level == 0, na.rm = TRUE)
  })

  output$batch_level_one_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$confidence_level == 1, na.rm = TRUE)
  })

  output$batch_level_two_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$confidence_level == 2, na.rm = TRUE)
  })

  output$batch_level_three_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$confidence_level == 3, na.rm = TRUE)
  })

  output$batch_error_count <- renderText({
    results <- batch_screening_results()
    if (is.null(results)) return("-")
    sum(results$status == "Error", na.rm = TRUE)
  })

  output$batch_screening_results_table <- renderDT({
    results <- batch_screening_results()
    if (is.null(results)) {
      return(datatable(
        data.frame(message = "Aucun screening de lot lance", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    keep <- c(
      "file", "source_type", "reference_year", "reference_month", "file_type",
      "duplicate_label", "replicate_label", "file_mode",
      "compound_name", "compound_type", "mode",
      "status", "confidence_level", "confidence_label", "is_detected", "minimum_confidence_level",
      "expected_mz", "observed_mz", "mz_tolerance", "mz_error_da", "mz_error_ppm", "mz_match",
      "expected_rt", "rt_tolerance", "max_intensity", "rt_max_intensity", "rt_at_max", "rt_error",
      "rt_match", "rt_intensity_match", "rt_window_used", "rt_window_available", "intensity_match",
      "expected_dt", "observed_dt", "dt_tolerance_pct", "dt_error", "dt_error_pct", "dt_match",
      "dt_status", "expected_ccs", "observed_ccs", "ccs_tolerance_pct", "ccs_error", "ccs_error_pct",
      "ccs_match", "ccs_status", "expected_dt_from_ccs", "ccs_calibration_c1", "ccs_calibration_c2",
      "ccs_to_dt_error_ms", "ccs_to_dt_error_pct", "ccs_to_dt_match", "ccs_to_dt_status", "n_scans",
      "rt_n_scans", "area_sum", "rt_area_sum", "max_intensity_total", "rt_at_global_max",
      "n_scans_total", "area_sum_total", "total_intensity", "total_intensity_n_points",
      "total_intensity_mslevel", "total_intensity_status", "total_intensity_error", "require_rt_match", "use_dt", "use_ccs", "mslevel",
      "screened_at", "error"
    )
    x <- results[, intersect(keep, names(results)), drop = FALSE]
    table <- datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE)
    )
    table |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#1f8f4d", "#c9342f", "#a66b00")
        ),
        color = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#ffffff", "#ffffff", "#ffffff")
        ),
        fontWeight = "bold"
      ) |>
      formatStyle(
        "confidence_label",
        backgroundColor = styleEqual(
          c("Preuve 0 - aucun signal", "Preuve 1 - m/z", "Preuve 2 - m/z + RT", "Preuve 3 - m/z + RT + mobilite", "Erreur"),
          c("#fff1f0", "#fff8e1", "#e8f4f8", "#eef8f1", "#fff8e1")
        ),
        fontWeight = "bold"
      )
  })

  output$download_batch_screening_results <- downloadHandler(
    filename = function() {
      paste0("screening_lot_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      results <- batch_screening_results()
      req(!is.null(results))
      export <- results
      export$file_path <- NULL
      utils::write.csv(sanitize_csv_for_export(export), file, row.names = FALSE, na = "")
    }
  )

  output$monitoring_files_count <- renderText({
    x <- monitoring_display_results()
    if (nrow(x) == 0) return("-")
    nrow(x)
  })

  output$monitoring_detected_count <- renderText({
    x <- monitoring_display_results()
    if (nrow(x) == 0 || !"status" %in% names(x)) return("-")
    sum(x$status == "Detected", na.rm = TRUE)
  })

  output$monitoring_signal_median <- renderText({
    x <- monitoring_display_results()
    values <- if (nrow(x) == 0 || !"signal_value" %in% names(x)) numeric() else x$signal_value
    values <- values[is.finite(values)]
    if (length(values) == 0) return("-")
    format_monitoring_value(stats::median(values))
  })

  output$monitoring_blank_signal <- renderText({
    blank <- monitoring_selected_blank()
    metric <- input$monitoring_metric %||% "rt_area_sum"
    if (nrow(blank) != 1 || !metric %in% names(blank)) return("-")
    format_monitoring_value(blank[[metric]][[1]])
  })

  output$monitoring_closest_reference_details <- renderText({
    reference <- monitoring_closest_normalization_reference()
    if (nrow(reference) != 1) {
      return("Aucun etalon interne de reference disponible")
    }
    if (!identical(reference$selection_status[[1]], "Etalon interne le plus proche selectionne")) {
      return(as.character(reference$selection_status[[1]]))
    }
    paste0(
      reference$compound_name[[1]],
      " | RT cible ", format(round(reference$target_expected_rt[[1]], 3), trim = TRUE),
      " min | RT etalon ", format(round(reference$reference_expected_rt[[1]], 3), trim = TRUE),
      " min | ecart ", format(round(reference$rt_distance[[1]], 3), trim = TRUE), " min"
    )
  })

  output$monitoring_plot <- renderPlot({
    x <- monitoring_display_results()
    validate(need(nrow(x) > 0, "Lancez un screening de lot pour afficher le suivi."))

    selected_blank <- monitoring_selected_blank()
    metric <- input$monitoring_metric %||% "rt_area_sum"
    blank_signal <- if (nrow(selected_blank) == 1 && metric %in% names(selected_blank)) {
      suppressWarnings(as.numeric(selected_blank[[metric]][[1]]))
    } else {
      NA_real_
    }
    treatment <- input$monitoring_treatment %||% "raw"
    normalization <- input$monitoring_normalization %||% "none"
    if (identical(treatment, "blank_corrected")) {
      validate(need(
        is.finite(blank_signal),
        "Selectionnez un blanc de reference avec un signal exploitable pour appliquer la correction."
      ))
    }
    if (identical(normalization, "internal_standard")) {
      validate(need(
        nzchar(input$monitoring_normalization_compound_id %||% ""),
        "Selectionnez un etalon de reference pour appliquer la normalisation."
      ))
    }
    if (identical(normalization, "closest_internal_standard")) {
      closest_reference <- monitoring_closest_normalization_reference()
      validate(need(
        nrow(closest_reference) == 1 &&
          identical(closest_reference$selection_status[[1]], "Etalon interne le plus proche selectionne"),
        if (nrow(closest_reference) == 1) {
          as.character(closest_reference$selection_status[[1]])
        } else {
          "Aucun etalon interne compatible avec RT dans le lot."
        }
      ))
    }
    if (!identical(normalization, "none")) {
      normalization_error <- if (
        identical(normalization, "total_intensity") &&
          all(x$normalisation_status == "Intensite totale non calculee pour ce lot", na.rm = TRUE)
      ) {
        "Relancez le screening du lot en cochant 'Calculer l'intensite totale MS'."
      } else {
        "Aucune valeur ne peut etre normalisee avec les parametres choisis."
      }
      validate(need(
        any(x$normalisation_status == "Appliquee", na.rm = TRUE),
        normalization_error
      ))
    }

    selected_blank_rows <- x[x$selected_blank & is.finite(x$signal_value), , drop = FALSE]
    blank_reference_signal <- if (nrow(selected_blank_rows) > 0) {
      selected_blank_rows$signal_value[[1]]
    } else {
      NA_real_
    }
    reference_names <- unique(as.character(x$normalisation_reference))
    reference_names <- reference_names[!is.na(reference_names) & nzchar(reference_names)]
    reference_name <- if (length(reference_names) > 0) reference_names[[1]] else NA_character_

    x <- x[is.finite(x$signal_value) & !is.na(x$monitoring_date), , drop = FALSE]
    validate(need(nrow(x) > 0, "Aucun signal exploitable pour les filtres choisis."))

    scale <- input$monitoring_scale %||% "linear"
    is_log_scale <- identical(scale, "log")
    if (is_log_scale) {
      x <- x[x$signal_value > 0, , drop = FALSE]
      validate(need(
        nrow(x) > 0,
        "Aucune valeur strictement positive ne peut etre affichee en echelle logarithmique."
      ))
      y_values <- c(x$signal_value, blank_reference_signal)
      y_values <- y_values[is.finite(y_values) & y_values > 0]
      y_min <- min(y_values)
      y_max <- max(y_values)
      if (y_max <= y_min) {
        y_min <- y_min / 10
        y_max <- y_max * 10
      } else {
        y_min <- y_min / 1.2
        y_max <- y_max * 1.2
      }
    } else {
      y_values <- c(x$signal_value, blank_reference_signal)
      y_values <- y_values[is.finite(y_values)]
      y_min <- min(c(0, y_values), na.rm = TRUE)
      y_max <- max(y_values, na.rm = TRUE)
      if (!is.finite(y_max) || y_max <= y_min) y_max <- y_min + 1
    }

    dates <- as.Date(x$monitoring_date)
    x_range <- range(dates)
    if (x_range[[1]] == x_range[[2]]) {
      x_range <- x_range + c(-15, 15)
    }
    plot(
      dates,
      x$signal_value,
      type = "n",
      xaxt = "n",
      xlim = x_range,
      ylim = c(y_min, if (is_log_scale) y_max else y_max * 1.08),
      xlab = "Periode de reference",
      ylab = paste0(
        monitoring_value_label(metric, treatment, normalization, reference_name),
        if (is_log_scale) " (echelle logarithmique)" else ""
      ),
      log = if (is_log_scale) "y" else "",
      main = ""
    )
    axis.Date(1, at = sort(unique(dates)), format = "%Y-%m")
    if (!is_log_scale) {
      abline(h = 0, col = "#d7dce0", lwd = 1)
    }
    if (is.finite(blank_reference_signal) && (!is_log_scale || blank_reference_signal > 0)) {
      abline(h = blank_reference_signal, col = "#215a6d", lty = 2, lwd = 1.4)
    }

    status_colors <- c("Detected" = "#1f8f4d", "Not Detected" = "#c9342f", "Error" = "#a66b00")
    point_colors <- unname(status_colors[x$status])
    point_colors[is.na(point_colors)] <- "#6c757d"
    is_blank <- rep(FALSE, nrow(x))
    if ("file_type" %in% names(x)) {
      is_blank <- !is.na(x$file_type) & x$file_type == "Blanc"
    }
    points(
      dates,
      x$signal_value,
      pch = ifelse(is_blank, 21, 19),
      bg = point_colors,
      col = ifelse(is_blank, "#1f2933", point_colors),
      cex = 1.25
    )
    if (any(x$selected_blank)) {
      points(dates[x$selected_blank], x$signal_value[x$selected_blank], pch = 1, cex = 2, lwd = 1.5, col = "#215a6d")
    }
    legend_labels <- c("Detected", "Not Detected", "Erreur")
    legend_colors <- c("#1f8f4d", "#c9342f", "#a66b00")
    legend_pch <- c(19, 19, 19)
    legend_lty <- c(0, 0, 0)
    if (is.finite(blank_reference_signal) && (!is_log_scale || blank_reference_signal > 0)) {
      legend_labels <- c(
        legend_labels,
        if (identical(treatment, "blank_corrected")) "Blanc corrige (0)" else "Blanc selectionne"
      )
      legend_colors <- c(legend_colors, "#215a6d")
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 2)
    }
    legend(
      "topright",
      legend = legend_labels,
      col = legend_colors,
      pch = legend_pch,
      lty = legend_lty,
      bty = "n"
    )
  })

  output$monitoring_results_table <- renderDT({
    x <- monitoring_display_results()
    if (nrow(x) == 0) {
      return(datatable(
        data.frame(message = "Aucun resultat de screening a afficher", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    x$periode <- x$period_label
    x$signal_affiche <- x$signal_value
    x$blanc_selectionne <- ifelse(x$selected_blank, "Oui", "")
    keep <- c(
      "periode", "file", "file_type", "duplicate_label", "replicate_label", "mode",
      "compound_name", "compound_type", "status", "confidence_level", "confidence_label", "normalisation_label",
      "normalisation_reference", "normalisation_reference_expected_rt", "normalisation_reference_rt_distance",
      "normalisation_divisor", "normalisation_status", "signal_affiche",
      "signal_traite", "signal_brut", "signal_corrige_blanc", "signal_normalise",
      "injection_aggregation_method", "injection_aggregation_n_total", "injection_aggregation_n_used",
      "injection_aggregation_n_excluded", "duplicate_aggregation_method", "duplicate_aggregation_n_total",
      "duplicate_aggregation_n_used", "duplicate_aggregation_n_excluded", "aggregation_source_files",
      "blanc_selectionne", "rt_at_max", "rt_area_sum", "rt_max_intensity", "error"
    )
    table_data <- x[, intersect(keep, names(x)), drop = FALSE]
    table <- datatable(table_data, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    table |>
      formatSignif(intersect(c("normalisation_divisor", "signal_affiche", "signal_normalise", "normalisation_reference_expected_rt", "normalisation_reference_rt_distance"), names(table_data)), digits = 5, mark = " ") |>
      formatRound(intersect(c("signal_traite", "signal_brut", "signal_corrige_blanc", "rt_at_max", "rt_area_sum", "rt_max_intensity"), names(table_data)), 3) |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#1f8f4d", "#c9342f", "#a66b00")
        ),
        color = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#ffffff", "#ffffff", "#ffffff")
        ),
        fontWeight = "bold"
      )
  })

  output$monitoring_raw_results_table <- renderDT({
    x <- monitoring_raw_audit()
    if (nrow(x) == 0) {
      return(datatable(
        data.frame(message = "Aucune valeur individuelle a afficher", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    x$periode <- x$period_label
    x$signal_affiche <- x$signal_value
    keep <- c(
      "periode", "file", "file_type", "duplicate_label", "replicate_label", "mode",
      "compound_name", "compound_type", "status", "confidence_level", "confidence_label", "signal_affiche",
      "signal_traite", "signal_brut", "signal_corrige_blanc", "signal_normalise",
      "aggregation_group", "aggregation_method", "aggregation_value_eligible",
      "aggregation_outlier", "aggregation_included", "aggregation_reason", "error"
    )
    table_data <- x[, intersect(keep, names(x)), drop = FALSE]
    table <- datatable(table_data, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    table |>
      formatSignif(intersect(c("signal_affiche", "signal_normalise"), names(table_data)), digits = 5, mark = " ") |>
      formatRound(intersect(c("signal_traite", "signal_brut", "signal_corrige_blanc"), names(table_data)), 3) |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#1f8f4d", "#c9342f", "#a66b00")
        ),
        color = styleEqual(
          c("Detected", "Not Detected", "Error"),
          c("#ffffff", "#ffffff", "#ffffff")
        ),
        fontWeight = "bold"
      )
  })

  output$monitoring_duplicates_table <- renderDT({
    x <- monitoring_duplicates_summary()
    if (nrow(x) == 0) {
      return(datatable(
        data.frame(message = "Aucun echantillon exploitable pour comparer les duplicats", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)) |>
      formatSignif(
        intersect(c("moyenne", "mediane", "ecart_type", "cv_pct", "minimum", "maximum"), names(x)),
        digits = 5,
        mark = " "
      )
  })

  output$download_monitoring_results <- downloadHandler(
    filename = function() {
      paste0("suivi_etalons_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      x <- monitoring_display_results()
      req(nrow(x) > 0)
      x$monitoring_metric <- input$monitoring_metric %||% "rt_area_sum"
      x$monitoring_metric_label <- monitoring_metric_label(input$monitoring_metric %||% "rt_area_sum")
      x$monitoring_treatment <- input$monitoring_treatment %||% "raw"
      x$monitoring_treatment_label <- monitoring_treatment_label(input$monitoring_treatment %||% "raw")
      x$monitoring_normalization <- input$monitoring_normalization %||% "none"
      x$monitoring_normalization_label <- monitoring_normalization_label(input$monitoring_normalization %||% "none")
      x$monitoring_normalization_reference_id <- if ("normalisation_reference_id" %in% names(x)) {
        x$normalisation_reference_id
      } else {
        input$monitoring_normalization_compound_id %||% ""
      }
      x$monitoring_scale <- input$monitoring_scale %||% "linear"
      x$monitoring_injection_aggregation <- input$monitoring_injection_aggregation %||% "none"
      x$monitoring_injection_exclude_outliers <- isTRUE(input$monitoring_injection_exclude_outliers)
      x$monitoring_duplicate_aggregation <- input$monitoring_duplicate_aggregation %||% "none"
      x$monitoring_duplicate_exclude_outliers <- isTRUE(input$monitoring_duplicate_exclude_outliers)
      x$monitoring_outlier_threshold <- input_number(input$monitoring_outlier_threshold, 3.5)
      utils::write.csv(sanitize_csv_for_export(x), file, row.names = FALSE, na = "")
    }
  )

  output$download_monitoring_raw_results <- downloadHandler(
    filename = function() {
      paste0("suivi_etalons_valeurs_brutes_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      x <- monitoring_raw_audit()
      req(nrow(x) > 0)
      x$monitoring_metric <- input$monitoring_metric %||% "rt_area_sum"
      x$monitoring_treatment <- input$monitoring_treatment %||% "raw"
      x$monitoring_normalization <- input$monitoring_normalization %||% "none"
      x$monitoring_normalization_reference_id <- if ("normalisation_reference_id" %in% names(x)) {
        x$normalisation_reference_id
      } else {
        input$monitoring_normalization_compound_id %||% ""
      }
      x$monitoring_injection_aggregation <- input$monitoring_injection_aggregation %||% "none"
      x$monitoring_injection_exclude_outliers <- isTRUE(input$monitoring_injection_exclude_outliers)
      x$monitoring_duplicate_aggregation <- input$monitoring_duplicate_aggregation %||% "none"
      x$monitoring_duplicate_exclude_outliers <- isTRUE(input$monitoring_duplicate_exclude_outliers)
      x$monitoring_outlier_threshold <- input_number(input$monitoring_outlier_threshold, 3.5)
      utils::write.csv(sanitize_csv_for_export(x), file, row.names = FALSE, na = "")
    }
  )

  control_catalog_diagnostics <- reactive({
    catalog_file_diagnostics(parquet_files())
  })

  output$control_files_count <- renderText(nrow(parquet_files()))
  output$control_json_count <- renderText({
    diagnostics <- control_catalog_diagnostics()
    paste0(sum(diagnostics$json == "Associe"), " / ", nrow(diagnostics))
  })
  output$control_ready_count <- renderText({
    sum(control_catalog_diagnostics()$statut == "OK")
  })
  output$control_review_count <- renderText({
    diagnostics <- control_catalog_diagnostics()
    sum(diagnostics$statut != "OK")
  })

  control_status_style <- function(table) {
    table |>
      formatStyle(
        "statut",
        backgroundColor = styleEqual(
          c("OK", "A verifier", "A corriger", "Erreur", "Information", "En attente"),
          c("#1f8f4d", "#a66b00", "#c9342f", "#c9342f", "#e7f0f5", "#f1f3f5")
        ),
        color = styleEqual(
          c("OK", "A verifier", "A corriger", "Erreur", "Information", "En attente"),
          c("#ffffff", "#ffffff", "#ffffff", "#ffffff", "#22313a", "#22313a")
        ),
        fontWeight = "bold"
      )
  }

  output$checks_table <- renderDT({
    checks <- global_data_checks(metadata_index, compound_catalog(), parquet_files())
    control_status_style(datatable(checks, rownames = FALSE, options = list(dom = "t", pageLength = 12)))
  })

  output$control_catalog_table <- renderDT({
    diagnostics <- control_catalog_diagnostics()
    if (nrow(diagnostics) == 0) {
      return(datatable(
        data.frame(message = "Aucun fichier Parquet disponible", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    control_status_style(datatable(
      diagnostics,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    ))
  })

  output$control_file_checks_table <- renderDT({
    diagnostics <- parquet_file_diagnostics(
      info = control_parquet_info(),
      file = control_selected_parquet_file(),
      error = control_parquet_error()
    )
    control_status_style(datatable(diagnostics, rownames = FALSE, options = list(dom = "t", pageLength = 12, scrollX = TRUE)))
  })
}

app <- shinyApp(ui, server)
app
