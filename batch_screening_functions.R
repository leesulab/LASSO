#' Batch screening functions for HRMS parquet files using polars
#' Allows screening for specific molecules across multiple files simultaneously

library(polars)
library(ggplot2)
library(patchwork)

#' Extract year-month label from file names
#'
#' @param file_names Character vector of file names
#' @return Character vector of year-month labels (e.g., "2021-01")
extract_date_label <- function(file_names) {
  # Try to extract date patterns from file names
  # Pattern 1: "E-2021_01-" format (from reorder_by_date)
  # Pattern 2: "2024-01" or "2024_01" format
  # Pattern 3: Year only like "2021" or "2023"
  
  labels <- sapply(file_names, function(fn) {
    # Try E-YYYY_MM pattern first
    match1 <- regmatches(fn, regexpr("E-\\d{4}_\\d{2}", fn))
    if (length(match1) > 0 && nchar(match1) > 0) {
      # Extract year and month from E-2021_01
      year <- substr(match1, 3, 6)
      month <- substr(match1, 8, 9)
      return(paste0(year, "-", month))
    }
    
    # Try YYYY-MM or YYYY_MM pattern
    match2 <- regmatches(fn, regexpr("20[12]\\d[-_]\\d{2}", fn))
    if (length(match2) > 0 && nchar(match2) > 0) {
      return(gsub("_", "-", match2))
    }
    
    # Try year only pattern
    match3 <- regmatches(fn, regexpr("20[12]\\d", fn))
    if (length(match3) > 0 && nchar(match3) > 0) {
      return(match3)
    }
    
    # Fallback: return shortened file name
    return(substr(fn, 1, 10))
  }, USE.NAMES = FALSE)
  
  labels
}


#' Get date labels from metadata for file names
#'
#' @param file_names Character vector of file names
#' @param metadata Data frame with columns: name, year, month
#' @return Character vector of year-month labels (e.g., "2021-01")
get_date_labels_from_metadata <- function(file_names, metadata) {
  
  # Decode URL encoding for matching
  file_names_decoded <- sapply(file_names, URLdecode, USE.NAMES = FALSE)
  
  # Also decode metadata names
  metadata$name_decoded <- sapply(metadata$name, URLdecode, USE.NAMES = FALSE)
  
  # Remove duplicates from metadata, keeping year >= 2020
  metadata <- metadata[order(metadata$name, -metadata$year), ]
  metadata <- metadata[!duplicated(metadata$name), ]
  
  labels <- sapply(file_names_decoded, function(fn) {
    # Try direct match first
    idx <- match(fn, metadata$name_decoded)
    
    # If no match, try matching the base filename (without prefix like E-2021_01-)
    if (is.na(idx)) {
      # Remove E-YYYY_MM- prefix if present
      fn_clean <- gsub("^E-\\d{4}_\\d{2}-", "", fn)
      idx <- match(fn_clean, metadata$name_decoded)
    }
    
    if (!is.na(idx)) {
      year <- metadata$year[idx]
      month <- metadata$month[idx]
      if (!is.na(year) && !is.na(month)) {
        return(sprintf("%d-%02d", as.integer(year), as.integer(month)))
      }
    }
    
    # Fallback to extract_date_label if no match
    return(NA_character_)
  }, USE.NAMES = FALSE)
  
  # For any NA values, fall back to filename extraction
  na_idx <- is.na(labels)
  if (any(na_idx)) {
    labels[na_idx] <- extract_date_label(file_names[na_idx])
  }
  
  labels
}


#' Reorder batch results by date using metadata
#'
#' @param batch_results List output from screen_mz_batch()
#' @param metadata Data frame with columns: name (filename), year, month
#' @return Reordered list of batch results
reorder_by_date <- function(batch_results, metadata) {
  
  # Ensure month is numeric
  if (is.character(metadata$month)) {
    metadata$month <- as.numeric(metadata$month)
  }
  
  # Remove duplicate entries, keeping the one with year >= 2020 (more likely correct)
  # This handles cases where metadata has duplicates with wrong years (e.g., 2007 vs 2021)
  metadata <- metadata[order(metadata$name, -metadata$year), ]
  metadata <- metadata[!duplicated(metadata$name), ]
  
  # Create date order column
  metadata$date_order <- metadata$year * 100 + metadata$month
  
  # Match batch results names to metadata
  file_names <- names(batch_results)
  
  # Decode URL encoding (%20 -> space) for matching
  file_names_decoded <- sapply(file_names, URLdecode)
  
  # Also create decoded version in metadata for matching
  metadata$name_decoded <- sapply(metadata$name, URLdecode)
  
  # Create ordering based on metadata
  order_df <- data.frame(
    file = file_names,
    file_decoded = file_names_decoded,
    stringsAsFactors = FALSE
  )
  
  # Try to match - first with original names, then with decoded
  order_df$idx <- match(order_df$file, metadata$name)
  
  # For unmatched, try decoded matching
  unmatched <- is.na(order_df$idx)
  if (any(unmatched)) {
    order_df$idx[unmatched] <- match(order_df$file_decoded[unmatched], metadata$name_decoded)
  }
  
  # Still unmatched? Try matching decoded file to original metadata name
  still_unmatched <- is.na(order_df$idx)
  if (any(still_unmatched)) {
    order_df$idx[still_unmatched] <- match(order_df$file_decoded[still_unmatched], metadata$name)
  }
  
  order_df$date_order <- metadata$date_order[order_df$idx]
  order_df$year <- metadata$year[order_df$idx]
  order_df$month <- metadata$month[order_df$idx]
  
  # Handle unmatched files (put them at the end)
  n_unmatched <- sum(is.na(order_df$date_order))
  if (n_unmatched > 0) {
    warning(sprintf("%d files could not be matched to metadata", n_unmatched))
  }
  order_df$date_order[is.na(order_df$date_order)] <- Inf
  
  # Sort by date, then by filename
  order_df <- order_df[order(order_df$date_order, order_df$file), ]
  
  # Reorder batch results and rename with decoded names for cleaner display
  reordered <- batch_results[order_df$file]
  
  # Optionally rename with cleaner names (replace %20 with space in display)
  new_names <- gsub("%20", " ", names(reordered))
  # Create unique names based on year-month prefix
  for (i in seq_along(reordered)) {
    if (!is.na(order_df$year[i]) && is.finite(order_df$date_order[i])) {
      new_names[i] <- sprintf("E-%d_%02d-%s", 
                              order_df$year[i], 
                              order_df$month[i],
                              gsub("%20", " ", gsub("\\.parquet$", "", order_df$file[i])))
      new_names[i] <- paste0(new_names[i], ".parquet")
    }
  }
  names(reordered) <- new_names
  
  min_year <- min(order_df$year, na.rm = TRUE)
  max_year <- max(order_df$year[is.finite(order_df$date_order)], na.rm = TRUE)
  min_month <- min(order_df$month[order_df$year == min_year], na.rm = TRUE)
  max_month <- max(order_df$month[order_df$year == max_year], na.rm = TRUE)
  
  message(sprintf("Reordered %d samples by date (from %d-%02d to %d-%02d)",
                  length(reordered), min_year, min_month, max_year, max_month))
  
  reordered
}


#' Load metadata from Excel file and prepare for use
#'
#' @param xlsx_path Path to the Excel metadata file
#' @return Data frame with metadata
load_metadata <- function(xlsx_path) {
  
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to load Excel files")
  }
  
  metadata <- readxl::read_excel(xlsx_path)
  
  # Ensure required columns exist
  required_cols <- c("name", "year", "month")
  missing_cols <- setdiff(required_cols, colnames(metadata))
  
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Convert month to numeric if needed
  if (is.character(metadata$month)) {
    metadata$month <- as.numeric(metadata$month)
  }
  
  metadata
}


#' Filter batch results to keep only samples (exclude blancs and references)
#'
#' @param batch_results List output from screen_mz_batch()
#' @param exclude_patterns Character vector of patterns to exclude (case insensitive)
#' @return Filtered list of batch results
filter_samples <- function(batch_results, 
                           exclude_patterns = c("blanc", "reference", "pool", "cal", "SST")) {
  
  # Build regex pattern
  pattern <- paste(exclude_patterns, collapse = "|")
  
 # Filter out files matching any pattern
  keep_idx <- !grepl(pattern, names(batch_results), ignore.case = TRUE)
  
  filtered <- batch_results[keep_idx]
  
  message(sprintf("Kept %d/%d samples (excluded %d)", 
                  length(filtered), length(batch_results), 
                  length(batch_results) - length(filtered)))
  
  filtered
}


#' Filter file paths to keep only samples (before screening)
#'
#' @param file_paths Character vector of file paths or URLs
#' @param exclude_patterns Character vector of patterns to exclude (case insensitive)
#' @return Filtered character vector of file paths
filter_sample_paths <- function(file_paths,
                                exclude_patterns = c("blanc", "reference", "pool", "cal")) {
  
  # Build regex pattern
  pattern <- paste(exclude_patterns, collapse = "|")
  
  # Filter based on basename
  basenames <- basename(file_paths)
  keep_idx <- !grepl(pattern, basenames, ignore.case = TRUE)
  
  filtered <- file_paths[keep_idx]
  
  message(sprintf("Kept %d/%d files (excluded %d)", 
                  length(filtered), length(file_paths), 
                  length(file_paths) - length(filtered)))
  
  filtered
}


#' Screen multiple parquet files for a target m/z value
#'
#' @param file_paths Character vector of parquet file paths (local or URLs)
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes (default c(0, 30))
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file
screen_mz_batch <- function(file_paths, 
                            target_mz, 
                            mz_tolerance = 0.01,
                            rt_range = c(0, 30),
                            mslevel = "1",
                            min_intensity = 0,
                            show_progress = TRUE) {
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f)...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  results <- lapply(seq_along(file_paths), function(i) {
    file_path <- file_paths[i]
    
    if (show_progress) {
      # Progress message with carriage return to overwrite
      cat(sprintf("\r[%d/%d] Processing: %s", i, n_files, basename(file_path)))
      if (i == n_files) cat("\n")
    }
    
    tryCatch({
      # Lazy scan the parquet file
      lf <- pl$scan_parquet(file_path)
      
      # Query EIC data
      eic <- lf$
        filter(pl$col("mslevel") == mslevel)$
        filter(pl$col("mz") >= mz_min & pl$col("mz") <= mz_max)$
        filter(pl$col("rt") >= rt_range[1] & pl$col("rt") <= rt_range[2])$
        filter(pl$col("intensity") >= min_intensity)$
        group_by(pl$col("rt"))$
        agg(
          pl$col("intensity")$sum()$alias("intensity")
        )$
        sort("rt")$
        collect() |>
        as.data.frame()
      
      # Add metadata
      eic$file <- basename(file_path)
      eic$target_mz <- target_mz
      
      eic
      
    }, error = function(e) {
      if (show_progress) cat("\n")
      warning(sprintf("Error processing %s: %s", basename(file_path), e$message))
      data.frame(
        rt = numeric(),
        intensity = numeric(),
        file = character(),
        target_mz = numeric()
      )
    })
  })
  
  if (show_progress) {
    message(sprintf("Done! Processed %d files.", n_files))
  }
  
  names(results) <- sapply(file_paths, basename)
  results
}


#' Fast screening of multiple parquet files for a target m/z value
#'
#' Optimized version using predicate pushdown, column selection, and parallel processing.
#'
#' @param file_paths Character vector of parquet file paths (local or URLs)
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes, or NULL for no restriction (default NULL)
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param n_workers Number of parallel workers (default: number of cores - 1)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file
screen_mz_batch_fast <- function(file_paths, 
                                  target_mz, 
                                  mz_tolerance = 0.01,
                                  rt_range = NULL,
                                  mslevel = "1",
                                  min_intensity = 0,
                                  n_workers = NULL,
                                  show_progress = TRUE) {
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f)...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  # Internal function to process a single file
  process_file <- function(file_path) {
    tryCatch({
      # Lazy scan with column selection for faster reading
      lf <- pl$scan_parquet(file_path)
      
      # Build filter predicate - rt_range is optional
      if (!is.null(rt_range)) {
        filter_expr <- (
          (pl$col("mslevel") == mslevel) &
          (pl$col("mz") >= mz_min) & (pl$col("mz") <= mz_max) &
          (pl$col("rt") >= rt_range[1]) & (pl$col("rt") <= rt_range[2]) &
          (pl$col("intensity") >= min_intensity)
        )
      } else {
        filter_expr <- (
          (pl$col("mslevel") == mslevel) &
          (pl$col("mz") >= mz_min) & (pl$col("mz") <= mz_max) &
          (pl$col("intensity") >= min_intensity)
        )
      }
      
      # Query EIC data with combined predicates for better pushdown
      # and select only needed columns upfront
      eic <- lf$
        select(c("mslevel", "mz", "rt", "intensity"))$
        filter(filter_expr)$
        group_by("rt")$
        agg(
          pl$col("intensity")$sum()$alias("intensity")
        )$
        sort("rt")$
        collect() |>
        as.data.frame()
      
      # Add metadata
      eic$file <- basename(file_path)
      eic$target_mz <- target_mz
      
      eic
      
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", basename(file_path), e$message))
      data.frame(
        rt = numeric(),
        intensity = numeric(),
        file = character(),
        target_mz = numeric()
      )
    })
  }
  
  # Determine number of workers
  if (is.null(n_workers)) {
    n_workers <- max(1, parallel::detectCores() - 1)
  }
  
  # Use parallel processing if multiple workers and files
  if (n_workers > 1 && n_files > 1) {
    if (show_progress) {
      message(sprintf("Using %d parallel workers...", n_workers))
    }
    
    # Create cluster
    cl <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export required objects to workers
    parallel::clusterExport(cl, c("mz_min", "mz_max", "rt_range", "mslevel", 
                                   "min_intensity", "target_mz", "process_file"),
                            envir = environment())
    
    # Try to load polars on each worker
    # polars on Linux is installed from r-universe, workers need the lib path
    load_result <- tryCatch({
      # Get the polars library path from the main session
      polars_path <- dirname(system.file(package = "polars"))
      
      parallel::clusterExport(cl, "polars_path", envir = environment())
      
      parallel::clusterEvalQ(cl, {
        # Add the polars library path to .libPaths if not already there
        if (!polars_path %in% .libPaths()) {
          .libPaths(c(polars_path, .libPaths()))
        }
        library(polars)
        TRUE
      })
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    # If polars loading failed, fall back to sequential processing
    if (!load_result) {
      warning("Could not load polars on parallel workers. Falling back to sequential processing.")
      parallel::stopCluster(cl)
      n_workers <- 1
    }
  }
  
  # Re-check after potential fallback
  if (n_workers > 1 && n_files > 1) {
    # Process files in parallel
    if (show_progress) {
      # Use pbapply for progress bar if available
      if (requireNamespace("pbapply", quietly = TRUE)) {
        results <- pbapply::pblapply(file_paths, process_file, cl = cl)
      } else {
        results <- parallel::parLapply(cl, file_paths, process_file)
      }
    } else {
      results <- parallel::parLapply(cl, file_paths, process_file)
    }
    
  } else {
    # Sequential processing
    if (show_progress) {
      results <- lapply(seq_along(file_paths), function(i) {
        cat(sprintf("\r[%d/%d] Processing: %s", i, n_files, basename(file_paths[i])))
        if (i == n_files) cat("\n")
        process_file(file_paths[i])
      })
    } else {
      results <- lapply(file_paths, process_file)
    }
  }
  
  if (show_progress) {
    message(sprintf("Done! Processed %d files.", n_files))
  }
  
  names(results) <- sapply(file_paths, basename)
  results
}


#' Screen multiple parquet files using DuckDB (often faster than Polars)
#'
#' Uses DuckDB's optimized parquet reader with predicate pushdown for fast screening.
#' DuckDB often outperforms Polars for I/O-bound parquet queries.
#' Supports both local files and HTTP/HTTPS URLs.
#'
#' @param file_paths Character vector of parquet file paths (local or HTTP/HTTPS URLs)
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes, or NULL for no restriction (default NULL)
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file
screen_mz_batch_duckdb <- function(file_paths, 
                                    target_mz, 
                                    mz_tolerance = 0.01,
                                    rt_range = NULL,
                                    mslevel = "1",
                                    min_intensity = 0,
                                    show_progress = TRUE) {
  
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Install with: install.packages('duckdb')")
  }
  
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required. Install with: install.packages('DBI')")
  }
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  # Check if any files are URLs
  has_urls <- any(grepl("^https?://", file_paths))
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f) using DuckDB...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  # Create a single DuckDB connection (reused for all files)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Install and load httpfs extension if URLs are present
  if (has_urls) {
    tryCatch({
      DBI::dbExecute(con, "INSTALL httpfs;")
      DBI::dbExecute(con, "LOAD httpfs;")
    }, error = function(e) {
      # httpfs might already be installed/loaded
      tryCatch({
        DBI::dbExecute(con, "LOAD httpfs;")
      }, error = function(e2) {
        warning("Could not load httpfs extension. URL access may not work.")
      })
    })
  }
  
  # Build the SQL query template - rt_range is optional
  if (!is.null(rt_range)) {
    query_template <- sprintf("
      SELECT 
        rt,
        SUM(intensity) as intensity
      FROM read_parquet('%%s')
      WHERE mslevel = '%s'
        AND mz >= %f AND mz <= %f
        AND rt >= %f AND rt <= %f
        AND intensity >= %f
      GROUP BY rt
      ORDER BY rt
    ", mslevel, mz_min, mz_max, rt_range[1], rt_range[2], min_intensity)
  } else {
    query_template <- sprintf("
      SELECT 
        rt,
        SUM(intensity) as intensity
      FROM read_parquet('%%s')
      WHERE mslevel = '%s'
        AND mz >= %f AND mz <= %f
        AND intensity >= %f
      GROUP BY rt
      ORDER BY rt
    ", mslevel, mz_min, mz_max, min_intensity)
  }
  
  results <- lapply(seq_along(file_paths), function(i) {
    file_path <- file_paths[i]
    
    if (show_progress) {
      cat(sprintf("\r[%d/%d] Processing: %s", i, n_files, basename(file_path)))
      if (i == n_files) cat("\n")
    }
    
    tryCatch({
      # Execute query with file path
      query <- sprintf(query_template, file_path)
      eic <- DBI::dbGetQuery(con, query)
      
      # Handle empty results
      if (nrow(eic) == 0) {
        eic <- data.frame(
          rt = numeric(),
          intensity = numeric()
        )
      }
      
      # Add metadata
      eic$file <- basename(file_path)
      eic$target_mz <- target_mz
      
      eic
      
    }, error = function(e) {
      if (show_progress) cat("\n")
      warning(sprintf("Error processing %s: %s", basename(file_path), e$message))
      data.frame(
        rt = numeric(),
        intensity = numeric(),
        file = character(),
        target_mz = numeric()
      )
    })
  })
  
  if (show_progress) {
    message(sprintf("Done! Processed %d files.", n_files))
  }
  
  names(results) <- sapply(file_paths, basename)
  results
}


#' Screen multiple parquet files using Arrow (for local files only)
#'
#' Uses Arrow's lazy evaluation with predicate pushdown for efficient local file reading.
#' Arrow is optimized for columnar parquet access and may offer consistent performance.
#'
#' @param file_paths Character vector of local parquet file paths
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes, or NULL for no restriction (default NULL)
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file
screen_mz_batch_arrow <- function(file_paths, 
                                   target_mz, 
                                   mz_tolerance = 0.01,
                                   rt_range = NULL,
                                   mslevel = "1",
                                   min_intensity = 0,
                                   show_progress = TRUE) {
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with: install.packages('arrow')")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f) using Arrow...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  results <- lapply(seq_along(file_paths), function(i) {
    file_path <- file_paths[i]
    
    if (show_progress) {
      cat(sprintf("\r[%d/%d] Processing: %s", i, n_files, basename(file_path)))
      if (i == n_files) cat("\n")
    }
    
    tryCatch({
      # Open parquet file lazily with Arrow
      ds <- arrow::open_dataset(file_path)
      
      # Build query with predicate pushdown
      if (!is.null(rt_range)) {
        eic <- ds |>
          dplyr::select(mslevel, mz, rt, intensity) |>
          dplyr::filter(
            mslevel == !!mslevel,
            mz >= !!mz_min,
            mz <= !!mz_max,
            rt >= !!rt_range[1],
            rt <= !!rt_range[2],
            intensity >= !!min_intensity
          ) |>
          dplyr::group_by(rt) |>
          dplyr::summarise(intensity = sum(intensity, na.rm = TRUE)) |>
          dplyr::arrange(rt) |>
          dplyr::collect()
      } else {
        eic <- ds |>
          dplyr::select(mslevel, mz, rt, intensity) |>
          dplyr::filter(
            mslevel == !!mslevel,
            mz >= !!mz_min,
            mz <= !!mz_max,
            intensity >= !!min_intensity
          ) |>
          dplyr::group_by(rt) |>
          dplyr::summarise(intensity = sum(intensity, na.rm = TRUE)) |>
          dplyr::arrange(rt) |>
          dplyr::collect()
      }
      
      # Convert to data.frame
      eic <- as.data.frame(eic)
      
      # Handle empty results
      if (nrow(eic) == 0) {
        eic <- data.frame(
          rt = numeric(),
          intensity = numeric()
        )
      }
      
      # Add metadata
      eic$file <- basename(file_path)
      eic$target_mz <- target_mz
      
      eic
      
    }, error = function(e) {
      if (show_progress) cat("\n")
      warning(sprintf("Error processing %s: %s", basename(file_path), e$message))
      data.frame(
        rt = numeric(),
        intensity = numeric(),
        file = character(),
        target_mz = numeric()
      )
    })
  })
  
  if (show_progress) {
    message(sprintf("Done! Processed %d files.", n_files))
  }
  
  names(results) <- sapply(file_paths, basename)
  results
}


#' Screen multiple parquet files as a single dataset using Arrow (fastest for local files)
#'
#' Treats all parquet files as one dataset, allowing Arrow to optimize I/O across all files.
#' This is typically faster than processing files individually.
#'
#' @param file_paths Character vector of local parquet file paths
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes, or NULL for no restriction (default NULL)
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file (same format as other screen functions)
screen_mz_batch_arrow_dataset <- function(file_paths, 
                                           target_mz, 
                                           mz_tolerance = 0.01,
                                           rt_range = NULL,
                                           mslevel = "1",
                                           min_intensity = 0,
                                           show_progress = TRUE) {
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with: install.packages('arrow')")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install with: install.packages('dplyr')")
  }
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f) using Arrow dataset...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  tryCatch({
    # Open all files as a single dataset with filename as partition
    ds <- arrow::open_dataset(file_paths)
    
    # Build query with predicate pushdown - process all files in one query
    if (!is.null(rt_range)) {
      all_eic <- ds |>
        dplyr::select(mslevel, mz, rt, intensity) |>
        dplyr::filter(
          mslevel == !!mslevel,
          mz >= !!mz_min,
          mz <= !!mz_max,
          rt >= !!rt_range[1],
          rt <= !!rt_range[2],
          intensity >= !!min_intensity
        ) |>
        dplyr::mutate(file = basename(arrow::add_filename())) |>
        dplyr::group_by(file, rt) |>
        dplyr::summarise(intensity = sum(intensity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(file, rt) |>
        dplyr::collect()
    } else {
      all_eic <- ds |>
        dplyr::select(mslevel, mz, rt, intensity) |>
        dplyr::filter(
          mslevel == !!mslevel,
          mz >= !!mz_min,
          mz <= !!mz_max,
          intensity >= !!min_intensity
        ) |>
        dplyr::mutate(file = basename(arrow::add_filename())) |>
        dplyr::group_by(file, rt) |>
        dplyr::summarise(intensity = sum(intensity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(file, rt) |>
        dplyr::collect()
    }
    
    # Convert to data.frame and add target_mz
    all_eic <- as.data.frame(all_eic)
    all_eic$target_mz <- target_mz
    
    # Split into list by file (same format as other functions)
    results <- split(all_eic, all_eic$file)
    
    # Ensure all input files are in results (even if empty)
    all_basenames <- basename(file_paths)
    missing_files <- setdiff(all_basenames, names(results))
    
    for (f in missing_files) {
      results[[f]] <- data.frame(
        rt = numeric(),
        intensity = numeric(),
        file = character(),
        target_mz = numeric()
      )
    }
    
    # Reorder to match input order
    results <- results[all_basenames]
    
    if (show_progress) {
      message(sprintf("Done! Processed %d files.", n_files))
    }
    
    results
    
  }, error = function(e) {
    stop(sprintf("Error processing dataset: %s", e$message))
  })
}


#' Screen multiple parquet files as a single dataset using DuckDB (fastest for local files)
#'
#' Treats all parquet files as one dataset using DuckDB's glob/list support.
#' This is typically faster than processing files individually.
#' Supports both local files and HTTP/HTTPS URLs.
#'
#' @param file_paths Character vector of parquet file paths (local or HTTP/HTTPS URLs)
#' @param target_mz Target m/z value to search for
#' @param mz_tolerance Mass tolerance in Da (default 0.01)
#' @param rt_range Numeric vector of length 2 with RT range in minutes, or NULL for no restriction (default NULL)
#' @param mslevel MS level to query ("1" or "2", default "1")
#' @param min_intensity Minimum intensity threshold (default 0)
#' @param show_progress Show progress messages (default TRUE)
#' @return A list with EIC data for each file (same format as other screen functions)
screen_mz_batch_duckdb_dataset <- function(file_paths, 
                                            target_mz, 
                                            mz_tolerance = 0.01,
                                            rt_range = NULL,
                                            mslevel = "1",
                                            min_intensity = 0,
                                            show_progress = TRUE) {
  
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required. Install with: install.packages('duckdb')")
  }
  
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required. Install with: install.packages('DBI')")
  }
  
  mz_min <- target_mz - mz_tolerance
  mz_max <- target_mz + mz_tolerance
  
  n_files <- length(file_paths)
  
  # Check if any files are URLs
  has_urls <- any(grepl("^https?://", file_paths))
  
  if (show_progress) {
    message(sprintf("Screening %d files for m/z %.4f (± %.4f) using DuckDB dataset...", 
                    n_files, target_mz, mz_tolerance))
  }
  
  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Install and load httpfs extension if URLs are present
  if (has_urls) {
    tryCatch({
      DBI::dbExecute(con, "INSTALL httpfs;")
      DBI::dbExecute(con, "LOAD httpfs;")
    }, error = function(e) {
      tryCatch({
        DBI::dbExecute(con, "LOAD httpfs;")
      }, error = function(e2) {
        stop("Could not load httpfs extension. URL access requires httpfs.")
      })
    })
  }
  
  tryCatch({
    # Create a list of files for DuckDB - use list syntax for multiple files
    file_list <- paste0("'", file_paths, "'", collapse = ", ")
    
    # Build query with filename extraction
    if (!is.null(rt_range)) {
      query <- sprintf("
        SELECT 
          regexp_extract(filename, '[^/\\\\]+$') as file,
          rt,
          SUM(intensity) as intensity
        FROM read_parquet([%s], filename = true)
        WHERE mslevel = '%s'
          AND mz >= %f AND mz <= %f
          AND rt >= %f AND rt <= %f
          AND intensity >= %f
        GROUP BY file, rt
        ORDER BY file, rt
      ", file_list, mslevel, mz_min, mz_max, rt_range[1], rt_range[2], min_intensity)
    } else {
      query <- sprintf("
        SELECT 
          regexp_extract(filename, '[^/\\\\]+$') as file,
          rt,
          SUM(intensity) as intensity
        FROM read_parquet([%s], filename = true)
        WHERE mslevel = '%s'
          AND mz >= %f AND mz <= %f
          AND intensity >= %f
        GROUP BY file, rt
        ORDER BY file, rt
      ", file_list, mslevel, mz_min, mz_max, min_intensity)
    }
    
    # Execute single query for all files
    all_eic <- DBI::dbGetQuery(con, query)
    
    # Add target_mz
    all_eic$target_mz <- target_mz
    
    # Split into list by file (same format as other functions)
    results <- split(all_eic, all_eic$file)
    
    # Ensure all input files are in results (even if empty)
    all_basenames <- basename(file_paths)
    missing_files <- setdiff(all_basenames, names(results))
    
    for (f in missing_files) {
      results[[f]] <- data.frame(
        file = character(),
        rt = numeric(),
        intensity = numeric(),
        target_mz = numeric()
      )
    }
    
    # Reorder to match input order
    results <- results[all_basenames]
    
    if (show_progress) {
      message(sprintf("Done! Processed %d files.", n_files))
    }
    
    results
    
  }, error = function(e) {
    stop(sprintf("Error processing dataset: %s", e$message))
  })
}


#' Combine batch screening results into a single data frame
#'
#' @param batch_results List output from screen_mz_batch()
#' @return Combined data frame with all EIC data
combine_batch_results <- function(batch_results) {
  do.call(rbind, batch_results)
}


#' Process screening results: filter samples, reorder by date, and get max intensities
#'
#' Convenience function that combines filter_samples(), reorder_by_date(), and
#' get_max_intensities() into a single call.
#'
#' @param batch_results List output from screen_mz_batch()
#' @param metadata Data frame with columns: name, year, month (from load_metadata())
#' @param exclude_patterns Character vector of patterns to exclude (default: "blanc", "reference", "pool", "cal", "SST")
#' @param reorder Logical, whether to reorder by date (default TRUE). Requires metadata.
#' @return Data frame with max intensities for filtered and ordered samples
process_screening_results <- function(batch_results,
                                       metadata = NULL,
                                       exclude_patterns = c("blanc", "reference", "pool", "cal", "SST"),
                                       reorder = TRUE) {
  
  # Step 1: Filter out blanks, references, etc.
  filtered <- filter_samples(batch_results, exclude_patterns = exclude_patterns)
  
  # Step 2: Reorder by date if metadata is provided
  if (reorder && !is.null(metadata)) {
    filtered <- reorder_by_date(filtered, metadata)
  } else if (reorder && is.null(metadata)) {
    message("Note: metadata not provided, skipping date reordering")
  }
  
  # Step 3: Get max intensities
  max_df <- get_max_intensities(filtered)
  
  # Step 4: Remove samples with zero max intensity
  n_before <- nrow(max_df)
  max_df <- max_df[max_df$max_intensity > 0, ]
  n_removed <- n_before - nrow(max_df)
  if (n_removed > 0) {
    message(sprintf("Removed %d samples with zero intensity", n_removed))
  }
  
  max_df
}


#' Plot EIC overlay for all files from batch screening
#'
#' @param batch_results List output from screen_mz_batch()
#' @param title Optional plot title
#' @param show_legend Show legend (default TRUE)
#' @param max_samples Maximum number of samples to display. If NULL (default), show all.
#'   If set, samples are evenly distributed across the dataset.
#' @return ggplot object
plot_eic_overlay <- function(batch_results, title = NULL, show_legend = TRUE, max_samples = NULL) {
  
  # Subsample if max_samples is specified
  if (!is.null(max_samples) && length(batch_results) > max_samples) {
    n_total <- length(batch_results)
    # Select evenly distributed samples
    indices <- round(seq(1, n_total, length.out = max_samples))
    batch_results <- batch_results[indices]
    message(sprintf("Showing %d/%d samples (evenly distributed)", max_samples, n_total))
  }
  
  combined_df <- combine_batch_results(batch_results)
  
  if (nrow(combined_df) == 0) {
    message("No data found for the target m/z")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  target_mz <- combined_df$target_mz[1]
  
  if (is.null(title)) {
    title <- sprintf("EIC Overlay - m/z %.4f", target_mz)
  }
  
  p <- ggplot(combined_df, aes(x = rt, y = intensity, color = file)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = title,
      x = "Retention Time (min)",
      y = "Intensity",
      color = "Sample"
    ) +
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  p
}


#' Plot EIC overlay comparing two molecules
#'
#' Combines EICs from two screening results, coloring all traces from each molecule
#' with a distinct color for easy comparison.
#'
#' @param batch_results_1 List output from screen_mz_batch() for molecule 1
#' @param batch_results_2 List output from screen_mz_batch() for molecule 2
#' @param name_1 Name for molecule 1 (default "Molecule 1")
#' @param name_2 Name for molecule 2 (default "Molecule 2")
#' @param colors Colors for the two molecules (default c("#00AFBB", "#FC4E07"))
#' @param alpha Transparency for lines (default 0.5)
#' @param title Optional plot title
#' @param show_legend Show legend (default TRUE)
#' @param max_samples Maximum number of samples per molecule. If NULL (default), show all.
#' @param xlim Optional numeric vector of length 2 for x-axis limits (RT range), e.g., c(5, 15)
#' @return ggplot object
plot_eic_overlay_compare <- function(batch_results_1,
                                      batch_results_2,
                                      name_1 = "Molecule 1",
                                      name_2 = "Molecule 2",
                                      colors = c("#00AFBB", "#FC4E07"),
                                      alpha = 0.5,
                                      title = NULL,
                                      show_legend = TRUE,
                                      max_samples = NULL,
                                      xlim = NULL) {
  
  # Subsample if max_samples is specified
  if (!is.null(max_samples)) {
    if (length(batch_results_1) > max_samples) {
      n_total <- length(batch_results_1)
      indices <- round(seq(1, n_total, length.out = max_samples))
      batch_results_1 <- batch_results_1[indices]
    }
    if (length(batch_results_2) > max_samples) {
      n_total <- length(batch_results_2)
      indices <- round(seq(1, n_total, length.out = max_samples))
      batch_results_2 <- batch_results_2[indices]
    }
    message(sprintf("Showing up to %d samples per molecule", max_samples))
  }
  
  # Combine results for each molecule
  df1 <- combine_batch_results(batch_results_1)
  df2 <- combine_batch_results(batch_results_2)
  
  if (nrow(df1) == 0 && nrow(df2) == 0) {
    message("No data found for either molecule")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  # Add molecule labels
  if (nrow(df1) > 0) df1$molecule <- name_1
  if (nrow(df2) > 0) df2$molecule <- name_2
  
  # Combine into single data frame
  combined_df <- rbind(df1, df2)
  combined_df$molecule <- factor(combined_df$molecule, levels = c(name_1, name_2))
  
  # Create unique group for each file+molecule combination
  combined_df$group <- paste(combined_df$molecule, combined_df$file, sep = "_")
  
  # Get target m/z values for title
  mz1 <- if (nrow(df1) > 0) df1$target_mz[1] else NA
  mz2 <- if (nrow(df2) > 0) df2$target_mz[1] else NA
  
  if (is.null(title)) {
    if (!is.na(mz1) && !is.na(mz2)) {
      title <- sprintf("%s (m/z %.4f) vs %s (m/z %.4f)", name_1, mz1, name_2, mz2)
    } else {
      title <- sprintf("%s vs %s", name_1, name_2)
    }
  }
  
  p <- ggplot(combined_df, aes(x = rt, y = intensity, color = molecule, group = group)) +
    geom_line(linewidth = 0.6, alpha = alpha) +
    scale_color_manual(values = setNames(colors, c(name_1, name_2))) +
    labs(
      title = title,
      x = "Retention Time (min)",
      y = "Intensity",
      color = "Molecule"
    ) +
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  # Apply x-axis limits if specified
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }
  
  p
}


#' Plot faceted EIC for each file separately
#'
#' @param batch_results List output from screen_mz_batch()
#' @param ncol Number of columns in facet grid
#' @param title Optional main title
#' @return ggplot object
plot_eic_faceted <- function(batch_results, ncol = 2, title = NULL) {
  
  combined_df <- combine_batch_results(batch_results)
  
  if (nrow(combined_df) == 0) {
    message("No data found for the target m/z")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  target_mz <- combined_df$target_mz[1]
  
  if (is.null(title)) {
    title <- sprintf("EIC - m/z %.4f", target_mz)
  }
  
  p <- ggplot(combined_df, aes(x = rt, y = intensity)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_area(fill = "steelblue", alpha = 0.3) +
    facet_wrap(~file, ncol = ncol, scales = "free_y") +
    labs(
      title = title,
      x = "Retention Time (min)",
      y = "Intensity"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
  
  p
}


#' Screen for multiple molecules across multiple files
#'
#' @param file_paths Character vector of parquet file paths
#' @param molecules Data frame with columns: name, mz (target m/z values)
#' @param mz_tolerance Mass tolerance in Da
#' @param rt_range RT range in minutes
#' @param mslevel MS level
#' @param min_intensity Minimum intensity
#' @return Named list of batch results, one per molecule
screen_multiple_molecules <- function(file_paths,
                                      molecules,
                                      mz_tolerance = 0.01,
                                      rt_range = c(0, 30),
                                      mslevel = "1",
                                      min_intensity = 0) {
  
  results <- lapply(seq_len(nrow(molecules)), function(i) {
    mol <- molecules[i, ]
    message(sprintf("Screening for %s (m/z %.4f)...", mol$name, mol$mz))
    
    batch_result <- screen_mz_batch(
      file_paths = file_paths,
      target_mz = mol$mz,
      mz_tolerance = mz_tolerance,
      rt_range = rt_range,
      mslevel = mslevel,
      min_intensity = min_intensity
    )
    
    # Add molecule name to results
    for (j in seq_along(batch_result)) {
      if (nrow(batch_result[[j]]) > 0) {
        batch_result[[j]]$molecule <- mol$name
      }
    }
    
    batch_result
  })
  
  names(results) <- molecules$name
  results
}


#' Generate summary statistics for batch screening results
#'
#' @param batch_results List output from screen_mz_batch()
#' @return Data frame with summary statistics per file
summarize_batch_results <- function(batch_results) {
  
  summaries <- lapply(names(batch_results), function(file_name) {
    df <- batch_results[[file_name]]
    
    if (nrow(df) == 0) {
      return(data.frame(
        file = file_name,
        detected = FALSE,
        max_intensity = NA,
        rt_at_max = NA,
        n_points = 0,
        auc = NA
      ))
    }
    
    max_idx <- which.max(df$intensity)
    
    # Calculate AUC using trapezoidal rule
    if (nrow(df) > 1) {
      df_sorted <- df[order(df$rt), ]
      auc <- sum(diff(df_sorted$rt) * (head(df_sorted$intensity, -1) + tail(df_sorted$intensity, -1)) / 2)
    } else {
      auc <- df$intensity[1]
    }
    
    data.frame(
      file = file_name,
      detected = TRUE,
      max_intensity = max(df$intensity),
      rt_at_max = df$rt[max_idx],
      n_points = nrow(df),
      auc = auc
    )
  })
  
  do.call(rbind, summaries)
}


#' Plot summary heatmap for multiple molecules across samples
#'
#' @param multi_results Output from screen_multiple_molecules()
#' @param metric Which metric to display: "max_intensity", "auc", "detected"
#' @return ggplot heatmap
plot_screening_heatmap <- function(multi_results, metric = "max_intensity") {
  
  # Build summary for all molecules
  all_summaries <- lapply(names(multi_results), function(mol_name) {
    summary_df <- summarize_batch_results(multi_results[[mol_name]])
    summary_df$molecule <- mol_name
    summary_df
  })
  
  combined <- do.call(rbind, all_summaries)
  
  # Create heatmap based on metric
  if (metric == "detected") {
    combined$value <- as.numeric(combined$detected)
    fill_label <- "Detected"
    fill_scale <- scale_fill_gradient(low = "white", high = "forestgreen")
  } else if (metric == "auc") {
    combined$value <- log10(combined$auc + 1)
    fill_label <- "log10(AUC)"
    fill_scale <- scale_fill_viridis_c(option = "plasma", na.value = "grey90")
  } else {
    combined$value <- log10(combined$max_intensity + 1)
    fill_label <- "log10(Max Intensity)"
    fill_scale <- scale_fill_viridis_c(option = "viridis", na.value = "grey90")
  }
  
  p <- ggplot(combined, aes(x = file, y = molecule, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    fill_scale +
    labs(
      title = "Screening Results Heatmap",
      x = "Sample",
      y = "Molecule",
      fill = fill_label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid = element_blank()
    )
  
  p
}


#' Interactive EIC plot using plotly
#'
#' @param batch_results List output from screen_mz_batch()
#' @param title Optional plot title
#' @param max_samples Maximum number of samples to display. If NULL (default), show all.
#'   If set, samples are evenly distributed across the dataset.
#' @return plotly object
plot_eic_interactive <- function(batch_results, title = NULL, max_samples = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  # Subsample if max_samples is specified
  if (!is.null(max_samples) && length(batch_results) > max_samples) {
    n_total <- length(batch_results)
    # Select evenly distributed samples
    indices <- round(seq(1, n_total, length.out = max_samples))
    batch_results <- batch_results[indices]
    message(sprintf("Showing %d/%d samples (evenly distributed)", max_samples, n_total))
  }
  
  combined_df <- combine_batch_results(batch_results)
  
  if (nrow(combined_df) == 0) {
    return(plotly::plot_ly() |> 
             plotly::layout(title = "No data found"))
  }
  
  target_mz <- combined_df$target_mz[1]
  
  if (is.null(title)) {
    title <- sprintf("EIC - m/z %.4f", target_mz)
  }
  
  p <- plotly::plot_ly()
  
  files <- unique(combined_df$file)
  colors <- grDevices::hcl.colors(length(files), palette = "Dark 2")
  
  for (i in seq_along(files)) {
    file_data <- combined_df[combined_df$file == files[i], ]
    p <- p |>
      plotly::add_trace(
        data = file_data,
        x = ~rt,
        y = ~intensity,
        type = "scatter",
        mode = "lines",
        name = files[i],
        line = list(color = colors[i], width = 2),
        hovertemplate = paste0(
          "<b>", files[i], "</b><br>",
          "RT: %{x:.2f} min<br>",
          "Intensity: %{y:.0f}<extra></extra>"
        )
      )
  }
  
  p <- p |>
    plotly::layout(
      title = list(text = title, x = 0.5),
      xaxis = list(title = "Retention Time (min)"),
      yaxis = list(title = "Intensity"),
      hovermode = "closest"
    )
  
  p
}


#' Interactive EIC plot comparing two molecules using plotly
#'
#' Combines EICs from two screening results, coloring all traces from each molecule
#' with a distinct color for easy comparison.
#'
#' @param batch_results_1 List output from screen_mz_batch() for molecule 1
#' @param batch_results_2 List output from screen_mz_batch() for molecule 2
#' @param name_1 Name for molecule 1 (default "Molecule 1")
#' @param name_2 Name for molecule 2 (default "Molecule 2")
#' @param colors Colors for the two molecules (default c("#00AFBB", "#FC4E07"))
#' @param alpha Transparency for lines (0-1, default 0.5)
#' @param title Optional plot title
#' @param max_samples Maximum number of samples per molecule. If NULL (default), show all.
#' @return plotly object
plot_eic_interactive_compare <- function(batch_results_1,
                                          batch_results_2,
                                          name_1 = "Molecule 1",
                                          name_2 = "Molecule 2",
                                          colors = c("#00AFBB", "#FC4E07"),
                                          alpha = 0.5,
                                          title = NULL,
                                          max_samples = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  # Subsample if max_samples is specified
  if (!is.null(max_samples)) {
    if (length(batch_results_1) > max_samples) {
      n_total <- length(batch_results_1)
      indices <- round(seq(1, n_total, length.out = max_samples))
      batch_results_1 <- batch_results_1[indices]
    }
    if (length(batch_results_2) > max_samples) {
      n_total <- length(batch_results_2)
      indices <- round(seq(1, n_total, length.out = max_samples))
      batch_results_2 <- batch_results_2[indices]
    }
    message(sprintf("Showing up to %d samples per molecule", max_samples))
  }
  
  # Combine results for each molecule
  df1 <- combine_batch_results(batch_results_1)
  df2 <- combine_batch_results(batch_results_2)
  
  if (nrow(df1) == 0 && nrow(df2) == 0) {
    return(plotly::plot_ly() |> 
             plotly::layout(title = "No data found"))
  }
  
  # Get target m/z values for title
  mz1 <- if (nrow(df1) > 0) df1$target_mz[1] else NA
  mz2 <- if (nrow(df2) > 0) df2$target_mz[1] else NA
  
  if (is.null(title)) {
    if (!is.na(mz1) && !is.na(mz2)) {
      title <- sprintf("%s (m/z %.4f) vs %s (m/z %.4f)", name_1, mz1, name_2, mz2)
    } else {
      title <- sprintf("%s vs %s", name_1, name_2)
    }
  }
  
  p <- plotly::plot_ly()
  
  # Add traces for molecule 1
  if (nrow(df1) > 0) {
    files1 <- unique(df1$file)
    for (i in seq_along(files1)) {
      file_data <- df1[df1$file == files1[i], ]
      p <- p |>
        plotly::add_trace(
          data = file_data,
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "lines",
          name = name_1,
          legendgroup = name_1,
          showlegend = (i == 1),  # Only show legend for first trace
          line = list(color = colors[1], width = 1.5),
          opacity = alpha,
          hovertemplate = paste0(
            "<b>", name_1, "</b><br>",
            "File: ", files1[i], "<br>",
            "RT: %{x:.2f} min<br>",
            "Intensity: %{y:.0f}<extra></extra>"
          )
        )
    }
  }
  
  # Add traces for molecule 2
  if (nrow(df2) > 0) {
    files2 <- unique(df2$file)
    for (i in seq_along(files2)) {
      file_data <- df2[df2$file == files2[i], ]
      p <- p |>
        plotly::add_trace(
          data = file_data,
          x = ~rt,
          y = ~intensity,
          type = "scatter",
          mode = "lines",
          name = name_2,
          legendgroup = name_2,
          showlegend = (i == 1),  # Only show legend for first trace
          line = list(color = colors[2], width = 1.5),
          opacity = alpha,
          hovertemplate = paste0(
            "<b>", name_2, "</b><br>",
            "File: ", files2[i], "<br>",
            "RT: %{x:.2f} min<br>",
            "Intensity: %{y:.0f}<extra></extra>"
          )
        )
    }
  }
  
  p <- p |>
    plotly::layout(
      title = list(text = title, x = 0.5),
      xaxis = list(title = "Retention Time (min)"),
      yaxis = list(title = "Intensity"),
      hovermode = "closest"
    )
  
  p
}


#' Extract max intensity values from batch screening results
#'
#' @param batch_results List output from screen_mz_batch()
#' @return Data frame with file names and their max intensities
get_max_intensities <- function(batch_results) {
  
  # If already a data frame with expected columns, return as-is
  if (is.data.frame(batch_results)) {
    if (all(c("file", "max_intensity") %in% colnames(batch_results))) {
      return(batch_results)
    }
  }
  
  max_vals <- lapply(names(batch_results), function(file_name) {
    df <- batch_results[[file_name]]
    
    if (nrow(df) == 0) {
      return(data.frame(
        file = file_name,
        max_intensity = 0,
        rt_at_max = NA,
        target_mz = NA
      ))
    }
    
    max_idx <- which.max(df$intensity)

    data.frame(
      file = file_name,
      max_intensity = df$intensity[max_idx],
      rt_at_max = df$rt[max_idx],
      target_mz = df$target_mz[1]
    )
  })
  
  do.call(rbind, max_vals)
}


#' Normalize molecule intensities by internal standard
#'
#' @param max_df_molecule Data frame with max intensities for the target molecule
#' @param max_df_istd Data frame with max intensities for the internal standard
#' @param molecule_name Name of the target molecule (optional)
#' @param istd_name Name of the internal standard (optional)
#' @return Data frame with normalized intensities (ratio molecule/ISTD)
normalize_by_istd <- function(max_df_molecule, 
                              max_df_istd,
                              molecule_name = "Molecule",
                              istd_name = "ISTD") {
  
  # Ensure both are data frames
  df_mol <- get_max_intensities(max_df_molecule)
  df_istd <- get_max_intensities(max_df_istd)
  
  # Merge by file name
  merged <- merge(
    df_mol, 
    df_istd, 
    by = "file", 
    suffixes = c("_mol", "_istd"),
    all.x = TRUE
  )
  
  # Calculate normalized intensity (ratio)
  merged$normalized_intensity <- merged$max_intensity_mol / merged$max_intensity_istd
  
  # Handle division by zero or NA
 merged$normalized_intensity[is.infinite(merged$normalized_intensity)] <- NA
  merged$normalized_intensity[merged$max_intensity_istd == 0] <- NA
  
  # Create output data frame compatible with plotting functions
  result <- data.frame(
    file = merged$file,
    max_intensity = merged$normalized_intensity,
    max_intensity_mol = merged$max_intensity_mol,
    max_intensity_istd = merged$max_intensity_istd,
    rt_at_max = merged$rt_at_max_mol,
    rt_at_max_istd = merged$rt_at_max_istd,
    target_mz = merged$target_mz_mol,
    target_mz_istd = merged$target_mz_istd,
    molecule = molecule_name,
    istd = istd_name,
    stringsAsFactors = FALSE
  )
  
  # Preserve row order from original molecule df
  result <- result[match(df_mol$file, result$file), ]
  rownames(result) <- NULL
  
  n_valid <- sum(!is.na(result$max_intensity))
  message(sprintf("Normalized %d/%d samples (%s / %s)", 
                  n_valid, nrow(result), molecule_name, istd_name))
  
  # Remove rows with NA or zero normalized intensity
  n_removed <- sum(is.na(result$max_intensity) | result$max_intensity == 0)
  if (n_removed > 0) {
    result <- result[!is.na(result$max_intensity) & result$max_intensity != 0, ]
    message(sprintf("Removed %d samples with NA or zero normalized intensity", n_removed))
  }
  
  result
}


#' Create combined max_df from multiple molecules for comparison
#'
#' @param ... Named arguments: max_df data frames (e.g., Caffeine = max_df1, Nicotine = max_df2)
#' @return Combined data frame with molecule column
combine_molecules <- function(...) {
  
  dfs <- list(...)
  
  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("All arguments must be named (e.g., Caffeine = max_df1)")
  }
  
  combined <- lapply(names(dfs), function(mol_name) {
    df <- get_max_intensities(dfs[[mol_name]])
    df$molecule <- mol_name
    df
  })
  
  do.call(rbind, combined)
}


#' Extract max intensities for multiple molecules
#'
#' @param multi_results Output from screen_multiple_molecules()
#' @return Data frame with molecule, file, and max intensity
get_multi_max_intensities <- function(multi_results) {
  
  all_max <- lapply(names(multi_results), function(mol_name) {
    max_df <- get_max_intensities(multi_results[[mol_name]])
    max_df$molecule <- mol_name
    max_df
  })
  
  do.call(rbind, all_max)
}


#' Create a stripes chart for a single molecule across samples
#'
#' @param batch_results List output from screen_mz_batch() OR a data frame from get_max_intensities()
#' @param title Optional plot title
#' @param color_palette Color palette: "viridis", "plasma", "inferno", "magma", "divergent", "spectral"
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param show_labels Show sample labels on x-axis (default TRUE)
#' @param x_labels What to show on x-axis: "file" (default), "date" (year-month), "year", or "none"
#' @param metadata Optional data frame with columns: name, year, month (used when x_labels="date" or "year")
#' @return ggplot object
plot_stripes <- function(batch_results, 
                         title = NULL, 
                         color_palette = "viridis",
                         log_scale = TRUE,
                         show_labels = TRUE,
                         x_labels = "file",
                         metadata = NULL) {
  
  # Handle both list (batch_results) and data frame (max_df) input
  df <- get_max_intensities(batch_results)
  
  if (nrow(df) == 0 || all(df$max_intensity == 0)) {
    message("No data found for stripes chart")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  # Create sample index for ordering (preserve existing order)
  df$sample_idx <- seq_len(nrow(df))
  
  # Extract year-month or year from metadata/filenames if needed
  if (x_labels %in% c("date", "year")) {
    if (!is.null(metadata)) {
      # Use metadata year/month columns
      df$date_label <- get_date_labels_from_metadata(df$file, metadata)
    } else {
      # Fall back to extracting from file names
      df$date_label <- extract_date_label(df$file)
    }
    # For year only, extract just the year part
    if (x_labels == "year") {
      df$date_label <- sub("-.*", "", df$date_label)
    }
  }
  
  # Apply log transformation if requested
  if (log_scale) {
    df$plot_intensity <- log10(df$max_intensity + 1)
    intensity_label <- "log10(Intensity)"
  } else {
    df$plot_intensity <- df$max_intensity
    intensity_label <- "Intensity"
  }
  
  target_mz <- df$target_mz[1]
  
  if (is.null(title)) {
    if (!is.na(target_mz)) {
      title <- sprintf("Stripes Chart - m/z %.4f", target_mz)
    } else {
      title <- "Stripes Chart"
    }
  }
  
  # Build the plot
  p <- ggplot(df, aes(x = sample_idx, y = 1, fill = plot_intensity)) +
    geom_tile(width = 1, height = 1)
  
  # Apply color palette
  p <- p + switch(color_palette,
    "viridis" = scale_fill_viridis_c(option = "viridis", name = intensity_label),
    "plasma" = scale_fill_viridis_c(option = "plasma", name = intensity_label),
    "inferno" = scale_fill_viridis_c(option = "inferno", name = intensity_label),
    "magma" = scale_fill_viridis_c(option = "magma", name = intensity_label),
    "divergent" = scale_fill_gradient2(
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = median(df$plot_intensity),
      name = intensity_label
    ),
    "spectral" = scale_fill_gradientn(
      colors = c("#313695", "#4575b4", "#abd9e9", "#ffffbf", 
                 "#fee090", "#f46d43", "#d73027", "#a50026"),
      name = intensity_label
    ),
    scale_fill_viridis_c(option = "viridis", name = intensity_label)
  )
  
  # Add x-axis labels based on x_labels parameter
  if (show_labels && x_labels != "none") {
    if (x_labels %in% c("date", "year")) {
      # Show only unique date/year labels at their first occurrence
      unique_dates <- !duplicated(df$date_label)
      p <- p + 
        scale_x_continuous(
          breaks = df$sample_idx[unique_dates], 
          labels = df$date_label[unique_dates]
        )
    } else {
      p <- p + 
        scale_x_continuous(breaks = df$sample_idx, labels = df$file)
    }
    # Use horizontal labels for year, angled for others
    x_angle <- if (x_labels == "year") 0 else 45
    x_hjust <- if (x_labels == "year") 0.5 else 1
    p <- p +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust, size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  } else {
    p <- p +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  }
  
  p + labs(title = title)
}


#' Create a multi-molecule stripes chart (heatmap style)
#'
#' @param multi_results Output from screen_multiple_molecules()
#' @param title Optional plot title
#' @param color_palette Color palette name
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param cluster_molecules Cluster molecules by intensity pattern (default FALSE
#' @param cluster_samples Cluster samples by intensity pattern (default FALSE)
#' @return ggplot object
plot_multi_stripes <- function(multi_results,
                               title = NULL,
                               color_palette = "viridis",
                               log_scale = TRUE,
                               cluster_molecules = FALSE,
                               cluster_samples = FALSE) {
  
  max_df <- get_multi_max_intensities(multi_results)
  
  if (nrow(max_df) == 0 || all(max_df$max_intensity == 0)) {
    message("No data found for stripes chart")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  # Apply log transformation if requested
  if (log_scale) {
    max_df$plot_intensity <- log10(max_df$max_intensity + 1)
    intensity_label <- "log10(Intensity)"
  } else {
    max_df$plot_intensity <- max_df$max_intensity
    intensity_label <- "Intensity"
  }
  
  # Optional clustering
  if (cluster_molecules || cluster_samples) {
    # Create wide matrix for clustering
    wide_mat <- reshape(
      max_df[, c("molecule", "file", "plot_intensity")],
      idvar = "molecule",
      timevar = "file",
      direction = "wide"
    )
    rownames(wide_mat) <- wide_mat$molecule
    wide_mat <- wide_mat[, -1, drop = FALSE]
    wide_mat[is.na(wide_mat)] <- 0
    
    if (cluster_molecules && nrow(wide_mat) > 1) {
      mol_order <- hclust(dist(wide_mat))$order
      max_df$molecule <- factor(max_df$molecule, 
                                levels = rownames(wide_mat)[mol_order])
    }
    
    if (cluster_samples && ncol(wide_mat) > 1) {
      sample_order <- hclust(dist(t(wide_mat)))$order
      sample_names <- gsub("plot_intensity\\.", "", colnames(wide_mat))
      max_df$file <- factor(max_df$file, levels = sample_names[sample_order])
    }
  }
  
  if (is.null(title)) {
    title <- "Multi-Molecule Stripes Chart"
  }
  
  # Build the plot
  p <- ggplot(max_df, aes(x = file, y = molecule, fill = plot_intensity)) +
    geom_tile(color = "white", linewidth = 0.3)
  
  # Apply color palette
  p <- p + switch(color_palette,
    "viridis" = scale_fill_viridis_c(option = "viridis", name = intensity_label, na.value = "grey90"),
    "plasma" = scale_fill_viridis_c(option = "plasma", name = intensity_label, na.value = "grey90"),
    "inferno" = scale_fill_viridis_c(option = "inferno", name = intensity_label, na.value = "grey90"),
    "magma" = scale_fill_viridis_c(option = "magma", name = intensity_label, na.value = "grey90"),
    "divergent" = scale_fill_gradient2(
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = median(max_df$plot_intensity, na.rm = TRUE),
      name = intensity_label, na.value = "grey90"
    ),
    "spectral" = scale_fill_gradientn(
      colors = c("#313695", "#4575b4", "#abd9e9", "#ffffbf", 
                 "#fee090", "#f46d43", "#d73027", "#a50026"),
      name = intensity_label, na.value = "grey90"
    ),
    scale_fill_viridis_c(option = "viridis", name = intensity_label, na.value = "grey90")
  )
  
  p <- p +
    labs(title = title, x = "Sample", y = "Molecule") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  p
}


#' Plot max intensities as a line plot across samples
#'
#' @param batch_results List output from screen_mz_batch() OR a data frame from get_max_intensities()
#' @param title Optional plot title
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param show_points Show points on the line (default TRUE)
#' @param color Line color (default "steelblue")
#' @param x_labels What to show on x-axis: "file" (default), "date" (year-month), "year", or "none"
#' @param metadata Optional data frame with columns: name, year, month (used when x_labels="date" or "year")
#' @return ggplot object
plot_intensity_line <- function(batch_results,
                                title = NULL,
                                log_scale = TRUE,
                                show_points = TRUE,
                                color = "steelblue",
                                x_labels = "file",
                                metadata = NULL) {
  
  # Handle both list (batch_results) and data frame (max_df) input
  df <- get_max_intensities(batch_results)
  
  if (nrow(df) == 0 || all(df$max_intensity == 0)) {
    message("No data found")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data found") +
             theme_void())
  }
  
  df$sample_idx <- seq_len(nrow(df))
  
  # Extract year-month or year from metadata/filenames if needed
  if (x_labels %in% c("date", "year")) {
    if (!is.null(metadata)) {
      df$date_label <- get_date_labels_from_metadata(df$file, metadata)
    } else {
      df$date_label <- extract_date_label(df$file)
    }
    # For year only, extract just the year part
    if (x_labels == "year") {
      df$date_label <- sub("-.*", "", df$date_label)
    }
  }
  
  if (log_scale) {
    df$plot_intensity <- log10(df$max_intensity + 1)
    y_label <- "log10(Max Intensity)"
  } else {
    df$plot_intensity <- df$max_intensity
    y_label <- "Max Intensity"
  }
  
  target_mz <- df$target_mz[1]
  
  if (is.null(title)) {
    if (!is.na(target_mz)) {
      title <- sprintf("Max Intensity across samples - m/z %.4f", target_mz)
    } else {
      title <- "Max Intensity across samples"
    }
  }
  
  p <- ggplot(df, aes(x = sample_idx, y = plot_intensity)) +
    geom_line(color = color, linewidth = 0.8)
  
  if (show_points) {
    p <- p + geom_point(color = color, size = 2)
  }
  
  # Set x-axis labels based on x_labels parameter
  if (x_labels %in% c("date", "year")) {
    unique_dates <- !duplicated(df$date_label)
    p <- p + scale_x_continuous(
      breaks = df$sample_idx[unique_dates], 
      labels = df$date_label[unique_dates]
    )
  } else if (x_labels == "none") {
    p <- p + scale_x_continuous(breaks = NULL)
  } else {
    p <- p + scale_x_continuous(breaks = df$sample_idx, labels = df$file)
  }
  
  p <- p +
    labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  p
}


#' Interactive line plot of max intensities using plotly
#'
#' @param batch_results List output from screen_mz_batch() OR a data frame from get_max_intensities()
#' @param title Optional plot title
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param color Line color (default "steelblue")
#' @param x_labels What to show on x-axis: "file" (default), "date" (year-month), "year", or "none"
#' @param metadata Optional data frame with columns: name, year, month (used when x_labels="date" or "year")
#' @return plotly object
plot_intensity_line_interactive <- function(batch_results,
                                            title = NULL,
                                            log_scale = TRUE,
                                            color = "steelblue",
                                            x_labels = "file",
                                            metadata = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  # Handle both list (batch_results) and data frame (max_df) input
  df <- get_max_intensities(batch_results)
  
  if (nrow(df) == 0 || all(df$max_intensity == 0)) {
    return(plotly::plot_ly() |> 
             plotly::layout(title = "No data found"))
  }
  
  df$sample_idx <- seq_len(nrow(df))
  
  # Extract year-month or year from metadata/filenames if needed
  if (x_labels %in% c("date", "year")) {
    if (!is.null(metadata)) {
      df$date_label <- get_date_labels_from_metadata(df$file, metadata)
    } else {
      df$date_label <- extract_date_label(df$file)
    }
    # For year only, extract just the year part
    if (x_labels == "year") {
      df$date_label <- sub("-.*", "", df$date_label)
    }
  }
  
  if (log_scale) {
    df$plot_intensity <- log10(df$max_intensity + 1)
    y_label <- "log10(Max Intensity)"
  } else {
    df$plot_intensity <- df$max_intensity
    y_label <- "Max Intensity"
  }
  
  target_mz <- df$target_mz[1]
  
  if (is.null(title)) {
    if (!is.na(target_mz)) {
      title <- sprintf("Max Intensity across samples - m/z %.4f", target_mz)
    } else {
      title <- "Max Intensity across samples"
    }
  }
  
  p <- plotly::plot_ly(
    data = df,
    x = ~sample_idx,
    y = ~plot_intensity,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = color, width = 2),
    marker = list(color = color, size = 8),
    text = ~sprintf(
      "<b>%s</b><br>Max Intensity: %.0f<br>RT at max: %.2f min",
      file, max_intensity, rt_at_max
    ),
    hoverinfo = "text"
  )
  
  # Set x-axis labels based on x_labels parameter
  if (x_labels %in% c("date", "year")) {
    unique_dates <- !duplicated(df$date_label)
    tick_vals <- df$sample_idx[unique_dates]
    tick_text <- df$date_label[unique_dates]
  } else if (x_labels == "none") {
    tick_vals <- NULL
    tick_text <- NULL
  } else {
    tick_vals <- df$sample_idx
    tick_text <- df$file
  }
  
  p <- p |>
    plotly::layout(
      title = list(text = title, x = 0.5),
      xaxis = list(
        title = "",
        tickvals = tick_vals,
        ticktext = tick_text,
        tickangle = 45
      ),
      yaxis = list(title = y_label),
      hovermode = "closest"
    )
  
  p
}


#' Create an interactive stripes chart using plotly
#'
#' @param batch_results List output from screen_mz_batch()
#' @param title Optional plot title
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param x_labels What to show on x-axis: "file" (default), "date" (year-month), "year", or "none"
#' @param metadata Optional data frame with columns: name, year, month (used when x_labels="date" or "year")
#' @return plotly object
plot_stripes_interactive <- function(batch_results, 
                                     title = NULL,
                                     log_scale = TRUE,
                                     x_labels = "file",
                                     metadata = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  max_df <- get_max_intensities(batch_results)
  
  if (nrow(max_df) == 0 || all(max_df$max_intensity == 0)) {
    return(plotly::plot_ly() |> 
             plotly::layout(title = "No data found"))
  }
  
  max_df$sample_idx <- seq_len(nrow(max_df))
  
  # Extract year-month or year labels if needed
  if (x_labels %in% c("date", "year")) {
    if (!is.null(metadata)) {
      max_df$date_label <- get_date_labels_from_metadata(max_df$file, metadata)
    } else {
      max_df$date_label <- extract_date_label(max_df$file)
    }
    # For year only, extract just the year part
    if (x_labels == "year") {
      max_df$date_label <- sub("-.*", "", max_df$date_label)
    }
  }
  
  if (log_scale) {
    max_df$plot_intensity <- log10(max_df$max_intensity + 1)
    colorbar_title <- "log10(Intensity)"
  } else {
    max_df$plot_intensity <- max_df$max_intensity
    colorbar_title <- "Intensity"
  }
  
  target_mz <- max_df$target_mz[1]
  
  if (is.null(title)) {
    title <- sprintf("Stripes Chart - m/z %.4f", target_mz)
  }
  
  n_samples <- nrow(max_df)
  
  # Get viridis colors mapped to intensities
  intensity_range <- range(max_df$plot_intensity, na.rm = TRUE)
  normalized <- (max_df$plot_intensity - intensity_range[1]) / 
                (intensity_range[2] - intensity_range[1] + 1e-10)
  colors <- viridis::viridis(100)[pmax(1, ceiling(normalized * 100))]
  
  # Create shapes for rectangles (stripes)
  shapes <- lapply(seq_len(n_samples), function(i) {
    list(
      type = "rect",
      x0 = i - 0.5,
      x1 = i + 0.5,
      y0 = 0,
      y1 = 1,
      fillcolor = colors[i],
      line = list(width = 0),
      layer = "below"
    )
  })
  
  # Create invisible scatter for hover info
  p <- plotly::plot_ly(
    data = max_df,
    x = ~sample_idx,
    y = ~0.5,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 40,
      opacity = 0,
      color = ~plot_intensity,
      colorscale = "Viridis",
      showscale = TRUE,
      colorbar = list(title = colorbar_title)
    ),
    text = ~sprintf(
      "<b>%s</b><br>Max Intensity: %.0f<br>RT at max: %.2f min",
      file, max_intensity, rt_at_max
    ),
    hoverinfo = "text"
  )
  
  p <- p |>
    plotly::layout(
      title = list(text = title, x = 0.5),
      shapes = shapes,
      xaxis = list(
        title = "",
        showgrid = FALSE,
        tickvals = if (x_labels %in% c("date", "year")) {
          # Show only unique date/year labels
          unique_dates <- !duplicated(max_df$date_label)
          max_df$sample_idx[unique_dates]
        } else if (x_labels == "none") {
          NULL
        } else {
          seq_len(n_samples)
        },
        ticktext = if (x_labels %in% c("date", "year")) {
          unique_dates <- !duplicated(max_df$date_label)
          max_df$date_label[unique_dates]
        } else if (x_labels == "none") {
          NULL
        } else {
          max_df$file
        },
        tickangle = if (x_labels == "year") 0 else 45,
        range = c(0.5, n_samples + 0.5)
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE,
        showgrid = FALSE,
        range = c(0, 1),
        fixedrange = TRUE
      ),
      plot_bgcolor = "white"
    )
  
  p
}


#' Compare two molecules with box plots using ggpubr
#'
#' Creates side-by-side box plots comparing max intensities of two molecules,
#' with statistical comparison (t-test or Wilcoxon test).
#'
#' @param max_df_1 Data frame with max intensities for molecule 1 (from process_screening_results or get_max_intensities)
#' @param max_df_2 Data frame with max intensities for molecule 2
#' @param name_1 Name for molecule 1 (default "Molecule 1")
#' @param name_2 Name for molecule 2 (default "Molecule 2")
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param test Statistical test: "t.test", "wilcox.test", or "none" (default "wilcox.test")
#' @param paired Logical, whether to use paired test (default TRUE, assumes same samples)
#' @param title Optional plot title
#' @param colors Colors for the two groups (default c("#00AFBB", "#FC4E07"))
#' @param horizontal Logical, whether to display boxes horizontally (default FALSE). Useful for compact A4 layout.
#' @return ggplot object
plot_compare_boxplot <- function(max_df_1,
                                  max_df_2,
                                  name_1 = "Molecule 1",
                                  name_2 = "Molecule 2",
                                  log_scale = TRUE,
                                  test = "wilcox.test",
                                  paired = TRUE,
                                  title = NULL,
                                  colors = c("#00AFBB", "#FC4E07"),
                                  horizontal = FALSE) {
  
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package 'ggpubr' is required. Install with: install.packages('ggpubr')")
  }
  
  # Ensure we have data frames with max_intensity
  df1 <- get_max_intensities(max_df_1)
  df2 <- get_max_intensities(max_df_2)
  
  # Add molecule labels
  df1$molecule <- name_1
  df2$molecule <- name_2
  
  # Combine data
  combined <- rbind(
    df1[, c("file", "max_intensity", "molecule")],
    df2[, c("file", "max_intensity", "molecule")]
  )
  
  # Apply log transformation if requested
  if (log_scale) {
    combined$intensity <- log10(combined$max_intensity + 1)
    intensity_label <- "log10(Max Intensity)"
  } else {
    combined$intensity <- combined$max_intensity
    intensity_label <- "Max Intensity"
  }
  
  # Set factor levels to control order
  combined$molecule <- factor(combined$molecule, levels = c(name_1, name_2))
  
  # Default title
  if (is.null(title)) {
    title <- sprintf("Comparison: %s vs %s", name_1, name_2)
  }
  
  # For horizontal orientation, swap x and y
  if (horizontal) {
    p <- ggpubr::ggboxplot(
      combined,
      x = "molecule",
      y = "intensity",
      color = "molecule",
      palette = colors,
      add = "jitter",
      add.params = list(size = 1.5, alpha = 0.5),
      title = title,
      xlab = "",
      ylab = intensity_label,
      orientation = "horizontal"
    )
    # Adjust y-axis scale to start near minimum data value (like vertical graph)
    intensity_range <- range(combined$intensity, na.rm = TRUE)
    y_min <- max(0, intensity_range[1] - 0.1 * diff(intensity_range))
    y_max <- intensity_range[2] + 0.1 * diff(intensity_range)
    p <- p + ggplot2::scale_y_continuous(limits = c(y_min, y_max))
  } else {
    p <- ggpubr::ggboxplot(
      combined,
      x = "molecule",
      y = "intensity",
      color = "molecule",
      palette = colors,
      add = "jitter",
      add.params = list(size = 1.5, alpha = 0.5),
      title = title,
      xlab = "",
      ylab = intensity_label
    )
  }
  
  # Add statistical comparison if requested
  if (test != "none") {
    if (horizontal) {
      # For horizontal, adjust label positions
      p <- p + ggpubr::stat_compare_means(
        method = test,
        paired = paired,
        label = "p.signif",
        label.y = 1.5,
        label.x.npc = 0.95
      )
      p <- p + ggpubr::stat_compare_means(
        method = test,
        paired = paired,
        label = "p.format",
        label.y = 1.5,
        label.x.npc = 0.80,
        size = 3.5
      )
    } else {
      p <- p + ggpubr::stat_compare_means(
        method = test,
        paired = paired,
        label = "p.signif",
        label.x = 1.5,
        label.y.npc = 0.95
      )
      p <- p + ggpubr::stat_compare_means(
        method = test,
        paired = paired,
        label = "p.format",
        label.x = 1.5,
        label.y.npc = 0.88,
        size = 3.5
      )
    }
  }
  
  p <- p + theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )
  
  p
}


#' Compare multiple molecules with box plots using ggpubr
#'
#' Creates box plots comparing max intensities across multiple molecules,
#' with pairwise statistical comparisons.
#'
#' @param ... Named arguments: max_df data frames (e.g., Sulfamethoxazole = max_df1, Metabolite = max_df2)
#' @param log_scale Use log10 scale for intensity (default TRUE)
#' @param test Statistical test: "t.test", "wilcox.test", or "none" (default "wilcox.test")
#' @param paired Logical, whether to use paired test (default TRUE)
#' @param title Optional plot title
#' @param comparisons List of pairs to compare. If NULL, compares all pairs.
#' @return ggplot object
plot_compare_multi_boxplot <- function(...,
                                        log_scale = TRUE,
                                        test = "wilcox.test",
                                        paired = TRUE,
                                        title = NULL,
                                        comparisons = NULL) {
  
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("Package 'ggpubr' is required. Install with: install.packages('ggpubr')")
  }
  
  dfs <- list(...)
  
  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    stop("All arguments must be named (e.g., Molecule1 = max_df1, Molecule2 = max_df2)")
  }
  
  # Combine all data frames
  combined_list <- lapply(names(dfs), function(mol_name) {
    df <- get_max_intensities(dfs[[mol_name]])
    df$molecule <- mol_name
    df[, c("file", "max_intensity", "molecule")]
  })
  
  combined <- do.call(rbind, combined_list)
  
  # Apply log transformation if requested
  if (log_scale) {
    combined$intensity <- log10(combined$max_intensity + 1)
    y_label <- "log10(Max Intensity)"
  } else {
    combined$intensity <- combined$max_intensity
    y_label <- "Max Intensity"
  }
  
  # Set factor levels to preserve order
  combined$molecule <- factor(combined$molecule, levels = names(dfs))
  
  # Default title
  if (is.null(title)) {
    title <- "Molecule Comparison"
  }
  
  # Generate all pairwise comparisons if not specified
  if (is.null(comparisons) && length(names(dfs)) > 1) {
    comparisons <- combn(names(dfs), 2, simplify = FALSE)
  }
  
  # Create box plot
  p <- ggpubr::ggboxplot(
    combined,
    x = "molecule",
    y = "intensity",
    color = "molecule",
    add = "jitter",
    add.params = list(size = 1.5, alpha = 0.5),
    title = title,
    xlab = "",
    ylab = y_label
  )
  
  # Add statistical comparisons if requested
  if (test != "none" && !is.null(comparisons)) {
    p <- p + ggpubr::stat_compare_means(
      comparisons = comparisons,
      method = test,
      paired = paired,
      label = "p.signif"
    )
  }
  
  p <- p + theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
  
  p
}