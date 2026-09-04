#!/usr/bin/env Rscript

# Reusable Parquet queries for raw HRMS chromatograms.
# Usage in terminal (with right directory):
#   source("scripts/parquet_chromatograms.R")
#   Rscript scripts/parquet_chromatograms.R data/raw/parquet/test/pharma_PT6_replicate_1.parquet

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required. Install it with install.packages('", package, "').")
  }
}

# app/app.R and the tests source this helper before this file. Keep a safe
# fallback for direct use of this script before the helper has been sourced.
if (!exists("resolve_expected_drifttime_from_ccs", mode = "function", inherits = TRUE)) {
  resolve_expected_drifttime_from_ccs <- function(ccs, mz, calibration_parameters, converter = NULL) {
    list(
      ok = FALSE,
      expected_dt = NA_real_,
      C1 = NA_real_,
      C2 = NA_real_,
      status = "Non evalue: module CCS -> DT absent"
    )
  }
}

with_arrow_metadata <- function(expr) {
  old_options <- options("arrow.unsafe_metadata")
  on.exit(options(old_options), add = TRUE)
  options(arrow.unsafe_metadata = TRUE)
  suppressWarnings(force(expr))
}

open_hrms_dataset <- function(path) {
  require_package("arrow")
  if (is_remote_parquet_path(path)) {
    stop("Arrow cannot open an HTTP Parquet URL in this application. Use the DuckDB remote backend instead.")
  }
  if (!file.exists(path)) {
    stop("Parquet file not found: ", path)
  }
  with_arrow_metadata(arrow::open_dataset(path))
}

check_required_columns <- function(dataset, required_columns) {
  missing_columns <- setdiff(required_columns, names(dataset))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  invisible(TRUE)
}

collect_quietly <- function(query) {
  require_package("dplyr")
  # Convert while Arrow metadata is explicitly allowed so callers receive a
  # plain data frame without repeated metadata warnings.
  with_arrow_metadata(as.data.frame(dplyr::collect(query)))
}

is_remote_parquet_path <- function(path) {
  length(path) == 1 && !is.na(path) && grepl("^https?://", as.character(path), ignore.case = TRUE)
}

parquet_source_name <- function(path) {
  clean_path <- sub("[?#].*$", "", as.character(path[[1]]))
  basename(clean_path)
}

safe_parquet_source_path <- function(path) {
  if (is_remote_parquet_path(path)) {
    return(NA_character_)
  }
  normalizePath(path, mustWork = FALSE)
}

duckdb_numeric_literal <- function(value, name) {
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(value)) {
    stop(name, " must be a finite numeric value.")
  }
  format(value, scientific = FALSE, trim = TRUE, digits = 15)
}

load_duckdb_httpfs <- function(connection) {
  loaded <- tryCatch({
    DBI::dbExecute(connection, "LOAD httpfs")
    TRUE
  }, error = function(e) FALSE)
  if (loaded) {
    return(invisible(TRUE))
  }

  allow_install <- tolower(trimws(Sys.getenv("ALLOW_DUCKDB_EXTENSION_INSTALL", unset = ""))) %in% c("1", "true", "yes", "on")
  if (!allow_install) {
    stop(
      "DuckDB cannot load the httpfs extension required for a remote Parquet URL. ",
      "Install it when provisioning the Shiny server or Docker image. ",
      "For local development only, set ALLOW_DUCKDB_EXTENSION_INSTALL=true."
    )
  }

  installed <- tryCatch({
    DBI::dbExecute(connection, "INSTALL httpfs")
    DBI::dbExecute(connection, "LOAD httpfs")
    TRUE
  }, error = function(e) e)
  if (inherits(installed, "error")) {
    stop(
      "DuckDB cannot load the httpfs extension required for a remote Parquet URL. ",
      "Install it with INSTALL httpfs on the Shiny server."
    )
  }
  invisible(TRUE)
}

normalize_http_headers <- function(http_headers) {
  if (is.null(http_headers) || length(http_headers) == 0) {
    return(character())
  }
  headers <- unlist(http_headers, recursive = TRUE, use.names = TRUE)
  header_names <- names(headers)
  if (is.null(header_names)) {
    stop("HTTP headers must be a named character vector.")
  }
  headers <- as.character(headers)
  keep <- !is.na(header_names) & nzchar(header_names) & !is.na(headers) & nzchar(headers)
  stats::setNames(headers[keep], header_names[keep])
}

configure_duckdb_http_headers <- function(connection, http_headers) {
  headers <- normalize_http_headers(http_headers)
  if (length(headers) == 0) {
    return(invisible(TRUE))
  }
  entries <- vapply(seq_along(headers), function(index) {
    key <- as.character(DBI::dbQuoteString(connection, names(headers)[[index]]))
    value <- as.character(DBI::dbQuoteString(connection, headers[[index]]))
    paste0(key, ": ", value)
  }, character(1))
  query <- paste0(
    "CREATE OR REPLACE SECRET app_http_headers (TYPE http, EXTRA_HTTP_HEADERS MAP {",
    paste(entries, collapse = ", "),
    "})"
  )
  tryCatch(
    DBI::dbExecute(connection, query),
    error = function(e) stop("DuckDB cannot configure the HTTP authentication for this remote file.")
  )
  invisible(TRUE)
}

with_duckdb_connection <- function(path, callback, http_headers = NULL) {
  require_package("duckdb")
  require_package("DBI")
  connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  if (is_remote_parquet_path(path)) {
    load_duckdb_httpfs(connection)
    configure_duckdb_http_headers(connection, http_headers)
  }
  callback(connection)
}

duckdb_parquet_relation <- function(connection, path) {
  quoted_path <- as.character(DBI::dbQuoteString(connection, as.character(path[[1]])))
  paste0("read_parquet(", quoted_path, ")")
}

duckdb_parquet_schema <- function(connection, path) {
  relation <- duckdb_parquet_relation(connection, path)
  as.data.frame(DBI::dbGetQuery(connection, paste0("DESCRIBE SELECT * FROM ", relation)))
}

duckdb_check_required_columns <- function(columns, required_columns) {
  missing_columns <- setdiff(required_columns, columns)
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }
  invisible(TRUE)
}

duckdb_mslevel_predicate <- function(connection, mslevel) {
  quoted_mslevel <- as.character(DBI::dbQuoteString(connection, as.character(mslevel[[1]])))
  paste0("CAST(mslevel AS VARCHAR) = ", quoted_mslevel)
}

compute_tic_duckdb <- function(path, mslevel = "1", http_headers = NULL) {
  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "scanid", "mslevel", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    query <- paste0(
      "SELECT scanid, rt, SUM(intensity) AS intensity, COUNT(*) AS n_points ",
      "FROM ", relation, " WHERE ", duckdb_mslevel_predicate(connection, mslevel),
      " GROUP BY scanid, rt ORDER BY rt"
    )
    as.data.frame(DBI::dbGetQuery(connection, query))
  }, http_headers = http_headers)
}

compute_bpi_duckdb <- function(path, mslevel = "1", http_headers = NULL) {
  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "scanid", "mslevel", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    query <- paste0(
      "SELECT scanid, rt, MAX(intensity) AS intensity, COUNT(*) AS n_points ",
      "FROM ", relation, " WHERE ", duckdb_mslevel_predicate(connection, mslevel),
      " GROUP BY scanid, rt ORDER BY rt"
    )
    result <- as.data.frame(DBI::dbGetQuery(connection, query))
    result$intensity[is.infinite(result$intensity)] <- NA_real_
    result
  }, http_headers = http_headers)
}

compute_tic_bpi_duckdb <- function(path, mslevel = "1", http_headers = NULL) {
  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "scanid", "mslevel", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    query <- paste0(
      "SELECT scanid, rt, SUM(intensity) AS tic_intensity, MAX(intensity) AS bpi_intensity, ",
      "COUNT(*) AS n_points FROM ", relation, " WHERE ", duckdb_mslevel_predicate(connection, mslevel),
      " GROUP BY scanid, rt ORDER BY rt"
    )
    combined <- as.data.frame(DBI::dbGetQuery(connection, query))
    combined$bpi_intensity[is.infinite(combined$bpi_intensity)] <- NA_real_
    tic <- combined[, c("scanid", "rt", "tic_intensity", "n_points"), drop = FALSE]
    names(tic)[names(tic) == "tic_intensity"] <- "intensity"
    bpi <- combined[, c("scanid", "rt", "bpi_intensity", "n_points"), drop = FALSE]
    names(bpi)[names(bpi) == "bpi_intensity"] <- "intensity"
    list(tic = tic, bpi = bpi)
  }, http_headers = http_headers)
}

compute_total_intensity_duckdb <- function(path, mslevel = "1", http_headers = NULL) {
  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("mslevel", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    query <- paste0(
      "SELECT COALESCE(SUM(intensity), 0) AS total_intensity, COUNT(*) AS n_points ",
      "FROM ", relation, " WHERE ", duckdb_mslevel_predicate(connection, mslevel)
    )
    as.data.frame(DBI::dbGetQuery(connection, query))
  }, http_headers = http_headers)
}

compute_eic_duckdb <- function(path, target_mz, mz_tolerance = 0.01, mslevel = "1", min_intensity = 0,
                               http_headers = NULL) {
  target_mz <- duckdb_numeric_literal(target_mz, "target_mz")
  mz_tolerance <- duckdb_numeric_literal(mz_tolerance, "mz_tolerance")
  min_intensity <- duckdb_numeric_literal(min_intensity, "min_intensity")
  if (as.numeric(mz_tolerance) <= 0 || as.numeric(min_intensity) < 0) {
    stop("mz_tolerance must be positive and min_intensity must be non-negative.")
  }
  lower_mz <- duckdb_numeric_literal(as.numeric(target_mz) - as.numeric(mz_tolerance), "lower_mz")
  upper_mz <- duckdb_numeric_literal(as.numeric(target_mz) + as.numeric(mz_tolerance), "upper_mz")

  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "scanid", "mslevel", "mz", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    having_clause <- if (as.numeric(min_intensity) > 0) {
      paste0(" HAVING SUM(intensity) >= ", min_intensity)
    } else {
      ""
    }
    query <- paste0(
      "SELECT scanid, rt, SUM(intensity) AS intensity, COUNT(*) AS n_points, ",
      "MIN(mz) AS min_mz, MAX(mz) AS max_mz FROM ", relation,
      " WHERE ", duckdb_mslevel_predicate(connection, mslevel),
      " AND mz >= ", lower_mz, " AND mz <= ", upper_mz,
      " GROUP BY scanid, rt", having_clause, " ORDER BY rt"
    )
    as.data.frame(DBI::dbGetQuery(connection, query))
  }, http_headers = http_headers)
}

empty_ms2_spectrum <- function() {
  data.frame(
    mz = numeric(),
    intensity = numeric(),
    max_intensity = numeric(),
    n_points = integer(),
    stringsAsFactors = FALSE
  )
}

compute_ms2_spectrum_duckdb <- function(path, expected_rt, rt_tolerance = 0.5, bin_width = 0.01,
                                        min_intensity = 0, top_n = 150, http_headers = NULL) {
  expected_rt <- duckdb_numeric_literal(expected_rt, "expected_rt")
  rt_tolerance <- duckdb_numeric_literal(rt_tolerance, "rt_tolerance")
  bin_width <- duckdb_numeric_literal(bin_width, "bin_width")
  min_intensity <- duckdb_numeric_literal(min_intensity, "min_intensity")
  top_n_value <- suppressWarnings(as.numeric(top_n[[1]]))
  if (
    as.numeric(rt_tolerance) < 0 || as.numeric(bin_width) <= 0 ||
      as.numeric(min_intensity) < 0 || !is.finite(top_n_value) ||
      top_n_value < 1 || top_n_value != floor(top_n_value)
  ) {
    stop(
      "rt_tolerance must be non-negative, bin_width must be positive, ",
      "min_intensity must be non-negative, and top_n must be a positive whole number."
    )
  }
  top_n <- as.integer(top_n_value)
  lower_rt <- duckdb_numeric_literal(as.numeric(expected_rt) - as.numeric(rt_tolerance), "lower_rt")
  upper_rt <- duckdb_numeric_literal(as.numeric(expected_rt) + as.numeric(rt_tolerance), "upper_rt")

  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "mslevel", "mz", "intensity"))
    relation <- duckdb_parquet_relation(connection, path)
    intensity_filter <- if (as.numeric(min_intensity) > 0) {
      paste0(" AND intensity >= ", min_intensity)
    } else {
      ""
    }

    # A raw file has no precursor assignment for every MS2 peak. Aggregate all
    # MS2 signals acquired in the selected RT window, then retain the strongest bins.
    query <- paste0(
      "WITH binned AS (",
      "SELECT ROUND(mz / ", bin_width, ") * ", bin_width, " AS mz, ",
      "SUM(intensity) AS intensity, MAX(intensity) AS max_intensity, COUNT(*) AS n_points ",
      "FROM ", relation,
      " WHERE ", duckdb_mslevel_predicate(connection, "2"),
      " AND rt >= ", lower_rt, " AND rt <= ", upper_rt,
      intensity_filter,
      " GROUP BY 1",
      ") SELECT mz, intensity, max_intensity, n_points FROM binned ",
      "ORDER BY intensity DESC LIMIT ", top_n
    )
    result <- as.data.frame(DBI::dbGetQuery(connection, query))
    if (nrow(result) == 0) {
      return(empty_ms2_spectrum())
    }
    result <- result[order(result$mz), , drop = FALSE]
    rownames(result) <- NULL
    result
  }, http_headers = http_headers)
}

collect_mz_window_points_duckdb <- function(path, target_mz, mz_tolerance = 0.01, mslevel = "1",
                                            http_headers = NULL) {
  target_mz <- suppressWarnings(as.numeric(target_mz))
  mz_tolerance <- suppressWarnings(as.numeric(mz_tolerance[[1]]))
  if (length(target_mz) == 0 || any(!is.finite(target_mz))) {
    stop("target_mz must contain at least one finite numeric value.")
  }
  if (!is.finite(mz_tolerance) || mz_tolerance <= 0) {
    stop("mz_tolerance must be a positive numeric value.")
  }

  with_duckdb_connection(path, function(connection) {
    schema <- duckdb_parquet_schema(connection, path)
    columns <- schema$column_name
    duckdb_check_required_columns(columns, c("rt", "scanid", "mslevel", "mz", "intensity"))
    selected_columns <- c("scanid", "rt", "mz", "intensity")
    if ("dt" %in% columns) selected_columns <- c(selected_columns, "dt")
    if ("ccs" %in% columns) selected_columns <- c(selected_columns, "ccs")
    mz_predicates <- vapply(target_mz, function(mz_value) {
      lower_mz <- duckdb_numeric_literal(mz_value - mz_tolerance, "lower_mz")
      upper_mz <- duckdb_numeric_literal(mz_value + mz_tolerance, "upper_mz")
      paste0("(mz >= ", lower_mz, " AND mz <= ", upper_mz, ")")
    }, character(1))
    relation <- duckdb_parquet_relation(connection, path)
    query <- paste0(
      "SELECT ", paste(selected_columns, collapse = ", "), " FROM ", relation,
      " WHERE ", duckdb_mslevel_predicate(connection, mslevel),
      " AND (", paste(mz_predicates, collapse = " OR "), ")"
    )
    as.data.frame(DBI::dbGetQuery(connection, query))
  }, http_headers = http_headers)
}

inspect_remote_parquet_file <- function(path, http_headers = NULL) {
  if (!is_remote_parquet_path(path)) {
    stop("inspect_remote_parquet_file expects an HTTP or HTTPS URL.")
  }
  with_duckdb_connection(path, function(connection) {
    schema_raw <- duckdb_parquet_schema(connection, path)
    columns <- schema_raw$column_name
    relation <- duckdb_parquet_relation(connection, path)
    summary_parts <- c("COUNT(*) AS rows")
    for (column in intersect(c("rt", "mz", "intensity", "dt", "ccs", "bin"), columns)) {
      summary_parts <- c(
        summary_parts,
        paste0("MIN(", column, ") AS min_", column),
        paste0("MAX(", column, ") AS max_", column)
      )
    }
    summary <- as.data.frame(DBI::dbGetQuery(
      connection,
      paste0("SELECT ", paste(summary_parts, collapse = ", "), " FROM ", relation)
    ))
    required_columns <- c("rt", "scanid", "mslevel", "mz", "intensity")
    summary$missing_required_columns <- if (length(setdiff(required_columns, columns)) == 0) {
      "aucune"
    } else {
      paste(setdiff(required_columns, columns), collapse = ", ")
    }
    ms_levels <- if ("mslevel" %in% columns) {
      as.data.frame(DBI::dbGetQuery(
        connection,
        paste0(
          "SELECT CAST(mslevel AS VARCHAR) AS mslevel, COUNT(*) AS rows FROM ",
          relation, " GROUP BY 1 ORDER BY 1"
        )
      ))
    } else {
      data.frame(mslevel = character(), rows = numeric(), stringsAsFactors = FALSE)
    }
    preview_columns <- intersect(c("rt", "scanid", "mslevel", "mz", "intensity", "bin", "dt", "ccs"), columns)
    preview <- if (length(preview_columns) > 0) {
      as.data.frame(DBI::dbGetQuery(
        connection,
        paste0("SELECT ", paste(preview_columns, collapse = ", "), " FROM ", relation, " LIMIT 8")
      ))
    } else {
      data.frame()
    }
    list(
      path = NA_character_,
      size = NA_real_,
      schema = data.frame(
        column = as.character(schema_raw$column_name),
        type = as.character(schema_raw$column_type),
        stringsAsFactors = FALSE
      ),
      columns = columns,
      summary = summary,
      ms_levels = ms_levels,
      preview = preview
    )
  }, http_headers = http_headers)
}

compute_tic <- function(path, mslevel = "1", http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(compute_tic_duckdb(path, mslevel = mslevel, http_headers = http_headers))
  }
  suppressWarnings({
  require_package("dplyr")
  dataset <- open_hrms_dataset(path)
  check_required_columns(dataset, c("rt", "scanid", "mslevel", "intensity"))
  mslevel_value <- as.character(mslevel)

  query <- dataset |>
    dplyr::filter(mslevel == mslevel_value) |>
    dplyr::group_by(scanid, rt) |>
    dplyr::summarise(
      intensity = sum(intensity, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(rt)

  collect_quietly(query)
  })
}

compute_bpi <- function(path, mslevel = "1", http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(compute_bpi_duckdb(path, mslevel = mslevel, http_headers = http_headers))
  }
  suppressWarnings({
  require_package("dplyr")
  dataset <- open_hrms_dataset(path)
  check_required_columns(dataset, c("rt", "scanid", "mslevel", "intensity"))
  mslevel_value <- as.character(mslevel)

  query <- dataset |>
    dplyr::filter(mslevel == mslevel_value) |>
    dplyr::group_by(scanid, rt) |>
    dplyr::summarise(
      intensity = max(intensity, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(rt)

  collect_quietly(query)
  })
}

compute_tic_bpi <- function(path, mslevel = "1", http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(compute_tic_bpi_duckdb(path, mslevel = mslevel, http_headers = http_headers))
  }
  suppressWarnings({
  require_package("dplyr")
  dataset <- open_hrms_dataset(path)
  check_required_columns(dataset, c("rt", "scanid", "mslevel", "intensity"))
  mslevel_value <- as.character(mslevel)

  query <- dataset |>
    dplyr::filter(mslevel == mslevel_value) |>
    dplyr::group_by(scanid, rt) |>
    dplyr::summarise(
      tic_intensity = sum(intensity, na.rm = TRUE),
      bpi_intensity = max(intensity, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(rt)

  combined <- collect_quietly(query)
  combined$bpi_intensity[is.infinite(combined$bpi_intensity)] <- NA_real_

  tic <- combined[, c("scanid", "rt", "tic_intensity", "n_points"), drop = FALSE]
  names(tic)[names(tic) == "tic_intensity"] <- "intensity"
  bpi <- combined[, c("scanid", "rt", "bpi_intensity", "n_points"), drop = FALSE]
  names(bpi)[names(bpi) == "bpi_intensity"] <- "intensity"

  list(tic = tic, bpi = bpi)
  })
}

compute_total_intensity <- function(path, mslevel = "1", http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(compute_total_intensity_duckdb(path, mslevel = mslevel, http_headers = http_headers))
  }
  suppressWarnings({
    require_package("dplyr")
    dataset <- open_hrms_dataset(path)
    check_required_columns(dataset, c("mslevel", "intensity"))
    mslevel_value <- as.character(mslevel)

    query <- dataset |>
      dplyr::filter(mslevel == mslevel_value) |>
      dplyr::summarise(
        total_intensity = sum(intensity, na.rm = TRUE),
        n_points = dplyr::n()
      )

    collect_quietly(query)
  })
}

compute_eic <- function(path, target_mz, mz_tolerance = 0.01, mslevel = "1", min_intensity = 0,
                        http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(compute_eic_duckdb(
      path,
      target_mz = target_mz,
      mz_tolerance = mz_tolerance,
      mslevel = mslevel,
      min_intensity = min_intensity,
      http_headers = http_headers
    ))
  }
  suppressWarnings({
  require_package("dplyr")
  if (length(target_mz) != 1 || is.na(target_mz)) {
    stop("target_mz must be a single numeric value.")
  }
  if (length(mz_tolerance) != 1 || is.na(mz_tolerance) || mz_tolerance <= 0) {
    stop("mz_tolerance must be a positive numeric value.")
  }

  dataset <- open_hrms_dataset(path)
  check_required_columns(dataset, c("rt", "scanid", "mslevel", "mz", "intensity"))

  mslevel_value <- as.character(mslevel)
  lower_mz <- target_mz - mz_tolerance
  upper_mz <- target_mz + mz_tolerance

  query <- dataset |>
    dplyr::filter(
      mslevel == mslevel_value,
      mz >= lower_mz,
      mz <= upper_mz
    ) |>
    dplyr::group_by(scanid, rt) |>
    dplyr::summarise(
      intensity = sum(intensity, na.rm = TRUE),
      n_points = dplyr::n(),
      min_mz = min(mz, na.rm = TRUE),
      max_mz = max(mz, na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(min_intensity) && !is.na(min_intensity) && min_intensity > 0) {
    query <- dplyr::filter(query, intensity >= min_intensity)
  }

  query <- dplyr::arrange(query, rt)
  collect_quietly(query)
  })
}

compute_ms2_spectrum <- function(path, expected_rt, rt_tolerance = 0.5, bin_width = 0.01,
                                 min_intensity = 0, top_n = 150, http_headers = NULL) {
  # DuckDB is also used for local files here because it handles dictionary-encoded
  # mslevel columns consistently across the Parquet files supplied by the observatory.
  compute_ms2_spectrum_duckdb(
    path,
    expected_rt = expected_rt,
    rt_tolerance = rt_tolerance,
    bin_width = bin_width,
    min_intensity = min_intensity,
    top_n = top_n,
    http_headers = http_headers
  )
}

empty_eic <- function(dt_column_available = FALSE, ccs_column_available = FALSE) {
  result <- data.frame(
    scanid = integer(),
    rt = numeric(),
    intensity = numeric(),
    n_points = integer(),
    min_mz = numeric(),
    max_mz = numeric(),
    observed_mz = numeric(),
    observed_dt = numeric(),
    observed_ccs = numeric(),
    dt_column_available = logical(),
    ccs_column_available = logical(),
    stringsAsFactors = FALSE
  )
  attr(result, "dt_column_available") <- isTRUE(dt_column_available)
  attr(result, "ccs_column_available") <- isTRUE(ccs_column_available)
  result
}

scalar_numeric <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(value[[1]]))
}

compound_numeric <- function(compound, column) {
  if (!column %in% names(compound) || nrow(compound) == 0) {
    return(NA_real_)
  }
  scalar_numeric(compound[[column]])
}

compound_character <- function(compound, column) {
  if (!column %in% names(compound) || nrow(compound) == 0) {
    return(NA_character_)
  }
  value <- as.character(compound[[column]][[1]])
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

peak_index <- function(x) {
  if (nrow(x) == 0 || !"intensity" %in% names(x)) {
    return(NA_integer_)
  }
  intensity <- suppressWarnings(as.numeric(x$intensity))
  valid <- is.finite(intensity) & intensity > 0
  if (!any(valid)) {
    return(NA_integer_)
  }
  which.max(replace(intensity, !valid, -Inf))
}

peak_value <- function(x, column, index) {
  if (!column %in% names(x) || length(index) == 0 || is.na(index) || index < 1 || index > nrow(x)) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(x[[column]][[index]]))
  if (is.finite(value)) value else NA_real_
}

peak_observed_mz <- function(x, index) {
  observed_mz <- peak_value(x, "observed_mz", index)
  if (is.finite(observed_mz)) {
    return(observed_mz)
  }
  min_mz <- peak_value(x, "min_mz", index)
  max_mz <- peak_value(x, "max_mz", index)
  if (is.finite(min_mz) && is.finite(max_mz)) {
    return((min_mz + max_mz) / 2)
  }
  NA_real_
}

signal_weighted_mean <- function(values, weights) {
  values <- suppressWarnings(as.numeric(values))
  weights <- suppressWarnings(as.numeric(weights))
  keep <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(values[keep] * weights[keep]) / sum(weights[keep])
}

has_rt_window <- function(expected_rt, rt_tolerance) {
  length(expected_rt) == 1 && length(rt_tolerance) == 1 &&
    is.finite(expected_rt) && is.finite(rt_tolerance) && rt_tolerance >= 0
}

summarise_eic_detection <- function(eic, expected_rt = NA_real_, rt_tolerance = NA_real_,
                                    restrict_to_rt_window = FALSE) {
  expected_rt <- scalar_numeric(expected_rt)
  rt_tolerance <- scalar_numeric(rt_tolerance)
  rt_window_available <- has_rt_window(expected_rt, rt_tolerance)
  dt_column_available <- isTRUE(attr(eic, "dt_column_available")) ||
    ("dt_column_available" %in% names(eic) && any(eic$dt_column_available %in% TRUE, na.rm = TRUE))
  ccs_column_available <- isTRUE(attr(eic, "ccs_column_available")) ||
    ("ccs_column_available" %in% names(eic) && any(eic$ccs_column_available %in% TRUE, na.rm = TRUE))

  if (nrow(eic) == 0) {
    return(data.frame(
      status = "Not Detected",
      max_intensity = 0,
      rt_at_max = NA_real_,
      observed_mz = NA_real_,
      observed_dt = NA_real_,
      observed_ccs = NA_real_,
      n_scans = 0,
      area_sum = 0,
      rt_window_used = isTRUE(restrict_to_rt_window) && rt_window_available,
      rt_window_available = rt_window_available,
      rt_match = if (rt_window_available) FALSE else NA,
      max_intensity_total = 0,
      rt_at_global_max = NA_real_,
      observed_mz_total = NA_real_,
      observed_dt_total = NA_real_,
      observed_ccs_total = NA_real_,
      n_scans_total = 0,
      area_sum_total = 0,
      dt_column_available = dt_column_available,
      ccs_column_available = ccs_column_available,
      stringsAsFactors = FALSE
    ))
  }

  total <- eic[!is.na(eic$rt), , drop = FALSE]
  if (nrow(total) == 0) total <- eic
  global_index <- peak_index(total)
  global_max <- if (is.na(global_index)) 0 else peak_value(total, "intensity", global_index)
  rt_at_global_max <- if (is.na(global_index)) NA_real_ else peak_value(total, "rt", global_index)
  observed_mz_total <- if (is.na(global_index)) NA_real_ else peak_observed_mz(total, global_index)
  observed_dt_total <- if (is.na(global_index)) NA_real_ else peak_value(total, "observed_dt", global_index)
  observed_ccs_total <- if (is.na(global_index)) NA_real_ else peak_value(total, "observed_ccs", global_index)

  rt_window <- empty_eic(
    dt_column_available = dt_column_available,
    ccs_column_available = ccs_column_available
  )
  if (rt_window_available) {
    in_window <- !is.na(eic$rt) & abs(eic$rt - expected_rt) <= rt_tolerance
    rt_window <- eic[in_window, , drop = FALSE]
  }

  use_rt_window <- isTRUE(restrict_to_rt_window) && rt_window_available
  decision_eic <- if (use_rt_window) rt_window else eic
  decision_index <- peak_index(decision_eic)
  decision_max <- if (is.na(decision_index)) 0 else peak_value(decision_eic, "intensity", decision_index)
  rt_at_max <- if (is.na(decision_index)) NA_real_ else peak_value(decision_eic, "rt", decision_index)
  observed_mz <- if (is.na(decision_index)) NA_real_ else peak_observed_mz(decision_eic, decision_index)
  observed_dt <- if (is.na(decision_index)) NA_real_ else peak_value(decision_eic, "observed_dt", decision_index)
  observed_ccs <- if (is.na(decision_index)) NA_real_ else peak_value(decision_eic, "observed_ccs", decision_index)

  rt_match <- if (rt_window_available) {
    nrow(rt_window) > 0 && any(!is.na(rt_window$intensity) & rt_window$intensity > 0)
  } else {
    NA
  }

  data.frame(
    status = if (decision_max > 0) "Detected" else "Not Detected",
    max_intensity = decision_max,
    rt_at_max = rt_at_max,
    observed_mz = observed_mz,
    observed_dt = observed_dt,
    observed_ccs = observed_ccs,
    n_scans = nrow(decision_eic),
    # The raw Parquet only contains points inside the m/z window. This is a
    # discrete sum of per-scan EIC intensities, not a time-integrated area.
    area_sum = sum(decision_eic$intensity, na.rm = TRUE),
    rt_window_used = use_rt_window,
    rt_window_available = rt_window_available,
    rt_match = rt_match,
    max_intensity_total = global_max,
    rt_at_global_max = rt_at_global_max,
    observed_mz_total = observed_mz_total,
    observed_dt_total = observed_dt_total,
    observed_ccs_total = observed_ccs_total,
    n_scans_total = nrow(eic),
    area_sum_total = sum(eic$intensity, na.rm = TRUE),
    dt_column_available = dt_column_available,
    ccs_column_available = ccs_column_available,
    stringsAsFactors = FALSE
  )
}

validate_compound <- function(compound, require_rt_match = TRUE) {
  required_columns <- c("compound_id", "name", "mode", "mz")
  missing_columns <- setdiff(required_columns, names(compound))
  if (length(missing_columns) > 0) {
    stop("Compound table missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (nrow(compound) != 1) {
    stop("A screening function expects exactly one compound row.")
  }

  expected_mz <- compound_numeric(compound, "mz")
  expected_rt <- compound_numeric(compound, "rt")
  if (!is.finite(expected_mz)) {
    stop("The compound m/z must be a finite numeric value.")
  }

  # A future suspect can be defined by m/z only. In that case it remains
  # visible as level-1 evidence even when RT coherence is requested.
  list(
    expected_mz = expected_mz,
    expected_rt = expected_rt,
    expected_dt = compound_numeric(compound, "dt"),
    expected_ccs = compound_numeric(compound, "ccs")
  )
}

eic_from_points <- function(points, target_mz, mz_tolerance = 0.01) {
  target_mz <- suppressWarnings(as.numeric(target_mz[[1]]))
  mz_tolerance <- suppressWarnings(as.numeric(mz_tolerance[[1]]))
  if (!is.finite(target_mz) || !is.finite(mz_tolerance) || mz_tolerance <= 0) {
    stop("target_mz and mz_tolerance must be finite numeric values, with a positive tolerance.")
  }
  has_dt <- "dt" %in% names(points)
  has_ccs <- "ccs" %in% names(points)
  if (nrow(points) == 0) {
    return(empty_eic(dt_column_available = has_dt, ccs_column_available = has_ccs))
  }

  keep <- !is.na(points$mz) & points$mz >= target_mz - mz_tolerance & points$mz <= target_mz + mz_tolerance
  selected_columns <- c("scanid", "rt", "mz", "intensity")
  if (has_dt) selected_columns <- c(selected_columns, "dt")
  if (has_ccs) selected_columns <- c(selected_columns, "ccs")
  x <- points[keep, selected_columns, drop = FALSE]
  if (nrow(x) == 0) {
    return(empty_eic(dt_column_available = has_dt, ccs_column_available = has_ccs))
  }

  result <- x |>
    dplyr::group_by(scanid, rt) |>
    dplyr::summarise(
      observed_mz = signal_weighted_mean(mz, intensity),
      observed_dt = if (has_dt) signal_weighted_mean(dt, intensity) else NA_real_,
      observed_ccs = if (has_ccs) signal_weighted_mean(ccs, intensity) else NA_real_,
      intensity = sum(intensity, na.rm = TRUE),
      n_points = dplyr::n(),
      min_mz = min(mz, na.rm = TRUE),
      max_mz = max(mz, na.rm = TRUE),
      dt_column_available = has_dt,
      ccs_column_available = has_ccs,
      .groups = "drop"
    ) |>
    dplyr::arrange(rt)
  result <- as.data.frame(result)
  attr(result, "dt_column_available") <- has_dt
  attr(result, "ccs_column_available") <- has_ccs
  result
}

make_mz_union_filter <- function(target_mz, mz_tolerance) {
  require_package("rlang")
  target_mz <- suppressWarnings(as.numeric(target_mz))
  if (length(target_mz) == 0 || any(!is.finite(target_mz))) {
    stop("target_mz must contain at least one finite numeric value.")
  }

  predicates <- lapply(target_mz, function(mz_value) {
    rlang::expr((mz >= !!(mz_value - mz_tolerance)) & (mz <= !!(mz_value + mz_tolerance)))
  })
  Reduce(function(left, right) rlang::expr((!!left) | (!!right)), predicates)
}

collect_mz_window_points <- function(path, target_mz, mz_tolerance = 0.01, mslevel = "1",
                                     http_headers = NULL) {
  if (is_remote_parquet_path(path)) {
    return(collect_mz_window_points_duckdb(
      path,
      target_mz = target_mz,
      mz_tolerance = mz_tolerance,
      mslevel = mslevel,
      http_headers = http_headers
    ))
  }
  suppressWarnings({
  require_package("dplyr")
  mz_tolerance <- suppressWarnings(as.numeric(mz_tolerance[[1]]))
  if (!is.finite(mz_tolerance) || mz_tolerance <= 0) {
    stop("mz_tolerance must be a positive numeric value.")
  }

  dataset <- open_hrms_dataset(path)
  check_required_columns(dataset, c("rt", "scanid", "mslevel", "mz", "intensity"))
  mz_filter <- make_mz_union_filter(target_mz, mz_tolerance)
  mslevel_value <- as.character(mslevel)
  selected_columns <- c("scanid", "rt", "mz", "intensity")
  if ("dt" %in% names(dataset)) selected_columns <- c(selected_columns, "dt")
  if ("ccs" %in% names(dataset)) selected_columns <- c(selected_columns, "ccs")

  query <- dataset |>
    dplyr::filter(mslevel == mslevel_value, !!mz_filter) |>
    dplyr::select(dplyr::all_of(selected_columns))
  collect_quietly(query)
  })
}

confidence_label <- function(level) {
  level <- suppressWarnings(as.integer(level[[1]]))
  switch(
    as.character(level),
    "0" = "Preuve 0 - aucun signal",
    "1" = "Preuve 1 - m/z",
    "2" = "Preuve 2 - m/z + RT",
    "3" = "Preuve 3 - m/z + RT + mobilite",
    "Preuve inconnue"
  )
}

screening_result_template <- function(path, compound, mz_tolerance, rt_tolerance,
                                      dt_tolerance_pct, ccs_tolerance_pct,
                                      require_rt_match, use_dt, use_ccs) {
  data.frame(
    file = parquet_source_name(path),
    file_path = safe_parquet_source_path(path),
    compound_id = compound_character(compound, "compound_id"),
    compound_name = compound_character(compound, "name"),
    mode = compound_character(compound, "mode"),
    expected_mz = compound_numeric(compound, "mz"),
    observed_mz = NA_real_,
    mz_tolerance = mz_tolerance,
    mz_error_da = NA_real_,
    mz_error_ppm = NA_real_,
    mz_match = NA,
    expected_rt = compound_numeric(compound, "rt"),
    rt_tolerance = rt_tolerance,
    rt_at_max = NA_real_,
    rt_error = NA_real_,
    rt_match = NA,
    rt_intensity_match = NA,
    rt_window_used = NA,
    rt_window_available = NA,
    expected_dt = compound_numeric(compound, "dt"),
    observed_dt = NA_real_,
    dt_tolerance_pct = dt_tolerance_pct,
    dt_error = NA_real_,
    dt_error_pct = NA_real_,
    dt_match = NA,
    dt_status = "Non evalue",
    expected_ccs = compound_numeric(compound, "ccs"),
    observed_ccs = NA_real_,
    ccs_tolerance_pct = ccs_tolerance_pct,
    ccs_error = NA_real_,
    ccs_error_pct = NA_real_,
    ccs_match = NA,
    ccs_status = "Non evalue",
    expected_dt_from_ccs = NA_real_,
    ccs_calibration_c1 = NA_real_,
    ccs_calibration_c2 = NA_real_,
    ccs_to_dt_error_ms = NA_real_,
    ccs_to_dt_error_pct = NA_real_,
    ccs_to_dt_match = NA,
    ccs_to_dt_status = "Non evalue",
    max_intensity = NA_real_,
    rt_max_intensity = NA_real_,
    n_scans = NA_integer_,
    rt_n_scans = NA_integer_,
    area_sum = NA_real_,
    rt_area_sum = NA_real_,
    max_intensity_total = NA_real_,
    rt_at_global_max = NA_real_,
    n_scans_total = NA_integer_,
    area_sum_total = NA_real_,
    intensity_match = NA,
    confidence_level = NA_integer_,
    confidence_label = "Erreur",
    minimum_confidence_level = if (isTRUE(require_rt_match)) 2L else 1L,
    is_detected = NA,
    status = "Error",
    require_rt_match = isTRUE(require_rt_match),
    use_dt = isTRUE(use_dt),
    use_ccs = isTRUE(use_ccs),
    error = "",
    stringsAsFactors = FALSE
  )
}

screen_compound_from_eic <- function(path, compound, eic, mz_tolerance = 0.01, rt_tolerance = 0.5,
                                     min_intensity = 0, require_rt_match = TRUE,
                                     use_dt = TRUE, dt_tolerance_pct = 10,
                                     use_ccs = TRUE, ccs_tolerance_pct = 10,
                                     ccs_calibration = NULL, ccs_to_drifttime = NULL) {
  compound_values <- validate_compound(compound, require_rt_match = require_rt_match)
  mz_tolerance <- scalar_numeric(mz_tolerance)
  rt_tolerance <- scalar_numeric(rt_tolerance)
  min_intensity <- scalar_numeric(min_intensity)
  dt_tolerance_pct <- scalar_numeric(dt_tolerance_pct)
  ccs_tolerance_pct <- scalar_numeric(ccs_tolerance_pct)
  if (!is.finite(mz_tolerance) || mz_tolerance <= 0) {
    stop("mz_tolerance must be a positive numeric value.")
  }
  if (!is.finite(rt_tolerance) || rt_tolerance < 0) {
    stop("rt_tolerance must be a non-negative numeric value.")
  }
  if (!is.finite(min_intensity) || min_intensity < 0) {
    stop("min_intensity must be a non-negative numeric value.")
  }
  if (!is.finite(dt_tolerance_pct) || dt_tolerance_pct < 0) {
    stop("dt_tolerance_pct must be a non-negative numeric value.")
  }
  if (!is.finite(ccs_tolerance_pct) || ccs_tolerance_pct < 0) {
    stop("ccs_tolerance_pct must be a non-negative numeric value.")
  }

  global_summary <- summarise_eic_detection(
    eic,
    expected_rt = compound_values$expected_rt,
    rt_tolerance = rt_tolerance,
    restrict_to_rt_window = FALSE
  )
  rt_summary <- summarise_eic_detection(
    eic,
    expected_rt = compound_values$expected_rt,
    rt_tolerance = rt_tolerance,
    restrict_to_rt_window = TRUE
  )

  observed_mz <- global_summary$observed_mz[[1]]
  observed_dt <- rt_summary$observed_dt[[1]]
  observed_ccs <- rt_summary$observed_ccs[[1]]
  mz_error_da <- if (is.finite(observed_mz)) observed_mz - compound_values$expected_mz else NA_real_
  mz_error_ppm <- if (is.finite(mz_error_da) && compound_values$expected_mz != 0) {
    mz_error_da / compound_values$expected_mz * 1e6
  } else {
    NA_real_
  }
  signal_present <- isTRUE(global_summary$max_intensity[[1]] > 0)
  mz_match <- signal_present && is.finite(observed_mz) && abs(mz_error_da) <= mz_tolerance
  intensity_match <- signal_present && global_summary$max_intensity[[1]] >= min_intensity
  level_one <- mz_match && intensity_match

  rt_window_available <- isTRUE(rt_summary$rt_window_available[[1]])
  rt_intensity_match <- level_one && rt_window_available && isTRUE(rt_summary$rt_match[[1]]) &&
    isTRUE(rt_summary$max_intensity[[1]] > 0) && rt_summary$max_intensity[[1]] >= min_intensity
  level_two <- level_one && rt_intensity_match

  dt_match <- NA
  dt_error <- NA_real_
  dt_error_pct <- NA_real_
  dt_status <- "Non evalue"
  dt_column_available <- isTRUE(rt_summary$dt_column_available[[1]])
  if (!isTRUE(use_dt)) {
    dt_status <- "Non evalue: DT desactive"
  } else if (!level_two) {
    dt_status <- "Non evalue: RT non confirme"
  } else if (!is.finite(compound_values$expected_dt) || compound_values$expected_dt <= 0) {
    dt_status <- "Non evalue: DT non renseigne"
  } else if (!dt_column_available) {
    dt_status <- "Non evalue: colonne DT absente"
  } else if (!is.finite(observed_dt)) {
    dt_status <- "Non evalue: DT absent du pic"
  } else {
    dt_error <- observed_dt - compound_values$expected_dt
    dt_error_pct <- abs(dt_error) / compound_values$expected_dt * 100
    dt_match <- dt_error_pct <= dt_tolerance_pct
    dt_status <- if (isTRUE(dt_match)) "Compatible" else "Hors tolerance"
  }

  ccs_match <- NA
  ccs_error <- NA_real_
  ccs_error_pct <- NA_real_
  ccs_status <- "Non evalue"
  ccs_column_available <- isTRUE(rt_summary$ccs_column_available[[1]])
  if (!isTRUE(use_ccs)) {
    ccs_status <- "Non evalue: CCS desactive"
  } else if (!level_two) {
    ccs_status <- "Non evalue: RT non confirme"
  } else if (!is.finite(compound_values$expected_ccs) || compound_values$expected_ccs <= 0) {
    ccs_status <- "Non evalue: CCS non renseigne"
  } else if (!ccs_column_available) {
    ccs_status <- "Non evalue: colonne CCS absente"
  } else if (!is.finite(observed_ccs)) {
    ccs_status <- "Non evalue: CCS absent du pic"
  } else {
    ccs_error <- observed_ccs - compound_values$expected_ccs
    ccs_error_pct <- abs(ccs_error) / compound_values$expected_ccs * 100
    ccs_match <- ccs_error_pct <= ccs_tolerance_pct
    ccs_status <- if (isTRUE(ccs_match)) "Compatible" else "Hors tolerance"
  }

  # This path is deliberately evaluated only after m/z and RT have matched.
  # It avoids a conversion call for every raw signal in a Parquet file.
  ccs_to_dt <- list(
    ok = FALSE,
    expected_dt = NA_real_,
    C1 = NA_real_,
    C2 = NA_real_,
    status = "Non evalue: RT non confirme"
  )
  ccs_to_dt_match <- NA
  ccs_to_dt_error_ms <- NA_real_
  ccs_to_dt_error_pct <- NA_real_
  ccs_to_dt_status <- ccs_to_dt$status
  if (!isTRUE(use_ccs)) {
    ccs_to_dt_status <- "Non evalue: CCS desactive"
  } else if (level_two) {
    ccs_to_dt <- resolve_expected_drifttime_from_ccs(
      ccs = compound_values$expected_ccs,
      mz = compound_values$expected_mz,
      calibration_parameters = ccs_calibration,
      converter = ccs_to_drifttime
    )
    ccs_to_dt_status <- ccs_to_dt$status
    if (isTRUE(ccs_to_dt$ok) && !dt_column_available) {
      ccs_to_dt_status <- "Non evalue: colonne DT absente"
    } else if (isTRUE(ccs_to_dt$ok) && !is.finite(observed_dt)) {
      ccs_to_dt_status <- "Non evalue: DT absent du pic"
    } else if (isTRUE(ccs_to_dt$ok)) {
      ccs_to_dt_error_ms <- observed_dt - ccs_to_dt$expected_dt
      ccs_to_dt_error_pct <- abs(ccs_to_dt_error_ms) / ccs_to_dt$expected_dt * 100
      ccs_to_dt_match <- ccs_to_dt_error_pct <= dt_tolerance_pct
      ccs_to_dt_status <- if (isTRUE(ccs_to_dt_match)) {
        "Compatible (exploratoire)"
      } else {
        "Hors tolerance (exploratoire)"
      }
    }
  }

  confidence_level <- if (!level_one) {
    0L
  } else if (!level_two) {
    1L
  } else if (isTRUE(ccs_match)) {
    3L
  } else {
    2L
  }
  minimum_confidence_level <- if (isTRUE(require_rt_match)) 2L else 1L
  detected <- confidence_level >= minimum_confidence_level

  result <- screening_result_template(
    path,
    compound,
    mz_tolerance = mz_tolerance,
    rt_tolerance = rt_tolerance,
    dt_tolerance_pct = dt_tolerance_pct,
    ccs_tolerance_pct = ccs_tolerance_pct,
    require_rt_match = require_rt_match,
    use_dt = use_dt,
    use_ccs = use_ccs
  )
  result$observed_mz <- observed_mz
  result$mz_error_da <- mz_error_da
  result$mz_error_ppm <- mz_error_ppm
  result$mz_match <- mz_match
  result$rt_at_max <- rt_summary$rt_at_max[[1]]
  result$rt_error <- if (is.finite(result$rt_at_max[[1]]) && is.finite(compound_values$expected_rt)) {
    result$rt_at_max[[1]] - compound_values$expected_rt
  } else {
    NA_real_
  }
  result$rt_match <- rt_summary$rt_match[[1]]
  result$rt_intensity_match <- rt_intensity_match
  result$rt_window_used <- rt_summary$rt_window_used[[1]]
  result$rt_window_available <- rt_window_available
  result$observed_dt <- observed_dt
  result$dt_error <- dt_error
  result$dt_error_pct <- dt_error_pct
  result$dt_match <- dt_match
  result$dt_status <- dt_status
  result$observed_ccs <- observed_ccs
  result$ccs_error <- ccs_error
  result$ccs_error_pct <- ccs_error_pct
  result$ccs_match <- ccs_match
  result$ccs_status <- ccs_status
  result$expected_dt_from_ccs <- ccs_to_dt$expected_dt
  result$ccs_calibration_c1 <- ccs_to_dt$C1
  result$ccs_calibration_c2 <- ccs_to_dt$C2
  result$ccs_to_dt_error_ms <- ccs_to_dt_error_ms
  result$ccs_to_dt_error_pct <- ccs_to_dt_error_pct
  result$ccs_to_dt_match <- ccs_to_dt_match
  result$ccs_to_dt_status <- ccs_to_dt_status
  result$max_intensity <- global_summary$max_intensity[[1]]
  result$rt_max_intensity <- rt_summary$max_intensity[[1]]
  result$n_scans <- global_summary$n_scans[[1]]
  result$rt_n_scans <- rt_summary$n_scans[[1]]
  result$area_sum <- global_summary$area_sum[[1]]
  result$rt_area_sum <- rt_summary$area_sum[[1]]
  result$max_intensity_total <- global_summary$max_intensity_total[[1]]
  result$rt_at_global_max <- global_summary$rt_at_global_max[[1]]
  result$n_scans_total <- global_summary$n_scans_total[[1]]
  result$area_sum_total <- global_summary$area_sum_total[[1]]
  result$intensity_match <- intensity_match
  result$confidence_level <- confidence_level
  result$confidence_label <- confidence_label(confidence_level)
  result$minimum_confidence_level <- minimum_confidence_level
  result$is_detected <- detected
  result$status <- if (detected) "Detected" else "Not Detected"
  result
}

screening_error_row <- function(path, compound, mz_tolerance, rt_tolerance, require_rt_match,
                                use_dt, dt_tolerance_pct, use_ccs, ccs_tolerance_pct,
                                error_message) {
  result <- screening_result_template(
    path,
    compound,
    mz_tolerance = mz_tolerance,
    rt_tolerance = rt_tolerance,
    dt_tolerance_pct = dt_tolerance_pct,
    ccs_tolerance_pct = ccs_tolerance_pct,
    require_rt_match = require_rt_match,
    use_dt = use_dt,
    use_ccs = use_ccs
  )
  result$error <- error_message
  result
}

screen_compound_in_file <- function(path, compound, mz_tolerance = 0.01, rt_tolerance = 0.5,
                                    mslevel = "1", min_intensity = 0, require_rt_match = TRUE,
                                    use_dt = TRUE, dt_tolerance_pct = 10,
                                    use_ccs = TRUE, ccs_tolerance_pct = 10,
                                    ccs_calibration = NULL, ccs_to_drifttime = NULL,
                                    http_headers = NULL) {
  compound_values <- validate_compound(compound, require_rt_match = require_rt_match)
  points <- collect_mz_window_points(
    path,
    target_mz = compound_values$expected_mz,
    mz_tolerance = mz_tolerance,
    mslevel = mslevel,
    http_headers = http_headers
  )
  eic <- eic_from_points(points, compound_values$expected_mz, mz_tolerance = mz_tolerance)
  screen_compound_from_eic(
    path,
    compound,
    eic,
    mz_tolerance = mz_tolerance,
    rt_tolerance = rt_tolerance,
    min_intensity = min_intensity,
    require_rt_match = require_rt_match,
    use_dt = use_dt,
    dt_tolerance_pct = dt_tolerance_pct,
    use_ccs = use_ccs,
    ccs_tolerance_pct = ccs_tolerance_pct,
    ccs_calibration = ccs_calibration,
    ccs_to_drifttime = ccs_to_drifttime
  )
}

screen_compounds_in_file <- function(path, compounds, mz_tolerance = 0.01, rt_tolerance = 0.5,
                                     mslevel = "1", min_intensity = 0, require_rt_match = TRUE,
                                     use_dt = TRUE, dt_tolerance_pct = 10,
                                     use_ccs = TRUE, ccs_tolerance_pct = 10,
                                     ccs_calibration = NULL, ccs_to_drifttime = NULL,
                                     http_headers = NULL) {
  if (nrow(compounds) == 0) {
    return(data.frame())
  }
  required_columns <- c("compound_id", "name", "mode", "mz")
  missing_columns <- setdiff(required_columns, names(compounds))
  if (length(missing_columns) > 0) {
    stop("Compound table missing columns: ", paste(missing_columns, collapse = ", "))
  }

  points <- collect_mz_window_points(
    path,
    target_mz = compounds$mz,
    mz_tolerance = mz_tolerance,
    mslevel = mslevel,
    http_headers = http_headers
  )

  results <- lapply(seq_len(nrow(compounds)), function(i) {
    compound <- compounds[i, , drop = FALSE]
    tryCatch({
      compound_values <- validate_compound(compound, require_rt_match = require_rt_match)
      eic <- eic_from_points(points, compound_values$expected_mz, mz_tolerance = mz_tolerance)
      screen_compound_from_eic(
        path,
        compound,
        eic,
        mz_tolerance = mz_tolerance,
        rt_tolerance = rt_tolerance,
        min_intensity = min_intensity,
        require_rt_match = require_rt_match,
        use_dt = use_dt,
        dt_tolerance_pct = dt_tolerance_pct,
        use_ccs = use_ccs,
        ccs_tolerance_pct = ccs_tolerance_pct,
        ccs_calibration = ccs_calibration,
        ccs_to_drifttime = ccs_to_drifttime
      )
    }, error = function(e) {
      screening_error_row(
        path,
        compound,
        mz_tolerance = mz_tolerance,
        rt_tolerance = rt_tolerance,
        require_rt_match = require_rt_match,
        use_dt = use_dt,
        dt_tolerance_pct = dt_tolerance_pct,
        use_ccs = use_ccs,
        ccs_tolerance_pct = ccs_tolerance_pct,
        error_message = conditionMessage(e)
      )
    })
  })

  do.call(rbind, results)
}

is_running_as_script <- function() {
  file_args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_args) == 0) {
    return(FALSE)
  }
  grepl("parquet_chromatograms\\.R$", file_args[[1]])
}

if (is_running_as_script()) {
  args <- commandArgs(trailingOnly = TRUE)
  path <- if (length(args) >= 1) args[[1]] else "data/raw/parquet/test/pharma_PT6_replicate_1.parquet"

  cat("Parquet file:", path, "\n")
  tic <- compute_tic(path, mslevel = "1")
  bpi <- compute_bpi(path, mslevel = "1")
  eic <- compute_eic(path, target_mz = 235.1477, mz_tolerance = 0.01, mslevel = "1")
  eic_summary <- summarise_eic_detection(eic)

  cat("TIC rows:", nrow(tic), "\n")
  print(utils::head(tic, 3))
  cat("BPI rows:", nrow(bpi), "\n")
  print(utils::head(bpi, 3))
  cat("EIC rows:", nrow(eic), "\n")
  print(utils::head(eic, 3))
  cat("EIC summary:\n")
  print(eic_summary)
}
