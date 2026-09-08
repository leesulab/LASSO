#!/usr/bin/env Rscript

# Optional integration test for a local Parquet dataset. It intentionally uses
# no project data by default: set DATA_PATH to a mounted disk or USB key.
data_path <- Sys.getenv("DATA_PATH", unset = "")
if (!nzchar(data_path)) {
  cat("test_local_dataset_integration: SKIPPED (DATA_PATH is not set)\n")
  quit(status = 0)
}

data_path <- normalizePath(data_path, mustWork = TRUE)
if (!dir.exists(data_path)) {
  stop("DATA_PATH must point to a directory: ", data_path)
}

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
repository_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
parquet_files <- sort(list.files(data_path, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE))

if (length(parquet_files) == 0) {
  stop("No Parquet file found under DATA_PATH: ", data_path)
}

# Build the index exactly as it will be built on the target machine, but keep
# the generated output outside the repository and outside the data source.
temporary_index <- tempfile("metadata-index-", fileext = ".csv")
on.exit(unlink(temporary_index, force = TRUE), add = TRUE)
build_output <- system2(
  "Rscript",
  c("--vanilla", file.path(repository_root, "scripts", "build_metadata_index.R"), data_path, temporary_index),
  stdout = TRUE,
  stderr = TRUE
)
build_status <- attr(build_output, "status")
if (is.null(build_status)) {
  build_status <- 0L
}
if (!identical(build_status, 0L)) {
  stop("Metadata index build failed:\n", paste(build_output, collapse = "\n"))
}
metadata_index <- utils::read.csv(temporary_index, stringsAsFactors = FALSE, check.names = FALSE)
relative_parquet_paths <- sub(
  paste0("^", normalizePath(data_path, mustWork = TRUE), .Platform$file.sep),
  "",
  normalizePath(parquet_files, mustWork = TRUE)
)
if (nrow(metadata_index) != length(parquet_files)) {
  stop("Metadata index does not contain every Parquet file.")
}
if (anyDuplicated(metadata_index$parquet_relative_path) ||
    !setequal(metadata_index$parquet_relative_path, relative_parquet_paths)) {
  stop("Metadata index paths do not match the available Parquet files.")
}
if (!"json_available" %in% names(metadata_index)) {
  stop("Metadata index must report whether JSON metadata is available.")
}
cat("Metadata index: OK (", nrow(metadata_index), " Parquet files)\n", sep = "")

# app.R resolves its scripts and CSV files from PROJECT_ROOT. Use a disposable
# project copy so this test never overwrites the user's working configuration.
temporary_project <- tempfile("observatoire-hrms-integration-")
dir.create(temporary_project, recursive = TRUE)
on.exit(unlink(temporary_project, recursive = TRUE, force = TRUE), add = TRUE)
stopifnot(file.copy(file.path(repository_root, "app"), temporary_project, recursive = TRUE))
stopifnot(file.copy(file.path(repository_root, "scripts"), temporary_project, recursive = TRUE))
dir.create(file.path(temporary_project, "data", "processed"), recursive = TRUE)
stopifnot(file.copy(
  file.path(repository_root, "data", "processed", "compounds_reference.csv"),
  file.path(temporary_project, "data", "processed", "compounds_reference.csv")
))
stopifnot(file.copy(temporary_index, file.path(temporary_project, "data", "processed", "metadata_index.csv")))

saved_environment <- Sys.getenv(c("PROJECT_ROOT", "DATA_PATH"), unset = NA_character_)
on.exit({
  for (name in names(saved_environment)) {
    value <- saved_environment[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      arguments <- list(value)
      names(arguments) <- name
      do.call(Sys.setenv, arguments)
    }
  }
}, add = TRUE)
Sys.setenv(PROJECT_ROOT = temporary_project, DATA_PATH = data_path)

app_environment <- new.env(parent = globalenv())
source(file.path(temporary_project, "app", "app.R"), local = app_environment)
cat("Application loading: OK\n")

catalog <- app_environment$enrich_parquet_files(
  app_environment$list_parquet_files(data_path),
  app_environment$metadata_index
)
if (nrow(catalog) != length(parquet_files)) {
  stop("The application catalogue does not contain every local Parquet file.")
}
if (any(!catalog$parquet_mode %in% c("pos", "neg"))) {
  stop("At least one file has an unknown ionisation mode.")
}
cat("Application catalogue: OK (", nrow(catalog), " files)\n", sep = "")

# Validate the schema and chromatogram path on every local Parquet file.
cat("Schema validation: running\n")
diagnostics <- lapply(parquet_files, app_environment$inspect_parquet_file)
missing_columns <- vapply(diagnostics, function(item) item$summary$missing_required_columns[[1]], character(1))
if (any(missing_columns != "aucune")) {
  stop("Required columns are missing in: ", paste(basename(parquet_files[missing_columns != "aucune"]), collapse = ", "))
}
cat("TIC/BPI validation: running\n")
chromatograms <- lapply(parquet_files, function(path) compute_tic_bpi(path, mslevel = "1"))
chromatogram_ok <- vapply(chromatograms, function(item) {
  nrow(item$tic) > 0 && nrow(item$bpi) > 0 &&
    nrow(item$tic) == nrow(item$bpi) &&
    identical(item$tic$scanid, item$bpi$scanid)
}, logical(1))
if (!all(chromatogram_ok)) {
  stop("TIC/BPI calculation failed for: ", paste(basename(parquet_files[!chromatogram_ok]), collapse = ", "))
}
cat("Batch screening: running\n")

# Exercise the same batch-screening primitive used by the application. Results
# may legitimately be detected or not detected; an Error status is never valid.
compounds <- app_environment$compounds_reference
positive_standards <- compounds[compounds$mode == "pos", , drop = FALSE]
if (nrow(positive_standards) == 0) {
  stop("No positive internal standard is available for the integration test.")
}
positive_files <- catalog[catalog$parquet_mode == "pos", , drop = FALSE]
screening_rows <- lapply(seq_len(nrow(positive_files)), function(index) {
  screen_compounds_in_file(
    positive_files$full_path[[index]],
    positive_standards,
    mz_tolerance = 0.01,
    rt_tolerance = 0.5,
    mslevel = "1",
    min_intensity = 1000,
    require_rt_match = TRUE,
    use_dt = TRUE,
    dt_tolerance_pct = 10,
    use_ccs = TRUE,
    ccs_tolerance_pct = 10,
    ccs_calibration = app_environment$ccs_calibration_for_file(positive_files[index, , drop = FALSE]),
    ccs_to_drifttime = app_environment$arcms_ccs_to_drifttime_converter()
  )
})
screening <- do.call(rbind, screening_rows)
if (nrow(screening) != nrow(positive_files) * nrow(positive_standards)) {
  stop("Unexpected number of screening results.")
}
if (any(screening$status == "Error")) {
  stop("Screening failed for at least one file or internal standard.")
}

detected <- sum(screening$status == "Detected", na.rm = TRUE)
cat("test_local_dataset_integration: OK\n")
cat("Parquet files:", length(parquet_files), "\n")
cat("Validated TIC/BPI files:", sum(chromatogram_ok), "\n")
cat("Screening rows:", nrow(screening), "\n")
cat("Detected rows:", detected, "\n")
