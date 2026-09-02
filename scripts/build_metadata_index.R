#!/usr/bin/env Rscript

# Build a small metadata index from arcMS JSON metadata files.
# Usage in terminal (with right directory):
#   Rscript scripts/build_metadata_index.R
#   Rscript scripts/build_metadata_index.R data/raw/json data/processed/metadata_index.csv

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "data/raw/json"
output_file <- if (length(args) >= 2) args[[2]] else "data/processed/metadata_index.csv"

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
}

normalize_missing <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(NA_character_)
  }
  x <- as.character(x[[1]])
  if (is.na(x) || x %in% c("NaN", "NA", "")) {
    return(NA_character_)
  }
  x
}

first_number <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(NA_real_)
  }
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(x)) NA_real_ else x
}

finite_min <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  values <- values[is.finite(values)]
  if (length(values) == 0) NA_real_ else min(values)
}

finite_max <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  values <- values[is.finite(values)]
  if (length(values) == 0) NA_real_ else max(values)
}

extract_match <- function(x, pattern, group = 1) {
  match <- regexec(pattern, x, perl = TRUE)
  parts <- regmatches(x, match)[[1]]
  if (length(parts) <= group) {
    return(NA_character_)
  }
  parts[[group + 1]]
}

path_parts <- function(path) {
  strsplit(path, .Platform$file.sep, fixed = TRUE)[[1]]
}

mode_from_symbol <- function(symbol) {
  if (is.na(symbol)) {
    return(NA_character_)
  }
  if (symbol == "+") {
    return("pos")
  }
  if (symbol == "-") {
    return("neg")
  }
  NA_character_
}

parse_one_json <- function(path, root_dir) {
  metadata <- jsonlite::fromJSON(path, flatten = TRUE)
  sampleinfos <- metadata$sampleinfos
  spectruminfos <- metadata$spectruminfos

  rel_path <- sub(paste0("^", normalizePath(root_dir, mustWork = FALSE), .Platform$file.sep),
                  "", normalizePath(path, mustWork = FALSE))
  parts <- path_parts(rel_path)

  year_dir <- if (length(parts) >= 3) parts[[1]] else NA_character_
  mode_dir <- if (length(parts) >= 3) parts[[2]] else NA_character_

  sample_name <- normalize_missing(sampleinfos$sampleName)
  sample_result_id <- normalize_missing(sampleinfos$id)
  sample_base_name <- normalize_missing(sampleinfos$name)
  sample_group <- normalize_missing(sampleinfos$sample.groupId)
  sample_type <- normalize_missing(sampleinfos$sample.sampleType)
  analysis_name <- normalize_missing(sampleinfos$analysisName)
  acquisition_start <- normalize_missing(sampleinfos$sample.acquisitionStartTime)

  ion_modes <- unique(as.character(spectruminfos$analyticalTechnique.ionisationMode))
  ion_modes <- ion_modes[!is.na(ion_modes) & ion_modes != "NaN"]
  ion_mode_symbol <- if (length(ion_modes) == 1) ion_modes[[1]] else paste(ion_modes, collapse = ",")
  mode_json <- mode_from_symbol(ion_mode_symbol)

  ms_levels <- unique(as.character(spectruminfos$analyticalTechnique.tofGroup.mseLevel))
  ms_levels <- ms_levels[!is.na(ms_levels) & ms_levels != "NaN"]

  file_stem <- sub("-metadata\\.json$", "", basename(path), ignore.case = TRUE)
  parquet_relative_path <- if (!is.na(year_dir) && !is.na(mode_dir)) {
    file.path(year_dir, mode_dir, paste0(file_stem, ".parquet"))
  } else {
    NA_character_
  }

  reference_year <- extract_match(sample_name, "^E-(\\d{4})_(\\d{2})-")
  reference_month <- extract_match(sample_name, "^E-\\d{4}_(\\d{2})-")
  site <- extract_match(sample_name, "^E-\\d{4}_\\d{2}-([^-]+)-")
  duplicate_label <- extract_match(sample_name, "^E-\\d{4}_\\d{2}-[^-]+-([A-Za-z])")
  replicate_label <- extract_match(sample_name, "_replicate_(.+)$")

  data.frame(
    json_path = path,
    json_relative_path = rel_path,
    parquet_relative_path = parquet_relative_path,
    unique_key = paste(year_dir, mode_dir, sample_name, analysis_name, sep = "|"),
    year_dir = year_dir,
    mode_dir = mode_dir,
    mode_json = mode_json,
    mode_consistent = identical(mode_dir, mode_json),
    sample_name = sample_name,
    sample_result_id = sample_result_id,
    sample_base_name = sample_base_name,
    sample_group = sample_group,
    sample_type = sample_type,
    is_blank = identical(sample_type, "Blank") || grepl("^blanc|blank", sample_name, ignore.case = TRUE),
    analysis_name = analysis_name,
    reference_year = reference_year,
    reference_month = reference_month,
    site = site,
    duplicate_label = duplicate_label,
    replicate_label = replicate_label,
    replicate_number_metadata = first_number(sampleinfos$sample.replicateNumber),
    well_position = normalize_missing(sampleinfos$sample.wellPosition),
    injection_volume = first_number(sampleinfos$sample.injectionVolume),
    acquisition_runtime = first_number(sampleinfos$sample.acquisitionRunTime),
    acquisition_start_time = acquisition_start,
    ionisation_mode = ion_mode_symbol,
    ms_levels = paste(ms_levels, collapse = ","),
    has_low_energy = "Low" %in% ms_levels,
    has_high_energy = "High" %in% ms_levels,
    has_ion_mobility = any(spectruminfos$isIonMobilityData %in% TRUE, na.rm = TRUE),
    has_ccs_calibration = any(spectruminfos$hasCCSCalibration %in% TRUE, na.rm = TRUE),
    ccs_calibration_c1 = NA_real_,
    ccs_calibration_c2 = NA_real_,
    low_mass_min = finite_min(spectruminfos$analyticalTechnique.lowMass),
    high_mass_max = finite_max(spectruminfos$analyticalTechnique.highMass),
    stringsAsFactors = FALSE
  )
}

json_files <- list.files(input_dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
json_files <- sort(json_files)

if (length(json_files) == 0) {
  stop("No JSON files found in: ", input_dir)
}

index <- do.call(rbind, lapply(json_files, parse_one_json, root_dir = input_dir))

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(index, output_file, row.names = FALSE, na = "")

cat("Metadata index written to:", output_file, "\n")
cat("Rows:", nrow(index), "\n")
cat("Modes:\n")
print(table(index$mode_dir, useNA = "ifany"))
cat("Sample types:\n")
print(table(index$sample_type, useNA = "ifany"))
