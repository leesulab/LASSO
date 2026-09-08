#!/usr/bin/env Rscript

# Build a metadata index from JSON files and every Parquet found below a data
# root. A Parquet without JSON remains usable through path and filename data.

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "data/raw/json"
output_file <- if (length(args) >= 2) args[[2]] else "data/processed/metadata_index.csv"
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1) {
  stop("Unable to locate build_metadata_index.R.")
}
script_path <- sub("^--file=", "", script_argument[[1]])
source(file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "observatory_metadata.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Package 'jsonlite' is required. From the project root, run: ",
    "Rscript -e 'renv::restore(prompt = FALSE)'"
  )
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

relative_to_input_root <- function(path, root_dir) {
  root_dir <- normalizePath(root_dir, mustWork = TRUE)
  path <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(root_dir, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
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

new_metadata_row <- function(
    json_path = NA_character_, json_relative_path = NA_character_,
    parquet_relative_path = NA_character_, unique_key = NA_character_,
    year_dir = NA_character_, mode_dir = NA_character_, mode_json = NA_character_,
    mode_consistent = NA, json_available = FALSE,
    sample_name = NA_character_, sample_result_id = NA_character_,
    sample_base_name = NA_character_, sample_group = NA_character_,
    sample_type = NA_character_, is_blank = FALSE, analysis_name = NA_character_,
    reference_year = NA_character_, reference_month = NA_character_, site = NA_character_,
    duplicate_label = NA_character_, replicate_label = NA_character_,
    replicate_number_metadata = NA_real_, well_position = NA_character_,
    injection_volume = NA_real_, acquisition_runtime = NA_real_,
    acquisition_start_time = NA_character_, ionisation_mode = NA_character_,
    ms_levels = NA_character_, has_low_energy = NA, has_high_energy = NA,
    has_ion_mobility = NA, has_ccs_calibration = NA,
    ccs_calibration_c1 = NA_real_, ccs_calibration_c2 = NA_real_,
    low_mass_min = NA_real_, high_mass_max = NA_real_) {
  data.frame(
    json_path = json_path,
    json_relative_path = json_relative_path,
    parquet_relative_path = observatory_normalize_path(parquet_relative_path),
    unique_key = unique_key,
    year_dir = year_dir,
    mode_dir = mode_dir,
    mode_json = mode_json,
    mode_consistent = as.logical(mode_consistent),
    json_available = as.logical(json_available),
    sample_name = sample_name,
    sample_result_id = sample_result_id,
    sample_base_name = sample_base_name,
    sample_group = sample_group,
    sample_type = sample_type,
    is_blank = as.logical(is_blank),
    analysis_name = analysis_name,
    reference_year = reference_year,
    reference_month = reference_month,
    site = site,
    duplicate_label = duplicate_label,
    replicate_label = replicate_label,
    replicate_number_metadata = as.numeric(replicate_number_metadata),
    well_position = well_position,
    injection_volume = as.numeric(injection_volume),
    acquisition_runtime = as.numeric(acquisition_runtime),
    acquisition_start_time = acquisition_start_time,
    ionisation_mode = ionisation_mode,
    ms_levels = ms_levels,
    has_low_energy = as.logical(has_low_energy),
    has_high_energy = as.logical(has_high_energy),
    has_ion_mobility = as.logical(has_ion_mobility),
    has_ccs_calibration = as.logical(has_ccs_calibration),
    ccs_calibration_c1 = as.numeric(ccs_calibration_c1),
    ccs_calibration_c2 = as.numeric(ccs_calibration_c2),
    low_mass_min = as.numeric(low_mass_min),
    high_mass_max = as.numeric(high_mass_max),
    stringsAsFactors = FALSE
  )
}

parse_one_json <- function(path, root_dir) {
  metadata <- jsonlite::fromJSON(path, flatten = TRUE)
  sampleinfos <- metadata$sampleinfos
  spectruminfos <- metadata$spectruminfos

  rel_path <- relative_to_input_root(path, root_dir)
  parquet_relative_path <- observatory_json_to_parquet_relative_path(rel_path)[[1]]
  parquet_metadata <- observatory_parse_parquet_metadata(parquet_relative_path)

  sample_name_from_json <- normalize_missing(sampleinfos$sampleName)
  sample_name_metadata <- observatory_parse_name_metadata(sample_name_from_json)
  sample_name <- observatory_first_nonempty(sample_name_from_json, parquet_metadata$sample_name)[[1]]
  sample_base_name <- observatory_first_nonempty(
    normalize_missing(sampleinfos$name),
    parquet_metadata$sample_base_name
  )[[1]]
  sample_group <- observatory_first_nonempty(
    normalize_missing(sampleinfos$sample.groupId),
    parquet_metadata$sample_group
  )[[1]]
  sample_type <- normalize_missing(sampleinfos$sample.sampleType)
  analysis_name <- normalize_missing(sampleinfos$analysisName)
  acquisition_start <- normalize_missing(sampleinfos$sample.acquisitionStartTime)

  ion_modes <- unique(as.character(spectruminfos$analyticalTechnique.ionisationMode))
  ion_modes <- ion_modes[!is.na(ion_modes) & ion_modes != "NaN"]
  ion_mode_symbol <- if (length(ion_modes) == 1) ion_modes[[1]] else paste(ion_modes, collapse = ",")
  mode_json <- mode_from_symbol(ion_mode_symbol)
  mode_dir <- parquet_metadata$mode_dir[[1]]

  ms_levels <- unique(as.character(spectruminfos$analyticalTechnique.tofGroup.mseLevel))
  ms_levels <- ms_levels[!is.na(ms_levels) & ms_levels != "NaN"]

  reference_year <- observatory_first_nonempty(
    parquet_metadata$year_dir,
    observatory_first_nonempty(sample_name_metadata$reference_year, parquet_metadata$reference_year)
  )[[1]]
  reference_month <- observatory_first_nonempty(
    sample_name_metadata$reference_month,
    parquet_metadata$reference_month
  )[[1]]
  duplicate_label <- observatory_first_nonempty(
    sample_name_metadata$duplicate_label,
    parquet_metadata$duplicate_label
  )[[1]]
  replicate_label <- observatory_first_nonempty(
    sample_name_metadata$replicate_label,
    parquet_metadata$replicate_label
  )[[1]]
  is_blank <- identical(sample_type, "Blank") || isTRUE(parquet_metadata$is_blank[[1]]) ||
    isTRUE(grepl("^blanc|blank", sample_name, ignore.case = TRUE))
  if (is_blank && (is.na(sample_type) || identical(sample_type, "Unknown"))) {
    sample_type <- "Blank"
  }
  mode_consistent <- if (isTRUE(mode_dir %in% c("pos", "neg")) && isTRUE(mode_json %in% c("pos", "neg"))) {
    identical(mode_dir, mode_json)
  } else {
    NA
  }
  site <- if (isTRUE(grepl("clichy", sample_name, ignore.case = TRUE))) "Clichy" else NA_character_

  new_metadata_row(
    json_path = path,
    json_relative_path = rel_path,
    parquet_relative_path = parquet_relative_path,
    unique_key = paste(mode_dir, reference_year, reference_month, sample_name, analysis_name, sep = "|"),
    year_dir = parquet_metadata$year_dir[[1]],
    mode_dir = mode_dir,
    mode_json = mode_json,
    mode_consistent = mode_consistent,
    json_available = TRUE,
    sample_name = sample_name,
    sample_result_id = normalize_missing(sampleinfos$id),
    sample_base_name = sample_base_name,
    sample_group = sample_group,
    sample_type = sample_type,
    is_blank = is_blank,
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
    high_mass_max = finite_max(spectruminfos$analyticalTechnique.highMass)
  )
}

parse_one_parquet_without_json <- function(path, root_dir) {
  rel_path <- relative_to_input_root(path, root_dir)
  metadata <- observatory_parse_parquet_metadata(rel_path)
  sample_type <- if (isTRUE(metadata$is_blank[[1]])) "Blank" else "Unknown"

  new_metadata_row(
    parquet_relative_path = rel_path,
    unique_key = paste(metadata$mode_dir, metadata$reference_year, metadata$reference_month, metadata$sample_name, sep = "|"),
    year_dir = metadata$year_dir[[1]],
    mode_dir = metadata$mode_dir[[1]],
    json_available = FALSE,
    sample_name = metadata$sample_name[[1]],
    sample_base_name = metadata$sample_base_name[[1]],
    sample_group = metadata$sample_group[[1]],
    sample_type = sample_type,
    is_blank = metadata$is_blank[[1]],
    reference_year = metadata$reference_year[[1]],
    reference_month = metadata$reference_month[[1]],
    duplicate_label = metadata$duplicate_label[[1]],
    replicate_label = metadata$replicate_label[[1]]
  )
}

copy_suffix_key <- function(paths) {
  sub("\\([0-9]+\\)(?=\\.parquet$)", "", observatory_normalize_path(paths), perl = TRUE)
}

resolve_copy_suffix_associations <- function(index, parquet_relative_paths) {
  if (is.null(index) || nrow(index) == 0 || length(parquet_relative_paths) == 0) {
    return(index)
  }
  available <- observatory_normalize_path(parquet_relative_paths)
  assigned <- index$parquet_relative_path[index$parquet_relative_path %in% available]
  expected_keys <- copy_suffix_key(index$parquet_relative_path)
  available_keys <- copy_suffix_key(available)

  for (index_row in seq_len(nrow(index))) {
    expected <- observatory_normalize_path(index$parquet_relative_path[[index_row]])
    if (expected %in% available) {
      next
    }
    candidates <- available[
      available_keys == expected_keys[[index_row]] & !(available %in% assigned)
    ]
    if (length(candidates) == 1) {
      index$parquet_relative_path[[index_row]] <- candidates[[1]]
      assigned <- c(assigned, candidates[[1]])
    }
  }
  index
}

json_files <- sort(list.files(input_dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE))
parquet_files <- sort(list.files(input_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE))

if (length(json_files) == 0 && length(parquet_files) == 0) {
  stop("No JSON or Parquet files found in: ", input_dir)
}

json_rows <- if (length(json_files) > 0) {
  lapply(json_files, parse_one_json, root_dir = input_dir)
} else {
  list()
}
json_index <- if (length(json_rows) > 0) do.call(rbind, json_rows) else NULL
parquet_relative_paths <- vapply(
  parquet_files,
  relative_to_input_root,
  root_dir = input_dir,
  FUN.VALUE = character(1)
)
json_index <- resolve_copy_suffix_associations(json_index, parquet_relative_paths)
indexed_paths <- if (is.null(json_index)) character() else observatory_normalize_path(json_index$parquet_relative_path)
missing_json_paths <- parquet_files[!observatory_normalize_path(parquet_relative_paths) %in% indexed_paths]
parquet_rows <- if (length(missing_json_paths) > 0) {
  lapply(missing_json_paths, parse_one_parquet_without_json, root_dir = input_dir)
} else {
  list()
}

rows <- c(if (is.null(json_index)) list() else list(json_index), parquet_rows)
index <- do.call(rbind, rows)
index <- index[!duplicated(observatory_normalize_path(index$parquet_relative_path)), , drop = FALSE]
index <- index[order(index$parquet_relative_path), , drop = FALSE]
rownames(index) <- NULL

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(index, output_file, row.names = FALSE, na = "")

cat("Metadata index written to:", output_file, "\n")
cat("Parquet indexed:", nrow(index), "\n")
cat("JSON associated:", sum(index$json_available %in% TRUE), "\n")
cat("Modes:\n")
print(table(index$mode_dir, useNA = "ifany"))
cat("Sample types:\n")
print(table(index$sample_type, useNA = "ifany"))
