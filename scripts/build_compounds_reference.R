#!/usr/bin/env Rscript

# Build a clean compound reference table from the internal standards CSV.
# Usage:
#   Rscript scripts/build_compounds_reference.R
#   Rscript scripts/build_compounds_reference.R data/reference/etalons-internes.csv data/processed/compounds_reference.csv

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[[1]] else "data/reference/etalons-internes.csv"
output_file <- if (length(args) >= 2) args[[2]] else "data/processed/compounds_reference.csv"

required_columns <- c("name", "mode", "mz", "rt", "dt", "ccs")

parse_decimal <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\"", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

normalize_mode <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("+", "positive", "positif")] <- "pos"
  x[x %in% c("-", "negative", "negatif")] <- "neg"
  x
}

raw <- read.csv2(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

compounds <- data.frame(
  compound_id = sprintf("istd_%03d", seq_len(nrow(raw))),
  name = trimws(raw$name),
  mode = normalize_mode(raw$mode),
  mz = parse_decimal(raw$mz),
  rt = parse_decimal(raw$rt),
  dt = parse_decimal(raw$dt),
  ccs = parse_decimal(raw$ccs),
  mz_tolerance = NA_real_,
  rt_tolerance = NA_real_,
  dt_tolerance = NA_real_,
  ccs_tolerance = NA_real_,
  compound_type = "internal_standard",
  source_file = input_file,
  stringsAsFactors = FALSE
)

invalid_modes <- compounds$mode[!compounds$mode %in% c("pos", "neg")]
if (length(invalid_modes) > 0) {
  stop("Invalid modes found: ", paste(unique(invalid_modes), collapse = ", "))
}

numeric_columns <- c("mz", "rt", "dt", "ccs")
for (col in numeric_columns) {
  if (any(is.na(compounds[[col]]))) {
    bad_names <- compounds$name[is.na(compounds[[col]])]
    stop("Invalid numeric values in column '", col, "' for: ", paste(bad_names, collapse = ", "))
  }
}

if (anyDuplicated(compounds$name) > 0) {
  duplicated_names <- unique(compounds$name[duplicated(compounds$name)])
  warning("Duplicated compound names: ", paste(duplicated_names, collapse = ", "))
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(compounds, output_file, row.names = FALSE, na = "")

cat("Compounds reference written to:", output_file, "\n")
cat("Rows:", nrow(compounds), "\n")
cat("Modes:\n")
print(table(compounds$mode, useNA = "ifany"))
