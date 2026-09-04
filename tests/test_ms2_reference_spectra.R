#!/usr/bin/env Rscript

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
source(file.path(project_root, "scripts", "ms2_reference_spectra.R"))

reference_csv <- tempfile(fileext = ".csv")
on.exit(unlink(reference_csv), add = TRUE)
utils::write.csv2(
  data.frame(
    reference_id = c("demo-pos", "demo-pos", "demo-pos", "demo-neg"),
    compound_name = c("demo", "demo", "demo", "demo-neg"),
    mode = c("pos", "pos", "pos", "neg"),
    precursor_mz = c(200.1234, 200.1234, 200.1234, 300.1234),
    collision_energy = c("20 eV", "20 eV", "20 eV", "20 eV"),
    fragment_mz = c(50.0, 75.0, 100.0, 80.0),
    relative_intensity = c(100, 50, 25, 100),
    source = c("Test library", "Test library", "Test library", "Test library"),
    library_accession = c("DEMO-1", "DEMO-1", "DEMO-1", "DEMO-2"),
    stringsAsFactors = FALSE
  ),
  reference_csv,
  row.names = FALSE,
  na = ""
)

references <- read_ms2_reference_csv(reference_csv)
stopifnot(nrow(references) == 4L)
stopifnot(identical(unique(references$mode), c("neg", "pos")))
stopifnot(isTRUE(all.equal(
  references$relative_intensity[references$reference_id == "demo-pos"],
  c(100, 50, 25)
)))
summary <- ms2_reference_summary(references)
stopifnot(nrow(summary) == 2L)
stopifnot(all(summary$fragments %in% c(1L, 3L)))

compound <- data.frame(name = "demo", mode = "pos", stringsAsFactors = FALSE)
stopifnot(identical(ms2_reference_ids_for_compound(references, compound), "demo-pos"))

observed <- data.frame(
  mz = c(50.001, 75.002, 140.0),
  intensity = c(1000, 500, 250),
  stringsAsFactors = FALSE
)
comparison <- compare_ms2_spectrum_to_reference(
  observed,
  ms2_reference_spectrum(references, "demo-pos"),
  mz_tolerance = 0.01,
  min_matched_fragments = 2,
  min_cosine_similarity = 0.7
)
stopifnot(identical(comparison$summary$reference_fragments[[1]], 3L))
stopifnot(identical(comparison$summary$matched_fragments[[1]], 2L))
stopifnot(identical(comparison$summary$technical_status[[1]], "Compatible selon seuils exploratoires"))
stopifnot(identical(comparison$matches$matched, c(TRUE, TRUE, FALSE)))
stopifnot(isTRUE(all.equal(comparison$matches$mz_error_da[1:2], c(0.001, 0.002))))

insufficient <- compare_ms2_spectrum_to_reference(
  observed,
  ms2_reference_spectrum(references, "demo-pos"),
  mz_tolerance = 0.01,
  min_matched_fragments = 3,
  min_cosine_similarity = 0.99
)
stopifnot(identical(
  insufficient$summary$technical_status[[1]],
  "Preuve MS2 insuffisante selon seuils exploratoires"
))

no_signal <- compare_ms2_spectrum_to_reference(
  data.frame(mz = numeric(), intensity = numeric()),
  ms2_reference_spectrum(references, "demo-pos")
)
stopifnot(identical(no_signal$summary$technical_status[[1]], "Non evalue : spectre MS2 observe vide"))

invalid_reference <- data.frame(
  compound_name = "bad",
  fragment_mz = 50,
  relative_intensity = 0,
  stringsAsFactors = FALSE
)
invalid_error <- tryCatch(
  normalize_ms2_reference_spectra(invalid_reference),
  error = function(e) e
)
stopifnot(inherits(invalid_error, "error"))

cat("test_ms2_reference_spectra: OK\n")
