#!/usr/bin/env Rscript

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
source(file.path(project_root, "scripts", "ccs_drift_time.R"))
source(file.path(project_root, "scripts", "parquet_chromatograms.R"))

temporary_parquet <- tempfile(fileext = ".parquet")
on.exit(unlink(temporary_parquet), add = TRUE)

# A mass window containing only zeros is not a detected signal.
zero_signal <- data.frame(
  rt = c(1, 2), scanid = c(1L, 2L), mslevel = c("1", "1"),
  mz = c(100, 100), intensity = c(0, 0)
)
arrow::write_parquet(zero_signal, temporary_parquet)
zero_eic <- compute_eic(temporary_parquet, target_mz = 100, mz_tolerance = 0.01)
zero_summary <- summarise_eic_detection(
  zero_eic,
  expected_rt = 1,
  rt_tolerance = 0.1,
  restrict_to_rt_window = TRUE
)
stopifnot(identical(zero_summary$status[[1]], "Not Detected"))

# A stronger interference outside the RT window must not hide the expected peak.
interference <- data.frame(
  rt = c(5, 10), scanid = c(1L, 2L), mslevel = c("1", "1"),
  mz = c(100, 100), intensity = c(100, 50), dt = c(3, 3)
)
compound <- data.frame(compound_id = "test", name = "test", mode = "pos", mz = 100, rt = 10, dt = 3, ccs = 150)
arrow::write_parquet(interference, temporary_parquet)
single_result <- screen_compound_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = TRUE,
  dt_tolerance_pct = 10
)
batch_result <- screen_compounds_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = TRUE,
  dt_tolerance_pct = 10
)
stopifnot(identical(single_result$status[[1]], "Detected"))
stopifnot(identical(batch_result$status[[1]], "Detected"))
# DT remains exploratory: it does not create level-3 confidence without CCS.
stopifnot(identical(single_result$confidence_level[[1]], 2L))
stopifnot(identical(batch_result$confidence_level[[1]], 2L))
stopifnot(isTRUE(single_result$dt_match[[1]]))
stopifnot(identical(single_result$ccs_status[[1]], "Non evalue: colonne CCS absente"))
stopifnot(identical(single_result$rt_at_max[[1]], 10))
stopifnot(identical(batch_result$rt_at_max[[1]], 10))

# CCS is the third confidence criterion when the Parquet contains the column.
ccs_interference <- interference
ccs_interference$ccs <- c(150, 150)
arrow::write_parquet(ccs_interference, temporary_parquet)
ccs_result <- screen_compound_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = FALSE,
  use_ccs = TRUE,
  ccs_tolerance_pct = 10
)
stopifnot(identical(ccs_result$confidence_level[[1]], 3L))
stopifnot(identical(ccs_result$confidence_label[[1]], "Niveau 3 - m/z + RT + mobilite"))
stopifnot(isTRUE(ccs_result$ccs_match[[1]]))
stopifnot(identical(ccs_result$ccs_status[[1]], "Compatible"))
ccs_batch_result <- screen_compounds_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = FALSE,
  use_ccs = TRUE,
  ccs_tolerance_pct = 10
)
stopifnot(identical(ccs_batch_result$confidence_level[[1]], 3L))
ccs_duckdb_points <- collect_mz_window_points_duckdb(
  temporary_parquet,
  target_mz = 100,
  mz_tolerance = 0.01
)
stopifnot("ccs" %in% names(ccs_duckdb_points))

ccs_mismatch <- ccs_interference
ccs_mismatch$ccs <- c(180, 180)
arrow::write_parquet(ccs_mismatch, temporary_parquet)
ccs_mismatch_result <- screen_compound_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = FALSE,
  use_ccs = TRUE,
  ccs_tolerance_pct = 10
)
stopifnot(identical(ccs_mismatch_result$confidence_level[[1]], 2L))
stopifnot(identical(ccs_mismatch_result$ccs_status[[1]], "Hors tolerance"))

# The future CCS -> DT route accepts only the documented C1/C2 contract.
# The converter below is synthetic and checks plumbing only, not the analytical formula.
arrow::write_parquet(interference, temporary_parquet)
demo_converter <- function(ccs, mz, calibration_parameters) {
  ccs / calibration_parameters$C1 + calibration_parameters$C2 + 0 * mz
}
conversion <- resolve_expected_drifttime_from_ccs(
  ccs = 150,
  mz = 100,
  calibration_parameters = list(c1 = 50, c2 = 0),
  converter = demo_converter
)
stopifnot(isTRUE(conversion$ok))
stopifnot(isTRUE(all.equal(conversion$expected_dt, 3)))
stopifnot(isTRUE(all.equal(conversion$C1, 50)))
stopifnot(isTRUE(all.equal(conversion$C2, 0)))
missing_converter <- resolve_expected_drifttime_from_ccs(
  ccs = 150,
  mz = 100,
  calibration_parameters = list(C1 = 50, C2 = 0)
)
stopifnot(!isTRUE(missing_converter$ok))
stopifnot(identical(missing_converter$status, "Non evalue: fonction CCS -> DT absente"))
calibrated_dt_result <- screen_compound_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = FALSE,
  use_ccs = TRUE,
  dt_tolerance_pct = 10,
  ccs_calibration = list(C1 = 50, C2 = 0),
  ccs_to_drifttime = demo_converter
)
stopifnot(identical(calibrated_dt_result$confidence_level[[1]], 2L))
stopifnot(isTRUE(all.equal(calibrated_dt_result$expected_dt_from_ccs[[1]], 3)))
stopifnot(isTRUE(calibrated_dt_result$ccs_to_dt_match[[1]]))
stopifnot(identical(calibrated_dt_result$ccs_to_dt_status[[1]], "Compatible (exploratoire)"))

# The public helper also accepts a precomputed EIC, even when it has no DT.
precomputed_eic <- compute_eic(temporary_parquet, target_mz = 100, mz_tolerance = 0.01)
precomputed_result <- screen_compound_from_eic(
  temporary_parquet,
  compound,
  precomputed_eic,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = FALSE
)
stopifnot(identical(precomputed_result$confidence_level[[1]], 2L))

# A signal outside its expected RT window is retained as m/z evidence only.
outside_rt <- data.frame(
  rt = 5, scanid = 1L, mslevel = "1", mz = 100, intensity = 100, dt = 3
)
arrow::write_parquet(outside_rt, temporary_parquet)
level_one_result <- screen_compound_in_file(
  temporary_parquet,
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = TRUE,
  dt_tolerance_pct = 10
)
stopifnot(identical(level_one_result$confidence_level[[1]], 1L))
stopifnot(identical(level_one_result$status[[1]], "Not Detected"))
stopifnot(identical(level_one_result$confidence_label[[1]], "Niveau 1 - m/z"))

# A DT mismatch must retain the RT evidence but not grant level 3.
arrow::write_parquet(interference, temporary_parquet)
dt_mismatch_compound <- compound
dt_mismatch_compound$dt <- 4
level_two_result <- screen_compound_in_file(
  temporary_parquet,
  dt_mismatch_compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  min_intensity = 10,
  require_rt_match = TRUE,
  use_dt = TRUE,
  dt_tolerance_pct = 10
)
stopifnot(identical(level_two_result$confidence_level[[1]], 2L))
stopifnot(identical(level_two_result$status[[1]], "Detected"))
stopifnot(identical(level_two_result$dt_status[[1]], "Hors tolerance"))

# Keep this test self-contained: analytical Parquet files are intentionally
# ignored by Git and must not be required to run the automated checks.
demo_parquet <- temporary_parquet
chromatograms <- compute_tic_bpi(demo_parquet, mslevel = "1")
stopifnot(nrow(chromatograms$tic) > 0)
stopifnot(nrow(chromatograms$tic) == nrow(chromatograms$bpi))
stopifnot(identical(chromatograms$tic$scanid, chromatograms$bpi$scanid))

total_intensity <- compute_total_intensity(demo_parquet, mslevel = "1")
stopifnot(nrow(total_intensity) == 1)
stopifnot(isTRUE(all.equal(
  as.numeric(total_intensity$total_intensity[[1]]),
  sum(chromatograms$tic$intensity, na.rm = TRUE)
)))

# DuckDB is the backend used for remote URLs; exercise the same SQL path locally.
duckdb_chromatograms <- compute_tic_bpi_duckdb(demo_parquet, mslevel = "1")
stopifnot(nrow(duckdb_chromatograms$tic) == nrow(chromatograms$tic))
stopifnot(identical(duckdb_chromatograms$tic$scanid, chromatograms$tic$scanid))
duckdb_total_intensity <- compute_total_intensity_duckdb(demo_parquet, mslevel = "1")
stopifnot(isTRUE(all.equal(
  as.numeric(duckdb_total_intensity$total_intensity[[1]]),
  as.numeric(total_intensity$total_intensity[[1]])
)))

# A raw MS2 spectrum is aggregated by m/z bins inside the selected RT window.
ms2_points <- data.frame(
  rt = c(10, 10, 10, 10),
  scanid = c(1L, 1L, 2L, 3L),
  mslevel = c("2", "2", "2", "1"),
  mz = c(50.001, 50.002, 75.001, 50.001),
  intensity = c(100, 200, 300, 1000)
)
arrow::write_parquet(ms2_points, temporary_parquet)
ms2_spectrum <- compute_ms2_spectrum(
  temporary_parquet,
  expected_rt = 10,
  rt_tolerance = 0.1,
  bin_width = 0.01,
  min_intensity = 0,
  top_n = 10
)
stopifnot(nrow(ms2_spectrum) == 2)
stopifnot(isTRUE(all.equal(sum(ms2_spectrum$intensity), 600)))
stopifnot(identical(as.integer(ms2_spectrum$n_points), c(2L, 1L)))

# Remote account access supplies a transient HTTP Authorization header to DuckDB.
demo_headers <- c(Authorization = "Basic ZGVtbzpkZW1v")
stopifnot(identical(normalize_http_headers(demo_headers), demo_headers))
with_duckdb_connection(
  "https://cloud.example.test/protected.parquet",
  function(connection) {
    secrets <- DBI::dbGetQuery(connection, "FROM duckdb_secrets()")
    stopifnot(any(secrets$name == "app_http_headers"))
  },
  http_headers = demo_headers
)

remote_template <- screening_result_template(
  "https://cloud.example.test/public.php/dav/files/demoToken/example.parquet",
  compound,
  mz_tolerance = 0.01,
  rt_tolerance = 0.1,
  dt_tolerance_pct = 10,
  ccs_tolerance_pct = 10,
  require_rt_match = TRUE,
  use_dt = FALSE,
  use_ccs = TRUE
)
stopifnot(is.na(remote_template$file_path[[1]]))

cat("test_chromatograms: OK\n")
