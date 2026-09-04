#!/usr/bin/env Rscript

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
repository_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
project_root <- tempfile("observatoire-hrms-test-")
dir.create(project_root, recursive = TRUE)
on.exit(unlink(project_root, recursive = TRUE, force = TRUE), add = TRUE)

stopifnot(file.copy(file.path(repository_root, "app"), project_root, recursive = TRUE))
stopifnot(file.copy(file.path(repository_root, "scripts"), project_root, recursive = TRUE))
dir.create(file.path(project_root, "data", "processed"), recursive = TRUE)
dir.create(file.path(project_root, "data", "raw", "parquet", "test"), recursive = TRUE)
dir.create(file.path(project_root, "data", "raw", "parquet", "2024", "pos"), recursive = TRUE)

test_metadata <- data.frame(
  parquet_relative_path = "2024/pos/indexed.parquet",
  year_dir = "2024",
  mode_dir = "pos",
  mode_json = "pos",
  mode_consistent = TRUE,
  reference_year = "2024",
  reference_month = "01",
  is_blank = FALSE,
  duplicate_label = "A",
  replicate_label = "1_1",
  sample_name = "E-2024_01-Clichy-A_replicate_1_1",
  sample_base_name = "E-2024_01-Clichy-A",
  sample_group = "E-2024_01-Clichy-A",
  sample_result_id = "test-sample",
  has_ccs_calibration = FALSE,
  ccs_calibration_c1 = NA_real_,
  ccs_calibration_c2 = NA_real_,
  stringsAsFactors = FALSE
)
test_compounds <- data.frame(
  compound_id = c("istd_001", "istd_002", "istd_006"),
  name = c("terbuthylazine-d5", "isoproturon-d6", "triclosan-d3"),
  mode = c("pos", "pos", "neg"),
  mz = c(235.1477, 213.1865, 289.9629),
  rt = c(11.7, 10.152, 15.96),
  dt = c(3.24, 3.16, 3.35),
  ccs = c(158.51, 157.37, 161.48),
  mz_tolerance = NA_real_,
  rt_tolerance = NA_real_,
  dt_tolerance = NA_real_,
  ccs_tolerance = NA_real_,
  compound_type = "internal_standard",
  source_file = "tests/fixtures/standards.csv",
  stringsAsFactors = FALSE
)
test_parquet <- data.frame(
  rt = c(11.6, 11.7, 11.8, 5, 11.7, 11.7),
  scanid = c(1L, 2L, 3L, 4L, 5L, 5L),
  mslevel = c(rep("1", 4), "2", "2"),
  mz = c(235.1477, 235.1477, 235.1477, 500, 50.001, 75.001),
  intensity = c(1000, 5000, 1000, 200, 500, 800),
  dt = c(3.24, 3.24, 3.24, 2, NA_real_, NA_real_),
  stringsAsFactors = FALSE
)
utils::write.csv(test_metadata, file.path(project_root, "data", "processed", "metadata_index.csv"), row.names = FALSE, na = "")
utils::write.csv(test_compounds, file.path(project_root, "data", "processed", "compounds_reference.csv"), row.names = FALSE, na = "")
arrow::write_parquet(test_parquet, file.path(project_root, "data", "raw", "parquet", "test", "pharma_PT6_replicate_1.parquet"))
arrow::write_parquet(test_parquet, file.path(project_root, "data", "raw", "parquet", "2024", "pos", "indexed.parquet"))

saved_project_env <- Sys.getenv(c("PROJECT_ROOT", "DATA_PATH"), unset = NA_character_)
on.exit({
  for (name in names(saved_project_env)) {
    value <- saved_project_env[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      args <- list(value)
      names(args) <- name
      do.call(Sys.setenv, args)
    }
  }
}, add = TRUE)
Sys.setenv(
  PROJECT_ROOT = project_root,
  DATA_PATH = file.path(project_root, "data", "raw", "parquet")
)

app_environment <- new.env(parent = globalenv())
source(file.path(project_root, "app", "app.R"), local = app_environment)

test_extrema <- app_environment$local_extrema_indices(c(0, 4, 1, 3, 0), max_each = 10)
stopifnot(identical(test_extrema$maxima, c(2L, 4L)))
stopifnot(identical(test_extrema$minima, c(3L)))
test_interactive_chart <- app_environment$plot_chromatogram(
  data.frame(rt = c(1, 2, 3), intensity = c(1, 3, 2), scanid = 1:3, n_points = 1),
  "Test"
)
stopifnot(inherits(test_interactive_chart, "plotly"))
test_ms2_widget <- plotly::plotly_build(app_environment$plot_ms2_spectrum(
  data.frame(mz = c(50, 75), intensity = c(100, 200), max_intensity = c(100, 200), n_points = c(1L, 1L)),
  "Test MS2"
))
stopifnot(is.null(test_ms2_widget$x$data[[3]]$hovertemplate))
stopifnot(all(as.character(test_ms2_widget$x$data[[2]]$hoverinfo) == "text"))

safe_export <- app_environment$sanitize_csv_for_export(data.frame(
  name = c("=SUM(A1:A2)", "normal"),
  note = c(" @external", "-text"),
  value = c(-1, 2),
  stringsAsFactors = FALSE
))
stopifnot(identical(safe_export$name[[1]], "'=SUM(A1:A2)"))
stopifnot(identical(safe_export$name[[2]], "normal"))
stopifnot(identical(safe_export$note[[1]], "' @external"))
stopifnot(identical(safe_export$note[[2]], "'-text"))
stopifnot(identical(safe_export$value[[1]], -1))

token_env <- c("NEXTCLOUD_SHARE_TOKEN", "NEXTCLOUD_PUBLIC_URL", "APP_ENV", "NEXTCLOUD_ALLOWED_HOSTS")
saved_token_env <- Sys.getenv(token_env, unset = NA_character_)
on.exit({
  for (name in token_env) {
    value <- saved_token_env[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      args <- list(value)
      names(args) <- name
      do.call(Sys.setenv, args)
    }
  }
}, add = TRUE)
Sys.setenv(
  NEXTCLOUD_SHARE_TOKEN = "server-only-token",
  NEXTCLOUD_PUBLIC_URL = "https://cloud.example.test/s/server-only-token",
  APP_ENV = "",
  NEXTCLOUD_ALLOWED_HOSTS = ""
)
stopifnot(identical(app_environment$default_nextcloud_share_token(), ""))
stopifnot(identical(app_environment$default_nextcloud_url(), "https://cloud.example.test"))
stopifnot(identical(app_environment$resolve_nextcloud_share_token(""), "server-only-token"))
stopifnot(identical(app_environment$resolve_nextcloud_share_token("user-token"), "user-token"))

aggregation_fixture <- data.frame(
  parquet_id = c("a_1", "a_2", "a_3", "b_1", "b_2", "b_3", "blank"),
  file = c("a_1.parquet", "a_2.parquet", "a_3.parquet", "b_1.parquet", "b_2.parquet", "b_3.parquet", "blank.parquet"),
  reference_year = c(rep("2024", 6), NA),
  reference_month = c(rep("01", 6), NA),
  file_type = c(rep("Echantillon", 6), "Blanc"),
  duplicate_label = c(rep("A", 3), rep("B", 3), NA),
  replicate_label = c("1_1", "1_2", "1_3", "1_1", "1_2", "1_3", "1_1"),
  sample_group = c(rep("E-2024_01-Clichy-A", 3), rep("E-2024_01-Clichy-B", 3), "Blanc 80/20"),
  mode = rep("pos", 7),
  compound_id = rep("istd_001", 7),
  compound_name = rep("terbuthylazine-d5", 7),
  status = rep("Detected", 7),
  confidence_level = rep(2L, 7),
  confidence_label = rep("Preuve 2 - m/z + RT", 7),
  signal_value = c(100, 110, 1000, 200, 210, 220, 10),
  monitoring_date = as.Date(c(rep("2024-01-01", 6), NA)),
  period_label = c(rep("2024-01", 6), "Blanc de reference"),
  selected_blank = c(rep(FALSE, 6), TRUE),
  stringsAsFactors = FALSE
)
injection_aggregation <- app_environment$aggregate_monitoring_stage(
  aggregation_fixture,
  stage = "injections",
  method = "median",
  exclude_outliers = TRUE,
  outlier_threshold = 3.5
)
stopifnot(nrow(injection_aggregation$results) == 3)
injection_a <- injection_aggregation$results[injection_aggregation$results$duplicate_label == "A", , drop = FALSE]
stopifnot(isTRUE(all.equal(injection_a$signal_value[[1]], 105)))
stopifnot(identical(injection_a$aggregation_n_excluded[[1]], 1L))
stopifnot(sum(injection_aggregation$audit$aggregation_outlier) == 1L)

duplicate_aggregation <- app_environment$aggregate_monitoring_stage(
  injection_aggregation$results,
  stage = "duplicates",
  method = "mean",
  exclude_outliers = TRUE,
  outlier_threshold = 3.5
)
stopifnot(nrow(duplicate_aggregation$results) == 2)
duplicate_row <- duplicate_aggregation$results[duplicate_aggregation$results$file_type == "Echantillon", , drop = FALSE]
stopifnot(isTRUE(all.equal(duplicate_row$signal_value[[1]], 157.5)))
stopifnot(identical(duplicate_row$aggregation_n_used[[1]], 2L))

separate_injections <- app_environment$aggregate_monitoring_stage(
  aggregation_fixture,
  stage = "injections",
  method = "none",
  exclude_outliers = TRUE,
  outlier_threshold = 3.5
)
stopifnot(nrow(separate_injections$results) == 6)

isoproturon <- app_environment$compounds_reference[
  app_environment$compounds_reference$name == "isoproturon-d6",
  ,
  drop = FALSE
]
stopifnot(nrow(isoproturon) == 1)
stopifnot(identical(isoproturon$ccs[[1]], 157.37))
stopifnot(identical(
  app_environment$monitoring_metric_label("rt_area_sum"),
  "Somme des intensites EIC (fenetre RT)"
))
closest_reference <- app_environment$closest_internal_standard_by_rt(
  data.frame(
    compound_id = c("target", "standard_far", "standard_near"),
    compound_name = c("molecule cible", "etalon loin", "etalon proche"),
    mode = c("pos", "pos", "pos"),
    compound_type = c("suspect", "internal_standard", "internal_standard"),
    expected_rt = c(10, 6, 9.8),
    stringsAsFactors = FALSE
  ),
  target_id = "target",
  target_mode = "pos"
)
stopifnot(identical(closest_reference$compound_id[[1]], "standard_near"))
stopifnot(isTRUE(all.equal(closest_reference$rt_distance[[1]], 0.2)))
stopifnot(identical(
  as.character(app_environment$monitoring_reference_dates("2024", "01", "2026-08-03 12:00:00 UTC")),
  "2024-01-01"
))
duplicate_summary <- app_environment$summarise_monitoring_duplicates(data.frame(
  parquet_id = c("a_1", "a_2", "b_1", "blank"),
  period_label = c("2024-01", "2024-01", "2024-01", "Blanc de reference"),
  duplicate_label = c("A", "A", "B", NA),
  replicate_label = c("1_1", "1_2", "1_1", "1_1"),
  file_type = c("Echantillon", "Echantillon", "Echantillon", "Blanc"),
  status = c("Detected", "Detected", "Detected", "Not Detected"),
  signal_value = c(100, 110, 200, 5),
  stringsAsFactors = FALSE
))
stopifnot(nrow(duplicate_summary) == 2)
duplicate_a <- duplicate_summary[duplicate_summary$duplicat == "A", , drop = FALSE]
stopifnot(identical(duplicate_a$n_fichiers[[1]], 2L))
stopifnot(isTRUE(all.equal(duplicate_a$moyenne[[1]], 105)))
stopifnot(isTRUE(all.equal(duplicate_a$cv_pct[[1]], stats::sd(c(100, 110)) / 105 * 100)))
batch_export_csv <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(
  parquet_id = "sample-a",
  file = "sample-a.parquet",
  compound_id = "istd_001",
  compound_name = "terbuthylazine-d5",
  mode = "POS",
  status = "Detected",
  rt_area_sum = 123.45,
  stringsAsFactors = FALSE
), batch_export_csv, row.names = FALSE)
imported_batch <- app_environment$read_batch_screening_export(batch_export_csv)
stopifnot(identical(imported_batch$mode[[1]], "pos"))
stopifnot(isTRUE(all.equal(imported_batch$rt_area_sum[[1]], 123.45)))
stopifnot("total_intensity" %in% names(imported_batch))
stopifnot("ccs_to_dt_status" %in% names(imported_batch))

batch_eic_parameters <- app_environment$batch_result_eic_parameters(data.frame(
  parquet_id = "nextcloud::2024/pos/sample.parquet",
  file_mode = "pos",
  compound_id = "suspect_001",
  compound_name = "suspect-test",
  expected_mz = 321.1234,
  mz_tolerance = 0.01,
  expected_rt = 7.25,
  rt_tolerance = 0.5,
  min_intensity = 1000,
  mslevel = "1",
  stringsAsFactors = FALSE
))
stopifnot(isTRUE(batch_eic_parameters$ok))
stopifnot(identical(batch_eic_parameters$parquet_id, "nextcloud::2024/pos/sample.parquet"))
stopifnot(identical(batch_eic_parameters$file_mode, "pos"))
stopifnot(isTRUE(all.equal(batch_eic_parameters$target_mz, 321.1234)))
stopifnot(isTRUE(all.equal(batch_eic_parameters$expected_rt, 7.25)))
stopifnot(identical(batch_eic_parameters$mslevel, "1"))
batch_monitoring_parameters <- app_environment$batch_result_monitoring_parameters(data.frame(
  compound_id = "suspect_001",
  compound_name = "suspect-test",
  mode = "POS",
  stringsAsFactors = FALSE
))
stopifnot(isTRUE(batch_monitoring_parameters$ok))
stopifnot(identical(batch_monitoring_parameters$compound_id, "suspect_001"))
stopifnot(identical(batch_monitoring_parameters$mode, "pos"))

import_csv <- tempfile(fileext = ".csv")
ms2_reference_csv <- tempfile(fileext = ".csv")
on.exit(unlink(c(import_csv, ms2_reference_csv)), add = TRUE)
writeLines(c(
  "name;mode;mz;rt;dt;ccs",
  "etalon-import;neg;123,456;4,50;2,10;150,20"
), import_csv)
writeLines(c(
  "reference_id;compound_name;mode;precursor_mz;collision_energy;fragment_mz;relative_intensity;source",
  "terbuthylazine-d5-pos;terbuthylazine-d5;pos;235,1477;20 eV;50,001;100;Test MS2",
  "terbuthylazine-d5-pos;terbuthylazine-d5;pos;235,1477;20 eV;75,001;50;Test MS2"
), ms2_reference_csv)
imported_raw <- app_environment$read_compounds_csv(import_csv)
stopifnot(identical(names(imported_raw), c("name", "mode", "mz", "rt", "dt", "ccs")))
imported_compounds <- app_environment$prepare_custom_compounds(
  imported_raw,
  app_ids = "custom_000001",
  source_label = "CSV: test.csv",
  source_file = "test.csv"
)
stopifnot(identical(imported_compounds$mode[[1]], "neg"))
stopifnot(identical(imported_compounds$mz[[1]], 123.456))
stopifnot(identical(imported_compounds$compound_type[[1]], "internal_standard"))

suspect_compounds <- app_environment$prepare_custom_compounds(
  data.frame(
    name = "suspect-test",
    mode = "pos",
    mz = "321.1234",
    rt = "8.50",
    stringsAsFactors = FALSE
  ),
  app_ids = "suspect_000001",
  source_label = "CSV suspect: test.csv",
  source_file = "test.csv",
  default_compound_type = "suspect"
)
stopifnot(identical(suspect_compounds$compound_type[[1]], "suspect"))
stopifnot(identical(
  app_environment$compound_type_label(c("internal_standard", "suspect")),
  c("Etalon interne", "Suspect")
))
test_suspect_copies <- app_environment$make_test_suspects_from_internal_standards(
  app_environment$compounds_reference[1:2, , drop = FALSE],
  app_ids = c("custom_100001", "custom_100002")
)
stopifnot(identical(test_suspect_copies$compound_type, c("suspect", "suspect")))
stopifnot(identical(test_suspect_copies$compound_id, c("suspect_test_istd_001", "suspect_test_istd_002")))
stopifnot(identical(test_suspect_copies$name, c("[test] terbuthylazine-d5", "[test] isoproturon-d6")))

catalog <- app_environment$parquet_catalog(file.path(project_root, "data", "raw", "parquet"))
stopifnot(identical(catalog$parquet_id, catalog$relative_path))
unindexed_catalog_file <- catalog[catalog$relative_path == "test/pharma_PT6_replicate_1.parquet", , drop = FALSE]
stopifnot(nrow(unindexed_catalog_file) == 1)
stopifnot(identical(unindexed_catalog_file$mode_source[[1]], "Inconnu"))

synthetic_files <- data.frame(
  parquet_id = "2024/neg/example.parquet",
  relative_path = "2024/neg/example.parquet",
  full_path = "/tmp/example.parquet",
  size = 1,
  size_label = "1.00 B",
  modified = "2026-07-27 00:00:00",
  stringsAsFactors = FALSE
)
synthetic_metadata <- data.frame(
  parquet_relative_path = "2024/neg/example.parquet",
  mode_dir = "neg",
  reference_year = "2024",
  reference_month = "06",
  file_type = "Echantillon",
  duplicate_label = "A",
  replicate_label = "1_1",
  sample_group = "E-2024_06-Clichy-A",
  sample_name = "E-2024_06-Clichy-A_replicate_1_1",
  stringsAsFactors = FALSE
)
matched <- app_environment$enrich_parquet_files(synthetic_files, synthetic_metadata)
stopifnot(isTRUE(matched$metadata_match[[1]]))
stopifnot(identical(matched$parquet_mode[[1]], "neg"))
stopifnot(identical(matched$sample_group[[1]], "E-2024_06-Clichy-A"))
stopifnot(identical(matched$mode_source[[1]], "JSON"))

ambiguous_blank_files <- synthetic_files
ambiguous_blank_files$parquet_id <- "observatoire-db/2024/neg/Blanc 8020 EUPMeOH_replicate_1.parquet"
ambiguous_blank_files$relative_path <- ambiguous_blank_files$parquet_id
ambiguous_blank_metadata <- synthetic_metadata
ambiguous_blank_metadata$parquet_relative_path <- "2024/pos/Blanc 8020 EUPMeOH_replicate_1.parquet"
ambiguous_blank_metadata$mode_dir <- "pos"
ambiguous_blank <- app_environment$enrich_parquet_files(ambiguous_blank_files, ambiguous_blank_metadata)
stopifnot(!isTRUE(ambiguous_blank$metadata_match[[1]]))
stopifnot(identical(ambiguous_blank$path_mode[[1]], "neg"))
stopifnot(identical(ambiguous_blank$parquet_mode[[1]], "neg"))
stopifnot(identical(ambiguous_blank$mode_source[[1]], "Chemin"))

wrong_year_files <- synthetic_files
wrong_year_files$parquet_id <- "observatoire-db/2024/pos/example.parquet"
wrong_year_files$relative_path <- wrong_year_files$parquet_id
wrong_year_metadata <- synthetic_metadata
wrong_year_metadata$parquet_relative_path <- "2021/pos/example.parquet"
wrong_year_metadata$mode_dir <- "pos"
wrong_year_metadata$reference_year <- "2021"
wrong_year <- app_environment$enrich_parquet_files(wrong_year_files, wrong_year_metadata)
stopifnot(!isTRUE(wrong_year$metadata_match[[1]]))
stopifnot(is.na(wrong_year$reference_year[[1]]))
stopifnot(identical(wrong_year$parquet_mode[[1]], "pos"))
blank_choices <- app_environment$make_nextcloud_file_choices(data.frame(
  path = c(
    "observatoire-db/2024/pos/Blanc 8020 EUPMeOH_replicate_1.parquet",
    "observatoire-db/2024/neg/Blanc 8020 EUPMeOH_replicate_1.parquet"
  ),
  size = c(1, 2),
  stringsAsFactors = FALSE
))
stopifnot(any(grepl("[POS]", names(blank_choices), fixed = TRUE)))
stopifnot(any(grepl("[NEG]", names(blank_choices), fixed = TRUE)))

prefixed_files <- synthetic_files
prefixed_files$parquet_id <- "observatoire-db/2024/neg/example.parquet"
prefixed_files$relative_path <- prefixed_files$parquet_id
prefixed <- app_environment$enrich_parquet_files(prefixed_files, synthetic_metadata)
stopifnot(isTRUE(prefixed$metadata_match[[1]]))
stopifnot(identical(prefixed$reference_year[[1]], "2024"))

filename_only_files <- synthetic_files
filename_only_files$parquet_id <- "observatoire-db/2023/pos/2024-02D_replicate_1_3.parquet"
filename_only_files$relative_path <- filename_only_files$parquet_id
filename_only <- app_environment$enrich_parquet_files(filename_only_files, synthetic_metadata)
stopifnot(!isTRUE(filename_only$metadata_match[[1]]))
stopifnot(identical(filename_only$reference_year[[1]], "2024"))
stopifnot(identical(filename_only$reference_month[[1]], "02"))
stopifnot(identical(filename_only$duplicate_label[[1]], "D"))
stopifnot(identical(filename_only$replicate_label[[1]], "1_3"))
stopifnot(identical(filename_only$file_type[[1]], "Echantillon"))
stopifnot(identical(filename_only$sample_name[[1]], "2024-02D_replicate_1_3"))
stopifnot(identical(filename_only$parquet_mode[[1]], "pos"))

catalog_diagnostics <- app_environment$catalog_file_diagnostics(matched)
stopifnot(nrow(catalog_diagnostics) == 1)
stopifnot(identical(catalog_diagnostics$statut[[1]], "OK"))
bad_catalog_file <- matched
bad_catalog_file$metadata_match <- FALSE
bad_catalog_file$path_mode <- "pos"
bad_catalog_file$metadata_mode <- "neg"
bad_catalog_file$parquet_mode <- "neg"
bad_catalog_diagnostics <- app_environment$catalog_file_diagnostics(bad_catalog_file)
stopifnot(identical(bad_catalog_diagnostics$statut[[1]], "A corriger"))
stopifnot(grepl("modes JSON et chemin", bad_catalog_diagnostics$details[[1]], fixed = TRUE))

schema_diagnostics <- app_environment$parquet_file_diagnostics(
  info = list(
    columns = c("rt", "scanid", "mslevel", "mz", "intensity", "dt", "ccs"),
    summary = data.frame(rows = 10, stringsAsFactors = FALSE),
    ms_levels = data.frame(mslevel = c("1", "2"), rows = c(8, 2), stringsAsFactors = FALSE)
  ),
  file = matched
)
required_columns_check <- schema_diagnostics[schema_diagnostics$controle == "Colonnes requises", , drop = FALSE]
ccs_check <- schema_diagnostics[schema_diagnostics$controle == "CCS", , drop = FALSE]
stopifnot(identical(required_columns_check$statut[[1]], "OK"))
stopifnot(identical(ccs_check$valeur[[1]], "Disponible"))
calibration_file <- matched
calibration_file$has_ccs_calibration <- TRUE
calibration_file$ccs_calibration_c1 <- 1.2
calibration_file$ccs_calibration_c2 <- 0.3
calibration_diagnostics <- app_environment$parquet_file_diagnostics(
  info = list(
    columns = c("rt", "scanid", "mslevel", "mz", "intensity", "dt"),
    summary = data.frame(rows = 10, stringsAsFactors = FALSE),
    ms_levels = data.frame(mslevel = "1", rows = 10, stringsAsFactors = FALSE)
  ),
  file = calibration_file
)
calibration_check <- calibration_diagnostics[calibration_diagnostics$controle == "Calibration CCS JSON", , drop = FALSE]
stopifnot(identical(calibration_check$valeur[[1]], "Declaree"))
stopifnot(grepl("C1/C2 disponibles", calibration_check$details[[1]], fixed = TRUE))
missing_schema_diagnostics <- app_environment$parquet_file_diagnostics(
  info = list(
    columns = c("rt", "scanid", "mz"),
    summary = data.frame(rows = 0, stringsAsFactors = FALSE),
    ms_levels = data.frame(mslevel = character(), rows = numeric(), stringsAsFactors = FALSE)
  )
)
missing_required_check <- missing_schema_diagnostics[missing_schema_diagnostics$controle == "Colonnes requises", , drop = FALSE]
stopifnot(identical(missing_required_check$statut[[1]], "A corriger"))
global_checks <- app_environment$global_data_checks(synthetic_metadata, app_environment$compounds_reference, matched)
stopifnot(all(c("Parquet catalogue avec JSON associe", "Parquet prets pour screening") %in% global_checks$controle))

remote_items <- data.frame(
  name = "example.parquet",
  path = "2024/neg/example.parquet",
  is_folder = FALSE,
  size = 1234,
  modified = "2026-07-28 10:00:00",
  url = "https://cloud.example.test/public.php/dav/files/demoToken/2024/neg/example.parquet",
  stringsAsFactors = FALSE
)
remote_catalog <- app_environment$nextcloud_to_parquet_files(remote_items)
stopifnot(identical(remote_catalog$parquet_id[[1]], "nextcloud::2024/neg/example.parquet"))
stopifnot(identical(remote_catalog$source_type[[1]], "nextcloud"))
remote_choices <- app_environment$make_parquet_choices(remote_catalog)
stopifnot(!any(grepl("demoToken", names(remote_choices), fixed = TRUE)))
enriched_remote_catalog <- app_environment$enrich_parquet_files(remote_catalog, synthetic_metadata)
screening_source_choices <- app_environment$make_screening_source_choices(enriched_remote_catalog)
stopifnot(!any(grepl("demoToken", names(screening_source_choices), fixed = TRUE)))

metadata_selection <- data.frame(
  parquet_relative_path = "2024/pos/indexed.parquet",
  stringsAsFactors = FALSE
)
available_sources <- data.frame(
  parquet_id = c(
    "nextcloud::observatoire-db/2024/pos/indexed.parquet",
    "2024/pos/indexed.parquet"
  ),
  relative_path = c(
    "observatoire-db/2024/pos/indexed.parquet",
    "2024/pos/indexed.parquet"
  ),
  source_type = c("nextcloud", "local"),
  stringsAsFactors = FALSE
)
matching_source_ids <- app_environment$matching_parquet_ids_for_metadata(metadata_selection, available_sources)
stopifnot(identical(matching_source_ids, "2024/pos/indexed.parquet"))

shiny::testServer(app_environment$server, {
  session$setInputs(screening_source_catalog_table_rows_selected = 1)
  session$setInputs(add_screening_sources = 1)
  stopifnot(identical(output$plan_source_selection_count, "1"))

  session$setInputs(files_table_rows_selected = 1)
  session$setInputs(add_selected_files = 1)
  stopifnot("2024/pos/indexed.parquet" %in% selected_screening_source_ids())

  session$setInputs(
    parquet_file_id = "test/pharma_PT6_replicate_1.parquet",
    parquet_mode_override = "auto",
    run_current_file_screening = 1
  )
  stopifnot(identical(output$screening_total_count, "-"))

  session$setInputs(
    parquet_mode_override = "pos",
    chrom_mslevel = "1",
    eic_mz_tolerance = 0.01,
    eic_rt_tolerance = 0.5,
    screening_min_intensity = 1000,
    screen_require_rt_match = TRUE,
    screen_use_dt = FALSE,
    screen_dt_tolerance_pct = 10,
    screen_use_ccs = TRUE,
    screen_ccs_tolerance_pct = 10,
    run_current_file_screening = 2
  )
  stopifnot(identical(
    output$screening_total_count,
    as.character(sum(app_environment$compounds_reference$mode == "pos"))
  ))
  stopifnot(!identical(output$screening_level_two_count, "-"))
  stopifnot(isTRUE(all(screening_results()$use_ccs)))
  stopifnot(isTRUE(all.equal(unique(screening_results()$ccs_tolerance_pct), 10)))

  session$setInputs(
    eic_compound_id = "",
    quick_eic_target_mz = 235.1477,
    eic_mz_tolerance = 0.01,
    run_quick_eic = 1
  )
  stopifnot(!is.null(eic_data()))
  stopifnot(nrow(eic_data()) == 3)
  stopifnot(!is.null(quick_eic_result()))
  stopifnot(isTRUE(quick_eic_result()$detected))
  stopifnot(grepl("Signal m/z observe", output$quick_eic_status, fixed = TRUE))
  stopifnot(identical(eic_context()$label, "Recherche libre"))

  session$setInputs(
    eic_compound_id = "compound_1",
    quick_eic_target_mz = 235.1477,
    run_quick_eic = 2
  )
  stopifnot(identical(eic_context()$label, "terbuthylazine-d5"))

  session$setInputs(
    eic_expected_rt = 11.7,
    eic_rt_tolerance = 0.5,
    ms2_bin_width = 0.01,
    ms2_min_intensity = 0,
    ms2_top_n = 20,
    compute_ms2_spectrum = 1
  )
  stopifnot(!is.null(ms2_spectrum_data()))
  stopifnot(nrow(ms2_spectrum_data()) == 2)

  session$setInputs(
    import_ms2_reference_csv = data.frame(
      name = "spectres-ms2-test.csv",
      size = file.info(ms2_reference_csv)$size,
      type = "text/csv",
      datapath = ms2_reference_csv,
      stringsAsFactors = FALSE
    )
  )
  session$setInputs(
    ms2_reference_id = "terbuthylazine-d5-pos",
    ms2_match_mz_tolerance = 0.01,
    ms2_min_matched_fragments = 2,
    ms2_min_cosine_similarity = 0.7,
    compare_ms2_spectrum = 1
  )
  stopifnot(!is.null(ms2_reference_comparison()))
  stopifnot(identical(ms2_reference_comparison()$summary$matched_fragments[[1]], 2L))
  stopifnot(identical(
    ms2_reference_comparison()$summary$technical_status[[1]],
    "Compatible selon seuils exploratoires"
  ))
  stopifnot(!is.null(output$ms2_comparison_summary_table))
  stopifnot(!is.null(output$ms2_comparison_table))

  session$setInputs(
    control_file_id = "test/pharma_PT6_replicate_1.parquet",
    run_control_file_checks = 1
  )
  stopifnot(!is.null(output$checks_table))
  stopifnot(!is.null(output$control_catalog_table))
  stopifnot(!is.null(output$control_file_checks_table))

  batch_catalog <- parquet_files()
  batch_catalog <- batch_catalog[
    batch_catalog$relative_path == "test/pharma_PT6_replicate_1.parquet",
    ,
    drop = FALSE
  ]
  batch_catalog$parquet_mode <- "pos"
  batch_catalog$mode_source <- "Test"
  parquet_files(batch_catalog)
  selected_screening_source_ids(batch_catalog$parquet_id)
  selected_compound_ids(
    app_environment$compounds_reference$app_compound_id[
      app_environment$compounds_reference$mode == "pos"
    ]
  )
  session$setInputs(
    screen_mode = "",
    screen_include_blanks = TRUE,
    screen_mz_tol = 0.01,
    screen_batch_rt_tol = 0.5,
    screen_min_intensity = 1000,
    screen_batch_mslevel = "1",
    screen_batch_compute_total_intensity = FALSE,
    screen_batch_require_rt = TRUE,
    screen_batch_use_dt = FALSE,
    screen_batch_dt_tolerance_pct = 10,
    screen_batch_use_ccs = TRUE,
    screen_batch_ccs_tolerance_pct = 10,
    confirm_batch_screening = 1
  )
  stopifnot(identical(output$batch_result_files, "1"))
  stopifnot(identical(
    output$batch_result_rows,
    as.character(sum(app_environment$compounds_reference$mode == "pos"))
  ))
  batch_level_counts <- as.integer(c(
    output$batch_level_zero_count,
    output$batch_level_one_count,
    output$batch_level_two_count,
    output$batch_level_three_count
  ))
  stopifnot(sum(batch_level_counts) == as.integer(output$batch_result_rows))
  stopifnot(grepl("Termine", output$batch_screening_status, fixed = TRUE))
  stopifnot(isTRUE(all(batch_screening_results()$use_ccs)))
  stopifnot(isTRUE(all.equal(unique(batch_screening_results()$ccs_tolerance_pct), 10)))
  stopifnot(isTRUE(all(batch_screening_results()$compound_type == "internal_standard")))
  stopifnot(isTRUE(all.equal(unique(batch_screening_results()$min_intensity), 1000)))
  stopifnot(isTRUE(all(is.na(batch_screening_results()$total_intensity))))
  stopifnot(isTRUE(all(batch_screening_results()$total_intensity_status == "Non demandee")))

  session$setInputs(batch_screening_results_table_rows_selected = 1)
  selected_batch_row <- selected_batch_screening_result()
  stopifnot(nrow(selected_batch_row) == 1)
  stopifnot(identical(selected_batch_row$parquet_id[[1]], "test/pharma_PT6_replicate_1.parquet"))
  session$setInputs(prepare_batch_result_eic = 1)
  session$setInputs(follow_batch_result_molecule = 1)

  session$setInputs(
    manual_compound_name = "etalon-manuel",
    manual_compound_mode = "neg",
    manual_compound_mz = "321,1234",
    manual_compound_rt = "7,25",
    manual_compound_dt = "",
    manual_compound_ccs = "",
    manual_compound_auto_select = TRUE,
    confirm_manual_compound = 1
  )
  custom_rows <- compound_catalog()[compound_catalog()$source_label == "Saisie manuelle", , drop = FALSE]
  stopifnot(nrow(custom_rows) == 1)
  stopifnot(identical(custom_rows$mz[[1]], 321.1234))
  stopifnot(custom_rows$app_compound_id[[1]] %in% selected_compound_ids())
  stopifnot(identical(output$n_custom_compounds, "1"))

  session$setInputs(reset_custom_compounds = 1)
  stopifnot(identical(output$n_custom_compounds, "0"))

  session$setInputs(
    import_compounds_auto_select = TRUE,
    import_compounds_csv = data.frame(
      name = "import-test.csv",
      size = file.info(import_csv)$size,
      type = "text/csv",
      datapath = import_csv,
      stringsAsFactors = FALSE
    )
  )
  imported_rows <- compound_catalog()[compound_catalog()$source_label == "CSV: import-test.csv", , drop = FALSE]
  stopifnot(nrow(imported_rows) == 1)
  stopifnot(imported_rows$app_compound_id[[1]] %in% selected_compound_ids())

  session$setInputs(reset_custom_compounds = 2)
  stopifnot(identical(output$n_custom_compounds, "0"))

  selected_compound_ids("compound_1")
  session$setInputs(create_test_suspects_from_internal_standards = 1)
  copied_suspect_rows <- compound_catalog()[
    compound_catalog()$source_label == "Etalon interne copie pour test",
    ,
    drop = FALSE
  ]
  stopifnot(nrow(copied_suspect_rows) == 1)
  stopifnot(identical(copied_suspect_rows$compound_type[[1]], "suspect"))
  stopifnot(identical(copied_suspect_rows$name[[1]], "[test] terbuthylazine-d5"))
  stopifnot(copied_suspect_rows$app_compound_id[[1]] %in% selected_compound_ids())

  session$setInputs(reset_custom_suspects = 1)
  stopifnot(identical(output$n_custom_suspects, "0"))

  session$setInputs(
    manual_suspect_name = "suspect-manuel",
    manual_suspect_mode = "pos",
    manual_suspect_mz = "321,1234",
    manual_suspect_rt = "7,25",
    manual_suspect_dt = "",
    manual_suspect_ccs = "",
    manual_suspect_auto_select = TRUE,
    confirm_manual_suspect = 1
  )
  suspect_rows <- compound_catalog()[compound_catalog()$source_label == "Saisie manuelle - suspect", , drop = FALSE]
  stopifnot(nrow(suspect_rows) == 1)
  stopifnot(identical(suspect_rows$compound_type[[1]], "suspect"))
  stopifnot(suspect_rows$app_compound_id[[1]] %in% selected_compound_ids())
  stopifnot(identical(output$n_custom_suspects, "1"))
  stopifnot(nrow(selected_suspects()) == 1)
  stopifnot(nrow(selected_internal_standards()) > 0)

  session$setInputs(reset_custom_suspects = 1)
  stopifnot(identical(output$n_custom_suspects, "0"))

  monitoring_fixture <- data.frame(
    parquet_id = c("sample-a", "blank-pos", "sample-b", "sample-neg", "sample-a", "blank-pos", "sample-b"),
    file = c("sample-a.parquet", "blank-pos.parquet", "sample-b.parquet", "sample-neg.parquet", "sample-a.parquet", "blank-pos.parquet", "sample-b.parquet"),
    reference_year = c("2024", NA, "2024", "2024", "2024", NA, "2024"),
    reference_month = c("01", NA, "01", "01", "01", NA, "01"),
    file_type = c("Echantillon", "Blanc", "Echantillon", "Echantillon", "Echantillon", "Blanc", "Echantillon"),
    duplicate_label = c("A", NA, "B", "A", "A", NA, "B"),
    replicate_label = c("1_1", "1_1", "1_2", "1_1", "1_1", "1_1", "1_2"),
    compound_id = c("istd_001", "istd_001", "istd_001", "istd_006", "istd_002", "istd_002", "istd_002"),
    compound_name = c("terbuthylazine-d5", "terbuthylazine-d5", "terbuthylazine-d5", "triclosan-d3", "isoproturon-d6", "isoproturon-d6", "isoproturon-d6"),
    mode = c("pos", "pos", "pos", "neg", "pos", "pos", "pos"),
    status = c("Detected", "Detected", "Detected", "Not Detected", "Detected", "Not Detected", "Detected"),
    confidence_level = c(2L, 2L, 2L, 1L, 2L, 0L, 2L),
    confidence_label = c("Preuve 2 - m/z + RT", "Preuve 2 - m/z + RT", "Preuve 2 - m/z + RT", "Preuve 1 - m/z", "Preuve 2 - m/z + RT", "Preuve 0 - aucun signal", "Preuve 2 - m/z + RT"),
    expected_rt = c(11.7, 11.7, 11.7, 15.96, 10.152, 10.152, 10.152),
    rt_area_sum = c(1000, 100, 1200, 300, 500, 20, 550),
    rt_max_intensity = c(500, 50, 600, 150, 250, 10, 275),
    rt_at_max = c(11.7, 11.7, 11.7, 16, 10.15, 10.15, 10.15),
    total_intensity = c(10000, 1000, 12000, 9000, 10000, 1000, 12000),
    total_intensity_mslevel = rep("1", 7),
    total_intensity_error = rep("", 7),
    screened_at = rep("2026-08-03 12:00:00 UTC", 7),
    stringsAsFactors = FALSE
  )
  batch_screening_results(monitoring_fixture)
  session$setInputs(
    monitoring_compound_id = "istd_001",
    monitoring_mode = "pos",
    monitoring_metric = "rt_area_sum",
    monitoring_treatment = "raw",
    monitoring_normalization = "none",
    monitoring_normalization_compound_id = "",
    monitoring_scale = "linear",
    monitoring_status = "",
    monitoring_include_blanks = TRUE,
    monitoring_blank_file_id = "blank-pos"
  )
  monitoring_rows <- monitoring_view_results()
  stopifnot(nrow(monitoring_rows) == 3)
  stopifnot(sum(monitoring_rows$selected_blank) == 1)
  blank_row <- monitoring_rows[monitoring_rows$parquet_id == "blank-pos", , drop = FALSE]
  stopifnot(is.na(blank_row$monitoring_date[[1]]))
  stopifnot(identical(blank_row$period_label[[1]], "Blanc de reference"))
  stopifnot(identical(output$monitoring_files_count, "3"))
  stopifnot(identical(output$monitoring_detected_count, "3"))
  stopifnot(identical(output$monitoring_blank_signal, "100"))
  stopifnot(identical(app_environment$format_monitoring_value(0.0000123), "1.23e-05"))
  stopifnot(!is.null(output$monitoring_results_table))
  stopifnot(!is.null(output$monitoring_raw_results_table))
  stopifnot(!is.null(output$monitoring_duplicates_table))
  stopifnot(!is.null(output$monitoring_plot))
  reference_candidates <- monitoring_normalization_reference_candidates()
  stopifnot(any(reference_candidates$compound_id == "istd_002"))

  session$setInputs(monitoring_treatment = "blank_corrected")
  corrected_rows <- monitoring_view_results()
  stopifnot(identical(corrected_rows$signal_corrige_blanc[corrected_rows$parquet_id == "sample-a"], 900))
  stopifnot(identical(corrected_rows$signal_corrige_blanc[corrected_rows$parquet_id == "sample-b"], 1100))
  stopifnot(identical(corrected_rows$signal_corrige_blanc[corrected_rows$parquet_id == "blank-pos"], 0))
  stopifnot(!is.null(output$monitoring_plot))

  session$setInputs(
    monitoring_normalization = "internal_standard",
    monitoring_normalization_compound_id = "istd_002"
  )
  internal_normalized_rows <- monitoring_view_results()
  stopifnot(isTRUE(all.equal(
    internal_normalized_rows$signal_normalise[internal_normalized_rows$parquet_id == "sample-a"],
    900 / 480
  )))
  stopifnot(isTRUE(all.equal(
    internal_normalized_rows$signal_normalise[internal_normalized_rows$parquet_id == "sample-b"],
    1100 / 530
  )))
  stopifnot(identical(
    internal_normalized_rows$normalisation_status[internal_normalized_rows$parquet_id == "blank-pos"],
    "Etalon non detecte"
  ))
  stopifnot(!is.null(output$monitoring_plot))

  session$setInputs(
    monitoring_normalization = "closest_internal_standard",
    monitoring_normalization_compound_id = ""
  )
  closest_reference <- monitoring_closest_normalization_reference()
  stopifnot(identical(closest_reference$compound_id[[1]], "istd_002"))
  stopifnot(isTRUE(all.equal(closest_reference$target_expected_rt[[1]], 11.7)))
  stopifnot(isTRUE(all.equal(closest_reference$rt_distance[[1]], 1.548)))
  closest_normalized_rows <- monitoring_view_results()
  stopifnot(isTRUE(all.equal(
    closest_normalized_rows$signal_normalise[closest_normalized_rows$parquet_id == "sample-a"],
    900 / 480
  )))
  stopifnot(identical(
    closest_normalized_rows$normalisation_reference_id[closest_normalized_rows$parquet_id == "sample-b"],
    "istd_002"
  ))
  stopifnot(!is.null(output$monitoring_closest_reference_details))
  stopifnot(!is.null(output$monitoring_plot))

  session$setInputs(monitoring_normalization = "total_intensity")
  total_normalized_rows <- monitoring_view_results()
  stopifnot(isTRUE(all.equal(
    total_normalized_rows$signal_normalise[total_normalized_rows$parquet_id == "sample-a"],
    900 / 10000
  )))
  stopifnot(identical(
    total_normalized_rows$normalisation_status[total_normalized_rows$parquet_id == "sample-b"],
    "Appliquee"
  ))
  stopifnot(!is.null(output$monitoring_plot))

  session$setInputs(monitoring_scale = "log")
  stopifnot(!is.null(output$monitoring_plot))

  session$setInputs(
    monitoring_injection_aggregation = "mean",
    monitoring_injection_exclude_outliers = TRUE,
    monitoring_duplicate_aggregation = "median",
    monitoring_duplicate_exclude_outliers = TRUE,
    monitoring_outlier_threshold = 3.5
  )
  aggregated_monitoring <- monitoring_display_results()
  stopifnot(nrow(aggregated_monitoring) == 2)
  stopifnot(!is.null(output$monitoring_results_table))
  stopifnot(!is.null(output$monitoring_raw_results_table))

  utils::write.csv(monitoring_fixture, batch_export_csv, row.names = FALSE)
  session$setInputs(import_batch_screening_csv = data.frame(
    name = "screening_lot_test.csv",
    size = file.info(batch_export_csv)$size,
    type = "text/csv",
    datapath = batch_export_csv,
    stringsAsFactors = FALSE
  ))
  stopifnot(nrow(batch_screening_results()) == nrow(monitoring_fixture))
  stopifnot(grepl("Resultats importes", batch_screening_status(), fixed = TRUE))
})

cat("test_app_server: OK\n")
