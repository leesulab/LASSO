#!/usr/bin/env Rscript

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
data_root <- tempfile("observatoire-metadata-")
output_file <- tempfile("metadata-index-", fileext = ".csv")
on.exit(unlink(c(data_root, output_file), recursive = TRUE, force = TRUE), add = TRUE)

write_metadata_json <- function(path, sample_name, mode = "+") {
  payload <- list(
    sampleinfos = list(
      sampleName = sample_name,
      id = paste0("id-", sample_name),
      name = sample_name,
      analysisName = "test-analysis",
      sample = list(
        groupId = sample_name,
        sampleType = "Unknown",
        replicateNumber = 1,
        wellPosition = "A1",
        injectionVolume = 1,
        acquisitionRunTime = 1,
        acquisitionStartTime = "2021-01-19T00:00:00Z"
      )
    ),
    spectruminfos = list(list(
      analyticalTechnique = list(
        ionisationMode = mode,
        tofGroup = list(mseLevel = "Low"),
        lowMass = 50,
        highMass = 1000
      ),
      isIonMobilityData = TRUE,
      hasCCSCalibration = FALSE
    ))
  )
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE), path)
}

dir.create(file.path(data_root, "2021", "pos"), recursive = TRUE)
dir.create(file.path(data_root, "2021", "neg"), recursive = TRUE)
dir.create(file.path(data_root, "2023", "neg"), recursive = TRUE)

file.create(file.path(data_root, "2021", "pos", "Obs_01_Clichy C_19012021_replicate_1(1).parquet"))
write_metadata_json(
  file.path(data_root, "2021", "pos", "Obs_01_Clichy C_19012021_replicate_1-metadata.json"),
  "Obs_01_Clichy C_19/01/2021_replicate_1"
)
file.create(file.path(data_root, "2021", "neg", "Obs_01_Clichy D_19012021_replicate_1.parquet"))
file.create(file.path(data_root, "2023", "neg", "2024-01C_replicate_1.parquet"))
write_metadata_json(
  file.path(data_root, "2023", "neg", "2024-01C_replicate_1-metadata.json"),
  "2024-01C_replicate_1",
  mode = "-"
)

build_output <- system2(
  "Rscript",
  c("--vanilla", file.path(project_root, "scripts", "build_metadata_index.R"), data_root, output_file),
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

index <- utils::read.csv(output_file, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(nrow(index) == 3L)
stopifnot(!anyDuplicated(index$parquet_relative_path))

legacy_positive <- index[
  index$parquet_relative_path == "2021/pos/Obs_01_Clichy C_19012021_replicate_1(1).parquet",
  ,
  drop = FALSE
]
stopifnot(nrow(legacy_positive) == 1L)
stopifnot(isTRUE(legacy_positive$json_available[[1]]))
stopifnot(identical(as.character(legacy_positive$reference_year[[1]]), "2021"))
stopifnot(identical(as.character(legacy_positive$reference_month[[1]]), "1"))
stopifnot(identical(legacy_positive$duplicate_label[[1]], "C"))

legacy_negative <- index[
  index$parquet_relative_path == "2021/neg/Obs_01_Clichy D_19012021_replicate_1.parquet",
  ,
  drop = FALSE
]
stopifnot(nrow(legacy_negative) == 1L)
stopifnot(!isTRUE(legacy_negative$json_available[[1]]))
stopifnot(identical(as.character(legacy_negative$reference_year[[1]]), "2021"))
stopifnot(identical(as.character(legacy_negative$reference_month[[1]]), "1"))
stopifnot(identical(legacy_negative$duplicate_label[[1]], "D"))

path_year <- index[index$parquet_relative_path == "2023/neg/2024-01C_replicate_1.parquet", , drop = FALSE]
stopifnot(nrow(path_year) == 1L)
stopifnot(isTRUE(path_year$json_available[[1]]))
stopifnot(identical(as.character(path_year$reference_year[[1]]), "2023"))
stopifnot(identical(as.character(path_year$reference_month[[1]]), "1"))
stopifnot(identical(path_year$duplicate_label[[1]], "C"))

cat("test_metadata_index: OK\n")
