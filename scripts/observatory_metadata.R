# Helpers shared by the metadata index builder and the Shiny application.

observatory_normalize_path <- function(paths) {
  paths <- as.character(paths)
  paths[is.na(paths)] <- ""
  gsub("\\\\", "/", paths, fixed = FALSE)
}

observatory_path_parts <- function(path) {
  parts <- strsplit(observatory_normalize_path(path), "/", fixed = TRUE)[[1]]
  parts[nzchar(parts)]
}

observatory_first_nonempty <- function(primary, fallback) {
  size <- max(length(primary), length(fallback))
  if (size == 0) {
    return(character())
  }
  primary <- as.character(rep_len(primary, size))
  fallback <- as.character(rep_len(fallback, size))
  missing <- is.na(primary) | !nzchar(trimws(primary))
  primary[missing] <- fallback[missing]
  primary
}

observatory_valid_year <- function(values) {
  grepl("^20[0-9]{2}$", as.character(values))
}

observatory_capture <- function(values, pattern, group = 1L) {
  values <- as.character(values)
  values[is.na(values)] <- ""
  capture_index <- as.integer(group) + 1L
  matches <- regmatches(values, regexec(pattern, values, perl = TRUE, ignore.case = TRUE))
  vapply(matches, function(parts) {
    if (length(parts) >= capture_index && nzchar(parts[[capture_index]])) {
      parts[[capture_index]]
    } else {
      NA_character_
    }
  }, character(1))
}

observatory_year_from_path <- function(paths) {
  paths <- observatory_normalize_path(paths)
  vapply(paths, function(path) {
    parts <- observatory_path_parts(path)
    if (length(parts) < 2) {
      return(NA_character_)
    }
    directories <- parts[seq_len(length(parts) - 1L)]
    mode_index <- which(tolower(directories) %in% c("pos", "neg"))
    scope <- if (length(mode_index) > 0) {
      directories[seq_len(mode_index[[1]] - 1L)]
    } else {
      directories
    }
    candidates <- scope[observatory_valid_year(scope)]
    if (length(candidates) == 0) NA_character_ else tail(candidates, 1)
  }, character(1))
}

observatory_mode_from_path <- function(paths) {
  paths <- observatory_normalize_path(paths)
  vapply(paths, function(path) {
    parts <- tolower(observatory_path_parts(path))
    matches <- unique(parts[parts %in% c("pos", "neg")])
    if (length(matches) == 1) matches[[1]] else NA_character_
  }, character(1))
}

observatory_json_to_parquet_filename <- function(json_paths) {
  filenames <- basename(observatory_normalize_path(json_paths))
  matches <- regmatches(
    filenames,
    regexec("^(.+)-metadata(\\([0-9]+\\))?\\.json$", filenames, perl = TRUE, ignore.case = TRUE)
  )
  vapply(seq_along(filenames), function(index) {
    parts <- matches[[index]]
    if (length(parts) >= 2) {
      copy_suffix <- if (length(parts) >= 3 && !is.na(parts[[3]])) parts[[3]] else ""
      return(paste0(parts[[2]], copy_suffix, ".parquet"))
    }
    sub("\\.json$", ".parquet", filenames[[index]], ignore.case = TRUE)
  }, character(1))
}

observatory_json_to_parquet_relative_path <- function(json_paths) {
  paths <- observatory_normalize_path(json_paths)
  filenames <- observatory_json_to_parquet_filename(paths)
  directories <- dirname(paths)
  result <- ifelse(
    directories %in% c(".", ""),
    filenames,
    paste(directories, filenames, sep = "/")
  )
  result[!grepl("\\.json$", paths, ignore.case = TRUE)] <- NA_character_
  result
}

observatory_parse_name_metadata <- function(values) {
  values <- as.character(values)
  values[is.na(values)] <- ""

  year_month_year <- observatory_capture(
    values,
    "(?:^|[^0-9])(20[0-9]{2})[-_](0[1-9]|1[0-2])",
    group = 1
  )
  year_month_month <- observatory_capture(
    values,
    "(?:^|[^0-9])(20[0-9]{2})[-_](0[1-9]|1[0-2])",
    group = 2
  )
  day_month_month <- observatory_capture(
    values,
    "(?:^|[^0-9])(?:0[1-9]|[12][0-9]|3[01])[/_-]?(0[1-9]|1[0-2])[/_-]?(20[0-9]{2})(?:$|[^0-9])",
    group = 1
  )
  day_month_year <- observatory_capture(
    values,
    "(?:^|[^0-9])(?:0[1-9]|[12][0-9]|3[01])[/_-]?(0[1-9]|1[0-2])[/_-]?(20[0-9]{2})(?:$|[^0-9])",
    group = 2
  )
  compact_duplicate <- observatory_capture(
    values,
    "20[0-9]{2}[-_](?:0[1-9]|1[0-2])([A-Za-z])(?=[_-]|$)"
  )
  clichy_duplicate <- observatory_capture(
    values,
    "(?:^|[ _-])clichy[ _-]+([A-Za-z])(?=[ _-]|$)"
  )
  replicate_label <- observatory_capture(values, "_replicate_(.+)$")
  replicate_label <- sub("\\([0-9]+\\)$", "", replicate_label)
  sample_base_name <- sub("_replicate_.+$", "", values, ignore.case = TRUE)
  sample_base_name[!nzchar(sample_base_name)] <- NA_character_

  data.frame(
    reference_year = observatory_first_nonempty(year_month_year, day_month_year),
    reference_month = observatory_first_nonempty(year_month_month, day_month_month),
    duplicate_label = observatory_first_nonempty(compact_duplicate, clichy_duplicate),
    replicate_label = replicate_label,
    is_blank = grepl(
      "(^|[^[:alpha:]])(blanc|blank)([^[:alpha:]]|$)",
      values,
      ignore.case = TRUE,
      perl = TRUE
    ),
    sample_name = ifelse(nzchar(values), values, NA_character_),
    sample_base_name = sample_base_name,
    sample_group = sample_base_name,
    stringsAsFactors = FALSE
  )
}

observatory_parse_parquet_metadata <- function(relative_paths) {
  paths <- observatory_normalize_path(relative_paths)
  stems <- sub("\\.parquet$", "", basename(paths), ignore.case = TRUE)
  names <- observatory_parse_name_metadata(stems)
  year_dir <- observatory_year_from_path(paths)
  mode_dir <- observatory_mode_from_path(paths)
  reference_year <- observatory_first_nonempty(year_dir, names$reference_year)

  data.frame(
    year_dir = year_dir,
    mode_dir = mode_dir,
    reference_year = reference_year,
    reference_month = names$reference_month,
    file_type = ifelse(
      names$is_blank,
      "Blanc",
      ifelse(!is.na(reference_year) & nzchar(reference_year), "Echantillon", NA_character_)
    ),
    duplicate_label = names$duplicate_label,
    replicate_label = names$replicate_label,
    is_blank = names$is_blank,
    sample_name = names$sample_name,
    sample_base_name = names$sample_base_name,
    sample_group = names$sample_group,
    stringsAsFactors = FALSE
  )
}
