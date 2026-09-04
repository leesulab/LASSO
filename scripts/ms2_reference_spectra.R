# Helpers for importing and comparing MS2 reference spectra.
#
# This module intentionally reports technical fragment agreement only. It does
# not assign a published identification-confidence level: that decision needs a
# validated analytical protocol, a suitable reference library, and scientific
# review.

empty_ms2_reference_spectra <- function() {
  data.frame(
    reference_id = character(),
    compound_name = character(),
    mode = character(),
    precursor_mz = numeric(),
    collision_energy = character(),
    fragment_mz = numeric(),
    relative_intensity = numeric(),
    source = character(),
    library_accession = character(),
    stringsAsFactors = FALSE
  )
}

empty_ms2_reference_matches <- function() {
  data.frame(
    reference_id = character(),
    reference_fragment_mz = numeric(),
    reference_relative_intensity = numeric(),
    observed_fragment_mz = numeric(),
    observed_intensity = numeric(),
    observed_relative_intensity = numeric(),
    mz_error_da = numeric(),
    mz_error_ppm = numeric(),
    matched = logical(),
    stringsAsFactors = FALSE
  )
}

empty_ms2_comparison_summary <- function(status = "Non evalue") {
  data.frame(
    reference_id = NA_character_,
    compound_name = NA_character_,
    mode = NA_character_,
    precursor_mz = NA_real_,
    collision_energy = NA_character_,
    source = NA_character_,
    library_accession = NA_character_,
    reference_fragments = 0L,
    matched_fragments = 0L,
    fragment_coverage_pct = NA_real_,
    weighted_coverage_pct = NA_real_,
    cosine_similarity = NA_real_,
    observed_signal_fraction_pct = NA_real_,
    mz_tolerance = NA_real_,
    min_matched_fragments = NA_integer_,
    min_cosine_similarity = NA_real_,
    technical_status = status,
    stringsAsFactors = FALSE
  )
}

ms2_reference_template <- function() {
  data.frame(
    reference_id = rep("example_standard_pos", 3),
    compound_name = rep("example-standard", 3),
    mode = rep("pos", 3),
    precursor_mz = rep(200.1234, 3),
    collision_energy = rep("20 eV", 3),
    fragment_mz = c(50.0, 75.0, 120.0),
    relative_intensity = c(100, 55, 20),
    source = rep("Example only - replace with validated reference data", 3),
    library_accession = rep("", 3),
    stringsAsFactors = FALSE
  )
}

canonical_ms2_reference_column <- function(name) {
  key <- tolower(trimws(as.character(name[[1]])))
  key <- gsub("[^[:alnum:]]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  aliases <- c(
    "reference_id" = "reference_id",
    "spectrum_id" = "reference_id",
    "spectrumid" = "reference_id",
    "compound_name" = "compound_name",
    "compound" = "compound_name",
    "name" = "compound_name",
    "molecule" = "compound_name",
    "mode" = "mode",
    "ion_mode" = "mode",
    "ionisation_mode" = "mode",
    "precursor_mz" = "precursor_mz",
    "parent_mz" = "precursor_mz",
    "precursor" = "precursor_mz",
    "collision_energy" = "collision_energy",
    "collisionenergy" = "collision_energy",
    "ce" = "collision_energy",
    "fragment_mz" = "fragment_mz",
    "product_mz" = "fragment_mz",
    "product_ion_mz" = "fragment_mz",
    "fragment" = "fragment_mz",
    "ms2_mz" = "fragment_mz",
    "relative_intensity" = "relative_intensity",
    "relativeintensity" = "relative_intensity",
    "rel_intensity" = "relative_intensity",
    "intensity_relative" = "relative_intensity",
    "intensity" = "relative_intensity",
    "source" = "source",
    "library" = "source",
    "library_name" = "source",
    "library_accession" = "library_accession",
    "accession" = "library_accession"
  )
  if (key %in% names(aliases)) unname(aliases[[key]]) else key
}

normalize_ms2_reference_columns <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- vapply(names(x), canonical_ms2_reference_column, character(1))
  x[, !duplicated(names(x)), drop = FALSE]
}

ms2_reference_numeric <- function(values) {
  values <- trimws(as.character(values))
  values[!nzchar(values)] <- NA_character_
  values <- gsub(",", ".", values, fixed = TRUE)
  suppressWarnings(as.numeric(values))
}

normalize_ms2_reference_mode <- function(values) {
  values <- tolower(trimws(as.character(values)))
  values[is.na(values)] <- ""
  values[values %in% c("+", "positive", "positif")] <- "pos"
  values[values %in% c("-", "negative", "negatif")] <- "neg"
  values
}

ms2_reference_character_column <- function(x, name, default = "") {
  if (!name %in% names(x)) {
    return(rep(default, nrow(x)))
  }
  values <- trimws(as.character(x[[name]]))
  values[is.na(values)] <- default
  values
}

ms2_reference_generated_id <- function(compound_name, mode, collision_energy, source) {
  clean <- function(value, fallback) {
    value <- tolower(trimws(as.character(value)))
    value <- gsub("[^[:alnum:]]+", "_", value)
    value <- gsub("^_+|_+$", "", value)
    ifelse(nzchar(value), value, fallback)
  }
  paste(
    "ref",
    clean(compound_name, "compound"),
    clean(mode, "mode"),
    clean(collision_energy, "ce"),
    clean(source, "source"),
    sep = "__"
  )
}

normalize_ms2_reference_spectra <- function(x, default_source = "CSV importe") {
  x <- normalize_ms2_reference_columns(x)
  if (nrow(x) == 0) {
    return(empty_ms2_reference_spectra())
  }

  required <- c("compound_name", "fragment_mz", "relative_intensity")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Le CSV de spectres MS2 doit contenir : ", paste(required, collapse = ", "), ". Colonne(s) absente(s) : ", paste(missing, collapse = ", "), ".")
  }

  compound_name <- ms2_reference_character_column(x, "compound_name", NA_character_)
  invalid_name <- is.na(compound_name) | !nzchar(compound_name)
  if (any(invalid_name)) {
    stop("Chaque ligne de spectre MS2 doit contenir compound_name.")
  }

  mode <- normalize_ms2_reference_mode(ms2_reference_character_column(x, "mode", ""))
  invalid_mode <- nzchar(mode) & !mode %in% c("pos", "neg")
  if (any(invalid_mode)) {
    stop("La colonne mode doit etre vide, pos ou neg.")
  }

  fragment_mz <- ms2_reference_numeric(x$fragment_mz)
  if (any(!is.finite(fragment_mz) | fragment_mz <= 0)) {
    stop("Chaque fragment_mz doit etre un nombre strictement positif.")
  }

  relative_intensity <- ms2_reference_numeric(x$relative_intensity)
  if (any(!is.finite(relative_intensity) | relative_intensity <= 0)) {
    stop("Chaque relative_intensity doit etre un nombre strictement positif.")
  }

  precursor_text <- ms2_reference_character_column(x, "precursor_mz", "")
  precursor_mz <- ms2_reference_numeric(precursor_text)
  invalid_precursor <- nzchar(precursor_text) & (!is.finite(precursor_mz) | precursor_mz <= 0)
  if (any(invalid_precursor)) {
    stop("precursor_mz doit etre vide ou un nombre strictement positif.")
  }

  collision_energy <- ms2_reference_character_column(x, "collision_energy", "")
  source <- ms2_reference_character_column(x, "source", default_source)
  source[!nzchar(source)] <- default_source
  library_accession <- ms2_reference_character_column(x, "library_accession", "")
  reference_id <- ms2_reference_character_column(x, "reference_id", "")
  missing_id <- !nzchar(reference_id)
  reference_id[missing_id] <- ms2_reference_generated_id(
    compound_name[missing_id],
    mode[missing_id],
    collision_energy[missing_id],
    source[missing_id]
  )

  result <- data.frame(
    reference_id = reference_id,
    compound_name = compound_name,
    mode = mode,
    precursor_mz = precursor_mz,
    collision_energy = collision_energy,
    fragment_mz = fragment_mz,
    relative_intensity = relative_intensity,
    source = source,
    library_accession = library_accession,
    stringsAsFactors = FALSE
  )

  for (id in unique(result$reference_id)) {
    rows <- result$reference_id == id
    names_for_id <- unique(result$compound_name[rows])
    modes_for_id <- unique(result$mode[rows & nzchar(result$mode)])
    if (length(names_for_id) != 1 || length(modes_for_id) > 1) {
      stop("Chaque reference_id doit decrire une seule molecule et un seul mode.")
    }
    result$relative_intensity[rows] <- result$relative_intensity[rows] /
      max(result$relative_intensity[rows]) * 100
  }

  result <- result[order(result$reference_id, result$fragment_mz), , drop = FALSE]
  rownames(result) <- NULL
  result
}

read_ms2_reference_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop("Le fichier CSV de spectres MS2 est introuvable.")
  }
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(first_line) == 0 || !nzchar(trimws(first_line[[1]]))) {
    stop("Le fichier CSV de spectres MS2 est vide.")
  }
  separator <- if (
    lengths(regmatches(first_line[[1]], gregexpr(";", first_line[[1]], fixed = TRUE))) >
      lengths(regmatches(first_line[[1]], gregexpr(",", first_line[[1]], fixed = TRUE)))
  ) {
    ";"
  } else {
    ","
  }
  imported <- tryCatch(
    if (identical(separator, ";")) {
      utils::read.csv2(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    },
    error = function(e) e
  )
  if (inherits(imported, "error")) {
    stop("Impossible de lire le CSV de spectres MS2 : ", conditionMessage(imported))
  }
  normalize_ms2_reference_spectra(imported)
}

load_optional_ms2_reference_spectra <- function(path) {
  if (!file.exists(path)) {
    return(empty_ms2_reference_spectra())
  }
  tryCatch(
    read_ms2_reference_csv(path),
    error = function(e) {
      warning("Spectres MS2 locaux ignores : ", conditionMessage(e), call. = FALSE)
      empty_ms2_reference_spectra()
    }
  )
}

ms2_reference_summary <- function(x) {
  x <- normalize_ms2_reference_spectra(x)
  if (nrow(x) == 0) {
    return(data.frame(
      reference_id = character(), compound_name = character(), mode = character(),
      precursor_mz = numeric(), collision_energy = character(), source = character(),
      library_accession = character(), fragments = integer(), stringsAsFactors = FALSE
    ))
  }
  ids <- unique(x$reference_id)
  rows <- lapply(ids, function(id) {
    spectrum <- x[x$reference_id == id, , drop = FALSE]
    data.frame(
      reference_id = id,
      compound_name = spectrum$compound_name[[1]],
      mode = spectrum$mode[[1]],
      precursor_mz = spectrum$precursor_mz[[1]],
      collision_energy = spectrum$collision_energy[[1]],
      source = spectrum$source[[1]],
      library_accession = spectrum$library_accession[[1]],
      fragments = nrow(spectrum),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_ms2_reference_choices <- function(x) {
  summary <- ms2_reference_summary(x)
  if (nrow(summary) == 0) {
    return(c("Aucun spectre de reference importe" = ""))
  }
  mode <- ifelse(nzchar(summary$mode), toupper(summary$mode), "mode non renseigne")
  precursor <- ifelse(
    is.finite(summary$precursor_mz),
    paste0(" | precurseur ", format(round(summary$precursor_mz, 5), trim = TRUE)),
    ""
  )
  energy <- ifelse(nzchar(summary$collision_energy), paste0(" | ", summary$collision_energy), "")
  labels <- paste0(
    summary$compound_name, " | ", mode, precursor, energy,
    " | ", summary$fragments, " fragments | ", summary$source
  )
  labels <- make.unique(labels, sep = " #")
  c("Aucun spectre de reference" = "", stats::setNames(summary$reference_id, labels))
}

ms2_reference_spectrum <- function(x, reference_id) {
  x <- normalize_ms2_reference_spectra(x)
  reference_id <- trimws(as.character(reference_id[[1]]))
  if (!nzchar(reference_id) || nrow(x) == 0) {
    return(x[0, , drop = FALSE])
  }
  x[x$reference_id == reference_id, , drop = FALSE]
}

ms2_reference_ids_for_compound <- function(x, compound) {
  x <- ms2_reference_summary(x)
  if (nrow(x) == 0 || is.null(compound) || nrow(compound) != 1 || !"name" %in% names(compound)) {
    return(character())
  }
  name <- tolower(trimws(as.character(compound$name[[1]])))
  if (!nzchar(name)) {
    return(character())
  }
  matches <- tolower(trimws(as.character(x$compound_name))) == name
  if ("mode" %in% names(compound)) {
    mode <- normalize_ms2_reference_mode(compound$mode[[1]])
    if (mode %in% c("pos", "neg")) {
      matches <- matches & (!nzchar(x$mode) | x$mode == mode)
    }
  }
  as.character(x$reference_id[matches])
}

validate_ms2_comparison_parameters <- function(mz_tolerance, min_matched_fragments, min_cosine_similarity) {
  mz_tolerance <- suppressWarnings(as.numeric(mz_tolerance[[1]]))
  min_matched_fragments <- suppressWarnings(as.integer(min_matched_fragments[[1]]))
  min_cosine_similarity <- suppressWarnings(as.numeric(min_cosine_similarity[[1]]))
  if (!is.finite(mz_tolerance) || mz_tolerance <= 0) {
    stop("La tolerance m/z des fragments doit etre strictement positive.")
  }
  if (is.na(min_matched_fragments) || min_matched_fragments < 1) {
    stop("Le nombre minimal de fragments doit etre un entier positif.")
  }
  if (!is.finite(min_cosine_similarity) || min_cosine_similarity < 0 || min_cosine_similarity > 1) {
    stop("Le score cosinus minimal doit etre compris entre 0 et 1.")
  }
  list(
    mz_tolerance = mz_tolerance,
    min_matched_fragments = min_matched_fragments,
    min_cosine_similarity = min_cosine_similarity
  )
}

compare_ms2_spectrum_to_reference <- function(observed, reference, mz_tolerance = 0.01,
                                              min_matched_fragments = 3,
                                              min_cosine_similarity = 0.7) {
  parameters <- validate_ms2_comparison_parameters(
    mz_tolerance,
    min_matched_fragments,
    min_cosine_similarity
  )
  reference <- normalize_ms2_reference_spectra(reference)
  if (nrow(reference) == 0) {
    return(list(
      summary = empty_ms2_comparison_summary("Non evalue : aucun spectre de reference selectionne"),
      matches = empty_ms2_reference_matches()
    ))
  }
  if (length(unique(reference$reference_id)) != 1) {
    stop("La comparaison MS2 doit utiliser un seul spectre de reference a la fois.")
  }

  observed <- as.data.frame(observed, stringsAsFactors = FALSE, check.names = FALSE)
  required_observed <- c("mz", "intensity")
  missing_observed <- setdiff(required_observed, names(observed))
  if (length(missing_observed) > 0) {
    stop("Le spectre MS2 observe doit contenir : ", paste(required_observed, collapse = ", "), ".")
  }
  observed$mz <- suppressWarnings(as.numeric(observed$mz))
  observed$intensity <- suppressWarnings(as.numeric(observed$intensity))
  observed <- observed[
    is.finite(observed$mz) & is.finite(observed$intensity) & observed$intensity > 0,
    c("mz", "intensity"),
    drop = FALSE
  ]

  summary_row <- ms2_reference_summary(reference)[1, , drop = FALSE]
  summary <- empty_ms2_comparison_summary()
  summary$reference_id <- summary_row$reference_id
  summary$compound_name <- summary_row$compound_name
  summary$mode <- summary_row$mode
  summary$precursor_mz <- summary_row$precursor_mz
  summary$collision_energy <- summary_row$collision_energy
  summary$source <- summary_row$source
  summary$library_accession <- summary_row$library_accession
  summary$reference_fragments <- nrow(reference)
  summary$mz_tolerance <- parameters$mz_tolerance
  summary$min_matched_fragments <- parameters$min_matched_fragments
  summary$min_cosine_similarity <- parameters$min_cosine_similarity

  if (nrow(observed) == 0) {
    summary$technical_status <- "Non evalue : spectre MS2 observe vide"
    return(list(summary = summary, matches = empty_ms2_reference_matches()))
  }

  observed$relative_intensity <- observed$intensity / max(observed$intensity) * 100
  assigned_observed <- rep(FALSE, nrow(observed))
  observed_index <- rep(NA_integer_, nrow(reference))
  order_reference <- order(-reference$relative_intensity, reference$fragment_mz)

  for (reference_index in order_reference) {
    candidates <- which(
      !assigned_observed &
        abs(observed$mz - reference$fragment_mz[[reference_index]]) <= parameters$mz_tolerance
    )
    if (length(candidates) == 0) {
      next
    }
    winner_order <- order(
      -observed$intensity[candidates],
      abs(observed$mz[candidates] - reference$fragment_mz[[reference_index]])
    )
    winner <- candidates[[winner_order[[1]]]]
    observed_index[[reference_index]] <- winner
    assigned_observed[[winner]] <- TRUE
  }

  matched <- !is.na(observed_index)
  observed_mz <- rep(NA_real_, nrow(reference))
  observed_intensity <- rep(NA_real_, nrow(reference))
  observed_relative_intensity <- rep(NA_real_, nrow(reference))
  observed_mz[matched] <- observed$mz[observed_index[matched]]
  observed_intensity[matched] <- observed$intensity[observed_index[matched]]
  observed_relative_intensity[matched] <- observed$relative_intensity[observed_index[matched]]
  mz_error_da <- observed_mz - reference$fragment_mz
  mz_error_ppm <- mz_error_da / reference$fragment_mz * 1e6

  matches <- data.frame(
    reference_id = reference$reference_id,
    reference_fragment_mz = reference$fragment_mz,
    reference_relative_intensity = reference$relative_intensity,
    observed_fragment_mz = observed_mz,
    observed_intensity = observed_intensity,
    observed_relative_intensity = observed_relative_intensity,
    mz_error_da = mz_error_da,
    mz_error_ppm = mz_error_ppm,
    matched = matched,
    stringsAsFactors = FALSE
  )

  reference_vector <- reference$relative_intensity
  observed_vector <- ifelse(matched, observed_relative_intensity, 0)
  cosine_denominator <- sqrt(sum(reference_vector ^ 2)) * sqrt(sum(observed_vector ^ 2))
  cosine_similarity <- if (is.finite(cosine_denominator) && cosine_denominator > 0) {
    sum(reference_vector * observed_vector) / cosine_denominator
  } else {
    0
  }

  summary$matched_fragments <- sum(matched)
  summary$fragment_coverage_pct <- mean(matched) * 100
  summary$weighted_coverage_pct <- sum(reference$relative_intensity[matched]) /
    sum(reference$relative_intensity) * 100
  summary$cosine_similarity <- cosine_similarity
  summary$observed_signal_fraction_pct <- sum(observed_intensity[matched], na.rm = TRUE) /
    sum(observed$intensity) * 100
  compatible <- summary$matched_fragments >= parameters$min_matched_fragments &&
    summary$cosine_similarity >= parameters$min_cosine_similarity
  summary$technical_status <- if (summary$matched_fragments == 0) {
    "Aucun fragment de reference concordant"
  } else if (compatible) {
    "Compatible selon seuils exploratoires"
  } else {
    "Preuve MS2 insuffisante selon seuils exploratoires"
  }

  list(summary = summary, matches = matches)
}
