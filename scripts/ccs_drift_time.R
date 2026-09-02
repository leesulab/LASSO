#!/usr/bin/env Rscript

# Contract for the future CCS -> drift-time conversion.
#
# No scientific formula is implemented here. The conversion function will be
# supplied by arcMS or the analytical team and must follow:
#   converter(ccs, mz, calibration_parameters) -> dt
# where calibration_parameters is a named list containing C1 and C2.

ccs_drift_scalar <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(value[[1]]))
}

empty_ccs_to_drifttime_result <- function(status) {
  list(
    ok = FALSE,
    expected_dt = NA_real_,
    C1 = NA_real_,
    C2 = NA_real_,
    status = status
  )
}

normalize_ccs_calibration_parameters <- function(calibration_parameters) {
  if (is.null(calibration_parameters) || length(calibration_parameters) == 0) {
    return(empty_ccs_to_drifttime_result("Non evalue: coefficients C1/C2 absents"))
  }

  values <- if (is.data.frame(calibration_parameters)) {
    if (nrow(calibration_parameters) != 1) {
      return(empty_ccs_to_drifttime_result("Non evalue: calibration C1/C2 non unique"))
    }
    as.list(calibration_parameters[1, , drop = FALSE])
  } else {
    calibration_parameters
  }
  values <- unlist(values, recursive = TRUE, use.names = TRUE)
  value_names <- names(values)
  if (is.null(value_names) || length(value_names) == 0) {
    return(empty_ccs_to_drifttime_result("Non evalue: noms C1/C2 absents"))
  }

  normalized_names <- toupper(gsub("[^A-Za-z0-9]", "", value_names))
  c1_index <- match("C1", normalized_names)
  c2_index <- match("C2", normalized_names)
  if (is.na(c1_index) || is.na(c2_index)) {
    return(empty_ccs_to_drifttime_result("Non evalue: coefficients C1/C2 incomplets"))
  }

  c1 <- ccs_drift_scalar(values[[c1_index]])
  c2 <- ccs_drift_scalar(values[[c2_index]])
  if (!is.finite(c1) || !is.finite(c2)) {
    return(empty_ccs_to_drifttime_result("Non evalue: coefficients C1/C2 invalides"))
  }

  list(
    ok = TRUE,
    expected_dt = NA_real_,
    C1 = c1,
    C2 = c2,
    parameters = list(C1 = c1, C2 = c2),
    status = "Calibration C1/C2 disponible"
  )
}

ccs_drift_time_value <- function(value) {
  if (is.data.frame(value) && "dt" %in% names(value) && nrow(value) > 0) {
    return(ccs_drift_scalar(value$dt))
  }
  if (is.list(value) && !is.null(value$dt)) {
    return(ccs_drift_scalar(value$dt))
  }
  ccs_drift_scalar(value)
}

resolve_expected_drifttime_from_ccs <- function(ccs, mz, calibration_parameters, converter = NULL) {
  ccs <- ccs_drift_scalar(ccs)
  mz <- ccs_drift_scalar(mz)
  if (!is.finite(ccs) || ccs <= 0) {
    return(empty_ccs_to_drifttime_result("Non evalue: CCS attendu absent"))
  }
  if (!is.finite(mz) || mz <= 0) {
    return(empty_ccs_to_drifttime_result("Non evalue: m/z attendu invalide"))
  }

  calibration <- normalize_ccs_calibration_parameters(calibration_parameters)
  if (!isTRUE(calibration$ok)) {
    return(calibration)
  }
  if (!is.function(converter)) {
    result <- calibration
    result$ok <- FALSE
    result$status <- "Non evalue: fonction CCS -> DT absente"
    return(result)
  }

  converted <- tryCatch(
    converter(
      ccs = ccs,
      mz = mz,
      calibration_parameters = calibration$parameters
    ),
    error = function(error) error
  )
  if (inherits(converted, "error")) {
    result <- calibration
    result$ok <- FALSE
    result$status <- paste0("Non evalue: conversion CCS -> DT en erreur (", conditionMessage(converted), ")")
    return(result)
  }

  expected_dt <- ccs_drift_time_value(converted)
  if (!is.finite(expected_dt) || expected_dt <= 0) {
    result <- calibration
    result$ok <- FALSE
    result$status <- "Non evalue: conversion CCS -> DT invalide"
    return(result)
  }

  list(
    ok = TRUE,
    expected_dt = expected_dt,
    C1 = calibration$C1,
    C2 = calibration$C2,
    parameters = calibration$parameters,
    status = "DT attendu calcule depuis le CCS"
  )
}
