#' Clean Up Climate Assessment Data
#'
#' This function cleans up climate assessment data by filtering for specific periods,
#' interpolating missing periods, and renaming variables from MAGICC7 to REMIND naming convention
#'
#' @param qf A `quitte` object containing the climate assessment data to be cleaned
#' @param periods Vector periods (years) to be considered
#' @return A cleaned `quitte` object with filtered periods, interpolated missing periods, and renamed variables
#' @importFrom dplyr filter mutate
#' @importFrom quitte interpolate_missing_periods
cleanUpClimateAssessment <- function(qf, periods) {
  return(
    qf %>%
      filter(.data$period %in% periods) %>%
      interpolate_missing_periods(periods, expand.values = FALSE) %>%
      mutate(variable = renameVariableMagicc7ToRemind(.data$variable))
  )
}
