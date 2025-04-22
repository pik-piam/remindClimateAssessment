#' emissionImpulseSenarios
#'
#' Geenerates emission impulse scenarios from a harmonized and infilled emissions data by repeating the unperturbed
#' scenario as baseline and concatenating perturbed scenarios. Each scenario adds a specified quantity of additional
#' emissions to the spcified emission variable in the given time periods. The periods of the original data frame can be
#' extended to observe long term effects in the climate emulation
#'
#' @md
#' @param qf `quitte` data frame containing the emission data from a single emission scenario
#' @param var Name of the variable to be perturbed
#' @param impulse Amount of additional emissions. Defaults to 3.66e2, CO2 equivalent in Mt to 1 GtC
#' @param periods Periods in which to add the emissions impulse. Defaults to c(2020, 2030, ..., 2110, 2130)
#' @param endYear Extends the original observation periods. Defaults to 2200
#' @param inbetween Duration of the extended observation periods in years. Defaults to 1
#' @return `quitte` data frame containing all data for the emission impulse scenarios
#' @importFrom quitte interpolate_missing_periods as.IAMCTimeseries
#' @importFrom dplyr arrange bind_rows case_when mutate
#' @examples
#' \dontrun{
#' impulses <- read.quitte(cfg$harmInfEmissionsFile) %>%
#'   emissionImpulseSenarios() %>%
#'   write_csv(cfg$emissionsImpulseFile, quote = "none")
#' }
#' @author Tonn Rüter
#' @export
emissionImpulseSenarios <- function(
  qf,
  var = "AR6 climate diagnostics|Infilled|Emissions|CO2|Energy and Industrial Processes",
  impulse = 3.66e3, # GtC_to_MtCO2
  periods = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100, 2110, 2130),
  endYear = 2200,
  inbetween = 1
) {
  if (!(var %in% qf$variable)) {
    stop("Missing variable ", var)
  }

  startYear <- as.integer(max(qf$period)) + inbetween
  if ((endYear - startYear) < inbetween) {
    stop("No columns added (startYear = ", startYear, ", endYear = ", endYear, ", inbetween = ", inbetween, ")")
  }

  extendedPeriods <- seq.int(from = startYear, to = endYear, by = inbetween)
  if (!is.null(extendedPeriods) || is.numeric(extendedPeriods)) {
    qf <- qf %>%
      interpolate_missing_periods(extendedPeriods, expand.values = TRUE, method = "linear") %>%
      arrange(.data$variable, .data$period)
  }

  impulses <- qf %>% mutate(scenario = "BASE")
  for (year in periods) {
    impulses <- bind_rows(
      impulses,
      qf %>%
        mutate(
          value = case_when(
            .data$variable == var & .data$period == year ~ .data$value + impulse,
            .default = .data$value
          ),
          scenario = paste0("P", year)
        )
    )
  }
  return(impulses %>% as.IAMCTimeseries())
}
