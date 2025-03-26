# Write a gdx file and the part that collects the corresponding vars/fns assessmentDataToGDX
#'
#' Something
#'
#' @name assessmentDataToGDX
#' @md
#' @param assessmentData Data frame as produced by `climate-assessment`
#' @param exportConf Associates climate assessment variables with the GDX file names and their GAMS counterpart
#' @param interpolateYears In case the periods in the emission report do not match the one reported by climate
#'  assessment, we need to interpolate to be able to integrate them with existing GAMS data
#' @param logFile Path to the log file. Default is NULL
#' @param verbose Determines logging level. If set to TRUE, log messages are printed to std out
#' @importFrom dplyr filter mutate rename_with select
#' @importFrom magrittr %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom rlang .data
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_wider
#' @importFrom yaml read_yaml
#' @examples
#' \dontrun{
#' # Generates REMIND emission report, extracts relevant variables and reshapes. Note: This funtion
#' can be used in a tidyverse pipeline.
#' emissionDataForClimateAssessment(
#'   remind2::reportEmi(fulldata.gdx),
#'   scenarioName = "SSP2EU-NPi-ar6",
#'   climateAssessmentYaml = file.path(
#'     system.file(package = "piamInterfaces"), "iiasaTemplates", "climate_assessment_variables.yaml"
#'   )
#'   logFile = "output/missing.log"
#' )
#' }
#' @author Tonn Rüter
#' @export
dumpToGDX <- function(assessmentData, exportConf, interpolateYears = NULL, logFile = NULL, verbose = NULL) {
  # Use the variable/file association to determine which variables shall be extracted from the MAGICC7 data
  # Read the YAML file
  associateVariablesAndFiles <- read_yaml(system.file("inst", "default.yaml", package = "remindClimateAssessment"))
  thesePlease <- unique(associateVariablesAndFiles$magicc7Variable)
  if (is.null(interpolateYears)) interpolateYears <- unique(assessmentData$period)
  relevantData <- assessmentData %>%
    # Exlude all other variables
    filter(.data$variable %in% thesePlease & .data$period %in% interpolateYears) %>%
    # Interpolate missing periods: TODO is this actually necessary? We only check for periods in the data anyway..
    interpolate_missing_periods(interpolateYears, expand.values = FALSE) %>%
    # Transform data from long to wide format such that yearly values are given in individual columns
    pivot_wider(names_from = "period", values_from = "value") %>%
    # Rename variables to REMIND-style names
    mutate(variable = vapply(
      .data$variable, remindClimateAssessment::renameVariableMagicc7ToRemind, #nolint
      simplify = TRUE, FUN.VALUE = character(1), USE.NAMES = FALSE))

  # Loop through each file name given in associateVariablesAndFiles and write the associated variables to GDX files
  # Note: This arrangement is capable of writing multiple variables to the same GDX file
  for (currentFn in unique(associateVariablesAndFiles$fileName)) {
    whatToWrite <- associateVariablesAndFiles %>%
      filter(.data$fileName == currentFn) %>%
      select(whatToWrite$remindVariable, whatToWrite$gamsVariable)
    # Build a column vector of the variable values
    dumpToGDX(relevantData, currentFn, whatToWrite)
    if (verbose || !is.null(logFile)) {
      logMsg <- paste0(date(), " Wrote '", currentFn, "'\n")
      if (verbose) cat(logMsg)
      if (!is.null(logFile)) cat(logMsg, file = logFile, append = TRUE)
    }
  }
}
