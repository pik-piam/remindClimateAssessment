#' emissionDataForClimateAssessment
#'
#' Converts remind emission data from long to wide format suitable for climate assessment. Only considers regions "GLO"
#' and "World" and extracts only the variables needed for climate assessment. Per default these are provided from the
#' AR6 mapping in the piamInterfaces package. The resulting data frame has one column for each year and one row for
#' each variable. For more information visit https://pyam-iamc.readthedocs.io/en/stable/data.html
#'
#' @md
#' @param qf `quitte` data frame containing the emission data
#' @param scenario Name of the scenario
#' @param variablesFile Path to the yaml file containing the variables needed for climate-assessment. If no file path
#'  is provided, the function gets the yaml file from the piamInterfaces package
#' @param logFile Path to the log file. Default is "output/missing.log"
#' @return `quitte` data frame with the REMIND emission data reshaped for climate assessment
#' @importFrom quitte is.quitte
#' @importFrom dplyr filter mutate rename_with
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_title
#' @importFrom piamInterfaces generateIIASASubmission
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
emissionDataForClimateAssessment <- function(qf, scenario, variablesFile = NULL, logFile = NULL) {
  if (!is.quitte(qf)) {
    stop("remindEmissionReport must be a `quitte` object")
  }
  if (is.null(variablesFile)) {
    variablesFile <- normalizePath(file.path(
      system.file(package = "piamInterfaces"), "iiasaTemplates", "climate_assessment_variables.yaml"
    ))
  }
  return(
    qf %>%
      # Consider only the global region
      filter(.data$region %in% c("GLO", "World")) %>%
      # Extract only the variables needed for climate-assessment. These are provided from the iiasaTemplates in the
      # piamInterfaces package. See also:
      # https://github.com/pik-piam/piamInterfaces/blob/master/inst/iiasaTemplates/climate_assessment_variables.yaml
      generateIIASASubmission(
        mapping = "AR6",
        outputFilename = NULL,
        iiasatemplate = variablesFile,
        logFile = logFile
      ) %>%
      mutate(region = factor("World"), scenario = factor(scenario)) %>%
      # Rename the columns using str_to_title which capitalizes the first letter of each word
      rename_with(str_to_title) %>%
      # Transforms the yearly values for each variable from a long to a wide format. The resulting data frame then has
      # one column for each year and one row for each variable
      pivot_wider(names_from = "Period", values_from = "Value")
  )
}
