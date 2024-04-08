#' emissionDataForClimateAssessment
#'
#' Converts remind emission data from long to wide format suitable for climate assessment. Only considers regions "GLO" and "World" and extracts only the variables needed for climate assessment. Per default these are provided from the AR6 mapping in the piamInterfaces package. The resulting data frame has one column for each year and one row for each variable.
#' 
#' @md 
#' @param remindEmissionReport Data frame containing REMIND emission data
#' @param scenarioName Name of the scenario
#' @param climateAssessmentYaml Path to the yaml file containing the variables needed for climate-assessment. If no file path is given, the function gets the yaml file from the piamInterfaces package
#' @param logFile Path to the log file. Default is "output/missing.log"
#' @return Data frame with the REMIND emission data reshaped for climate assessment
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter mutate rename_with
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_to_title
#' @examples
#' \dontrun{
#' # Generates REMIND emission report, extracts relevant variables and reshapes. Note: This funtion
#' can be used in a tidyverse pipeline.
#' emissionDataForClimateAssessment(
#'     remind2::reportEmi(fulldata.gdx),
#'     scenarioName = "SSP2EU-NPi-ar6",
#'     climateAssessmentYaml = file.path(
#'          system.file(package = "piamInterfaces"), "iiasaTemplates", "climate_assessment_variables.yaml"
#'      )
#'     logFile = "output/missing.log"
#' )
#' }
#' @author Tonn Rüter
#' @export
require(tidyverse)
require(quitte)
require(readr)
require(dplyr)
require(stringr)
require(piamInterfaces)
emissionDataForClimateAssessment <- function(
    remindEmissionReport,
    scenarioName,
    climateAssessmentYaml = NULL,
    logFile = "output/missing.log"
) {
    if (is.null(climateAssessmentYaml)) {
        climateAssessmentYaml <- file.path(
            system.file(package = "piamInterfaces"), "iiasaTemplates", "climate_assessment_variables.yaml"
        )
    }
    return(
        remindEmissionReport %>%
            as.quitte() %>%
            # Consider only the global region
            filter(region %in% c("GLO", "World")) %>%
            # Extract only the variables needed for climate-assessment. These are provided from the iiasaTemplates in the
            # piamInterfaces package. See also:
            # https://github.com/pik-piam/piamInterfaces/blob/master/inst/iiasaTemplates/climate_assessment_variables.yaml
            generateIIASASubmission(
                mapping = "AR6",
                outputFilename = NULL,
                iiasatemplate = climateAssessmentYaml,
                logFile = logFile
            ) %>%
            mutate(region = factor("World"), scenario = factor(scenarioName)) %>%
            # Rename the columns using str_to_title which capitalizes the first letter of each word
            rename_with(str_to_title) %>%
            # Transforms the yearly values for each variable from a long to a wide format. The resulting data frame then has
            # one column for each year and one row for each variable
            pivot_wider(names_from = "Period", values_from = "Value")
    )
}
