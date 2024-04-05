# TODO: Split function into part that actually write a gdx file and the part that collects the corresponding vars/fns
#' assessmentDataToGDX
#'
#' Something
#' 
#' @md 
#' @param assessmentData Data frame as produced by `climate-assessment`
#' @param exportConf Associates climate assessment variables with the GDX file names and their GAMS counterpart 
#' @param interpolateYears In case the periods in the emission report do not match the one reported by climate assessment, we need to interpolate to be able to integrate them with existing GAMS data
#' @param logFile Path to the log file. Default is NULL
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
library(gdxrrw)
require(renameVariableMagicc7ToRemind)
dumpToGDX <- function(assessmentData, exportConf, interpolateYears = NULL, logFile = NULL, verbose = NULL) {
    # Use the variable/file association to determine which variables shall be extracted from the MAGICC7 data
    thesePlease <- unique(associateVariablesAndFiles$magicc7Variable)
    if (is.null(interpolateYears)) interpolateYears <- unique(assessmentData$period)
    relevantData <- assessmentData %>%
        # Exlude all other variables
        filter(variable %in% thesePlease & period %in% interpolateYears) %>%
        # Interpolate missing periods: TODO is this actually necessary? We only check for periods in the data anyway..
        interpolate_missing_periods(interpolateYears, expand.values = FALSE) %>%
        # Transform data from long to wide format such that yearly values are given in individual columns
        pivot_wider(names_from = "period", values_from = "value") %>%
        # Rename variables to REMIND-style names
        mutate(variable = sapply(.data$variable, renameVariableMagicc7ToRemind, simplify = TRUE, USE.NAMES = FALSE))

    # Loop through each file name given in associateVariablesAndFiles and write the associated variables to GDX files
    # Note: This arrangement is capable of writing multiple variables to the same GDX file
    for (currentFn in unique(associateVariablesAndFiles$fileName)) {
        # gamsVariable <- gdxFilesAndRemindVariables[[fileName]]
        #cat(paste0(currentFn, "\n"))
        whatToWrite <- associateVariablesAndFiles %>%
            filter(.data$fileName == currentFn) %>%
            select(remindVariable, gamsVariable)
        # TODO BEGIN: Insert dumpDataFrameToGDX function HERE
        # Build a column vector of the variable values
        gdxData <- cbind(
            # First column has to be enumaration of values 1..n(variable values)
            1:length(interpolateYears),
            # Subsequent columns have to be the actual variable values
            relevantData %>%
                filter(.data$variable %in% whatToWrite$remindVariable) %>%
                select(all_of(as.character(interpolateYears))) %>%
                t()
        )
        # Drop row names (period/years), since they are provided in the GDX file as "uels"
        rownames(gdxData) <- NULL
        # Write the GDX file
        # First, create a list of lists that in turn contain the actual data to be written
        wgdx.lst(
            currentFn,
            llist <- purrr::map(2:ncol(gdxData), function(idx) {
                list(
                    name = whatToWrite$gamsVariable[idx - 1],
                    type = "parameter",
                    dim = 1,
                    val = gdxData[, c(1, idx)],
                    form = "sparse",
                    uels = list(interpolateYears),
                    domains = "tall"
                )
            })
        )
        # TODO END
        if (verbose | !is.null(logFile)) {
            logMsg <- paste0(date(), " Wrote '", currentFn, "'\n")
            if (verbose) cat(logMsg)
            if (!is.null(logFile)) capture.output(cat(logMsg), file = logFile, append = TRUE)
        }
    }
}