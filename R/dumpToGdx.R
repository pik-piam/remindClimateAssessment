#' assessmentDataToGDX
# TODO: Split function into part that actually write a gdx file and the part that collects the corresponding vars/fns
#'
#' Something
#' 
#' @md 
#' @param assessmentData Data frame as produced by `climate-assessment`
#' @param exportConf Associates climate assessment variables with the GDX file names and their GAMS counterpart 
#' @param interpolateYears In case the periods in the emission report do not match those reported by climate assessment, we need to interpolate to be able to integrate them with existing GAMS data. Default is NULL
#' @param verbose Prints log messages to standard out if set to TRUE or to the specified file if a string is provided. Default is NULL
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
dumpToGdx <- function(relevantData, fileName, variables) {
    if (!all(c("remindVariable", "gamsVariable") %in% names(variables))) {
        stop("Need to provide association between remind and gams variable names")
    }
    # Extract periods from col names
    usePeriods <- as.numeric(grep("[0-9]+", names(climateAssessmentInputData), value = TRUE))
    # Build a column vector of the variable values
    gdxData <- cbind(
        # First column has to be enumaration of values 1..n(variable values)
        1:length(usePeriods),
        # Subsequent columns have to be the actual variable values
        relevantData %>%
            filter(.data$variable %in% variables$remindVariable) %>%
            select(all_of(as.character(usePeriods))) %>%
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
                name = variables$gamsVariable[idx - 1],
                type = "parameter",
                dim = 1,
                val = gdxData[, c(1, idx)],
                form = "sparse",
                uels = list(usePeriods),
                domains = "tall"
            )
        })
    )
}