# Split function into part that actually write a gdx file and the part that collects the corresponding vars/fns
#' assessmentDataToGDX
#'
#' Something
#'
#' @md
#' @param relevantData FOO
#' @param fileName BAR
#' @param variables BAZ
#' @importFrom dplyr filter mutate rename_with all_of
#' @importFrom gdxrrw wgdx.lst
#' @importFrom magrittr %>%
#' @importFrom quitte as.quitte
#' @importFrom stringr str_to_title
#' @importFrom tidyr pivot_wider
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
dumpToGdx <- function(relevantData, fileName, variables) {
  if (!all(c("remindVariable", "gamsVariable") %in% names(variables))) {
    stop("Need to provide association between remind and gams variable names")
  }
  # Extract periods from col names
  usePeriods <- as.numeric(grep("[0-9]+", names(relevantData), value = TRUE))
  # Build a column vector of the variable values
  gdxData <- cbind(
    # First column has to be enumaration of values 1..n(variable values)
    seq_along(usePeriods),
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
    fileName,
    purrr::map(2:ncol(gdxData), function(idx) {
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
