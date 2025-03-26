#' Report climate assessment configuration
#'
#' Returns a string represenatation of a climate assessment configuration
#'
#' @param cfg Path to default.cfg file
#' @return A string detailing the configuration settings.
#'
#' @examples
#' \dontrun{
#' cfg <- climateAssessmentConfig("<outputDir>", "iteration")
#' report <- reportClimateAssessmentConfig(cfg)
#' cat(report)
#' }
#'
#' @export
reportClimateAssessmentConfig <- function(cfg) {
  return(
    paste0(
      paste0("climate assessment config of '", cfg$outputDir, "'\n"),
      "  scenario   = ", cfg$scenario, "\n",
      "  condaEnv   = ", cfg$condaEnv, "\n",
      "  isArchived = ", cfg$isArchived, "\n",
      "  logFile    = ", cfg$logFile, "\n",
      "  climateDir = ", cfg$climateDir, "\n",
      "  workersDir = ", cfg$workersDir, "\n",
      "  archiveDir = ", cfg$archiveDir, "\n",
      "  scriptsDir = ", cfg$scriptsDir, "\n",
      "  magiccBin  = ", cfg$magiccBin, "\n",
      "  variablesFile = ", cfg$variablesFile, "\n",
      "  infillingDatabase = ", cfg$infillingDatabase, "\n",
      "  probabilisticFile = ", cfg$probabilisticFile, "\n",
      "  containing nSets  = ", cfg$nSets, "\n",
      "  remindEmissionsFile   = ", cfg$remindEmissionsFile, "\n",
      "  harmInfEmissionsFile  = ", cfg$harmInfEmissionsFile, "\n",
      "  climateAssessmentFile = ", cfg$climateAssessmentFile, "\n"
    )
  )
}
