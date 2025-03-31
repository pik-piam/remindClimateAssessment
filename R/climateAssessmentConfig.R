#' Configure Climate Assessment
#'
#' Collects all necessary files and directories to set up climate assessment run from a REMIND output directory
#'
#' @param outputDir A REMIND output directory
#' @param mode Determines which probabiliy files is used. Must be either "report", "iteration", or a valid path
#'
#' @return A list containing the configuration settings for the climate assessment
# nolint start
#' @examples
#' \dontrun{
#' climateAssessmentConfig("<outputDir>", "iteration") # yields
#' list(
#'   outputDir  = "/p/tmp/tonnru/remind/output/h_cpol_KLW_d50_2025-02-06_18.10.48",
#'   scenario   = "h_cpol_KLW_d50",
#'   condaEnv   = "/p/projects/rd3mod/python/environments/scm_magicc7_hpc",
#'   isArchived = FALSE,
#'   logFile    = "<outputDir>/log_climate.txt",
#'   workersDir = "<outputDir>/climate-assessment-data/workers",
#'   climateDir = "<outputDir>/climate-assessment-data",
#'   archiveDir = "",
#'   scriptsDir = "<Depending on your installation>",
#'   magiccBin  = "<Depending on your installation>",
#'   variablesFile = "<Depending on piamInterfaces>",
#'   infillingDatabase = "<Depending on your REMIND default.cfg>",
#'   probabilisticFile = "<Depending on your REMIND default.cfg>",
#'   nSets             = "<Depending on your REMIND probabilisticFile>",
#'   ...
#' )
#' }
# nolint end
#' @export
climateAssessmentConfig <- function(outputDir, mode) {
  if (!(mode %in% c("report", "iteration")) || file.exists(mode))
    stop("'mode' must be either 'report', 'iteration' or valid path")
  runConfig <- read_yaml(file.path(outputDir, "cfg.txt"))
  cfg <- list(
    outputDir  = normalizePath(outputDir, mustWork = TRUE),
    scenario   = lucode2::getScenNames(outputDir),
    condaEnv   = normalizePath(runConfig$pythonPath, mustWork = TRUE),
    isArchived = isTRUE(runConfig$archiveClimateAssessmentData),
    logFile    = normalizePath(file.path(outputDir, "log_climate.txt"), mustWork = FALSE),
    climateDir = normalizePath(file.path(outputDir, "climate-assessment-data"), mustWork = FALSE),
    workersDir = normalizePath(file.path(outputDir, "climate-assessment-data", "workers"), mustWork = FALSE),
    archiveDir = if (!isTRUE(runConfig$archiveClimateAssessmentData)) {
      normalizePath(file.path(outputDir, "climate-assessment-data", "archive"), mustWork = FALSE)
    } else {
      normalizePath(file.path(runConfig$archiveClimateAssessmentData, mustWork = FALSE))
    },
    scriptsDir = normalizePath(file.path(runConfig$climate_assessment_root, "scripts"), mustWork = TRUE),
    magiccBin  = normalizePath(file.path(runConfig$climate_assessment_magicc_bin), mustWork = TRUE),
    variablesFile = normalizePath(
      file.path(system.file(package = "piamInterfaces"), "iiasaTemplates", "climate_assessment_variables.yaml"),
      mustWork = TRUE
    ),
    infillingDatabase = normalizePath(runConfig$climate_assessment_infiller_db, mustWork = TRUE),
    probabilisticFile = if (mode == "report") {
      normalizePath(runConfig$climate_assessment_magicc_prob_file_reporting, mustWork = TRUE)
    } else if (mode == "iteration") {
      normalizePath(runConfig$climate_assessment_magicc_prob_file_iteration, mustWork = TRUE)
    } else {
      normalizePath(mode, mustWork = TRUE)
    }
  )
  # Some more file needed to run climate assessment
  cfg$parameterSets <- read_yaml(cfg$probabilisticFile)
  cfg$nSets         <- length(cfg$parameterSets$configurations)
  # Emissions file is the output of the harmnonization and infilling step
  cfg$remindEmissionsFile  <- normalizePath(
    file.path(cfg$climateDir, paste0("ar6_climate_assessment_", cfg$scenario, ".csv")),
    mustWork = FALSE
  )
  cfg$harmInfEmissionsFile <- normalizePath(
    file.path(cfg$climateDir, paste0("ar6_climate_assessment_", cfg$scenario, "_harmonized_infilled.csv")),
    mustWork = FALSE
  )
  cfg$climateAssessmentFile <- normalizePath(
    file.path(cfg$climateDir, paste0("ar6_climate_assessment_", cfg$scenario, "_harmonized_infilled_IAMC_climateassessment.xlsx")), # nolint
    mustWork = FALSE
  )
  return(cfg)
}
