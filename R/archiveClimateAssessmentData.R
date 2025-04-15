#' Archive Climate Assessment Data
#'
#' Archives climate assessment data by copying all *.csv/*.xlsx from climate diretory (and, if specified, extra files)
#' to a temporary directory, then tar'ing and gzip'ing the temporary directory & deleting it afterwards
#'
#' @param climateDir Base directory containing climate assessment data
#' @param climateArchiveDir Directory where the climate assessment data will be archived. Default is NULL, in which case
#'  a new 'archive' subdir is created in the climate directory. Note: This directory is NOT deleted after archiving,
#'  only the temporary directory created within it is
#' @param suffix String appended to the file name. Can be used to identify the climate assessment run mode (i.e.
#'  'report', 'iteration', 'impulse') in the archive file name. Defaults to empty string
#' @param extraFiles Character vector of additional file paths to include in the archive. Default is an empty vector
#' @param returnFn A logical value indicating whether to return the path to the tar file. Defaults to TRUE
#' @return If returnFn is TRUE, the function returns the path to the tar file. Otherwise, it returns invisible()
#' @importFrom utils tar
#' @importFrom withr with_dir
#' @examples \dontrun{
#' cfg <- climateAssessmentConfig("<outputDir>", "iteration")
#' extraFiles <- c("pm_globalMeanTemperature", "p15_forc_magicc")
#' archiveClimateAssessmentData(cfg$climateArchiveDir, extraFiles)
#' }
#' @author Tonn Rüter
#' @export
archiveClimateAssessmentData <- function(
  climateDir, climateArchiveDir = NULL, suffix = "", extraFiles = c(), returnFn = TRUE
) {
  if (!dir.exists(climateDir)) {
    stop("Climate directory ", climateDir, " does not exist.")
  }
  # Create a temporary directory to store the files to be archived. If climateArchiveDir is not specified, create a new
  # 'archive' subdir in the climate directory
  if (is.null(climateArchiveDir)) {
    tmpDir <- file.path(climateDir, "archive", paste0("run_", format(Sys.time(), "%y%m%d_%H%M%S"), suffix))
  } else if (!dir.exists(climateArchiveDir)) {
    stop("Climate archive directory ", climateArchiveDir, " does not exist.")
  } else {
    tmpDir <- file.path(climateArchiveDir, paste0("run_", format(Sys.time(), "%y%m%d_%H%M%S"), suffix))
  }
  dir.create(tmpDir)
  # Find all data files in climate directory, combine with extraFiles
  climateAssessmentFiles <- c(list.files(climateDir, pattern = "\\.(xlsx|csv)$", full.names = TRUE), extraFiles)
  # Copy each file to climateArchiveDir
  lapply(climateAssessmentFiles, function(file) {
    file.copy(file, file.path(tmpDir, basename(file)))
  })
  # Create a tar archive of the directory Compress the tar archive using xz. Need to switch working directory here so
  # tar does not include the full path directory structure in the archive :facepalm:
  tarFile <- paste0(tmpDir, ".tar.gz")
  withr::with_dir(tmpDir, {
    tar(tarFile, files = basename(climateAssessmentFiles), compression = "gzip")
  })
  # Delete the archive directory (i.e. <outputdir>/climate_assessment_data/archive/iteration_<timestamp>, not the
  # <outputdir>/climate_assessment_data/archive directory itself)
  unlink(tmpDir, recursive = TRUE)
  if (returnFn) {
    return(tarFile)
  } else {
    return(invisible())
  }
}
