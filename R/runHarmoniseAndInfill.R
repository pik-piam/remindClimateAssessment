#' runHarmoniseAndInfill
#'
#' Executes the harmonisation and infilling script. Currently requires environment variable `CLIMATE_ASSESSMENT_ROOT`
#' to be set.
#' @md
#' @param emissionFile Location of the REMIND emission file
#' @author Tonn Rüter
#' @export
runHarmoniseAndInfill <- function(
    emissionsFile,
    workingDir = "climate-assessment-data",
    scriptsDir = file.path(Sys.getenv("CLIMATE_ASSESSMENT_ROOT"), "scripts"),
    verbose = NULL
) {
    # If verbose is a character string, interpret it as a logfile to append to or a boolean
    if (is.character(verbose)) {
        logFile <- verbose
        verbose <- FALSE
    }
    scriptsDir <- normalizePath(scriptsDir, mustWork = TRUE)
    cmd <- paste(
        "python",
        normalizePath(file.path(scriptsDir, "run_harm_inf.py"), mustWork = TRUE),
        emissionsFile,
        workingDir,
        "--no-inputcheck",
        "--infilling-database", infillingDatabaseFile
    )
    if (!is.null(verbose)) {
        logMsg <- paste0(date(), "Running harmonisation and infilling script with the following command:\n", cmd, "\n")
        if (is.logical(verbose) && verbose) cat(logMsg)
        if (!is.null(logFile)) capture.output(cat(logMsg), file = logFile, append = TRUE)
    }
    system(cmd, show.output.on.console = TRUE)
}
