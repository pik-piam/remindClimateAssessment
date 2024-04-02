#' renameVariableRemindToMagicc7
#'
#' Renames climate assessment variables given in the REMIND convetion MAGICC7 compatible variable names
#'
#' @md
#' @param varName Variable to be renamed
#' @param magiccVersion Version string of the MAGICC model in use. Default is "7.5.3"
#' @return Renamed variable
#'
#' @author Tonn Rüter
#' @export
renameVariableRemindToMagicc7 <- function(varName, magiccVersion = "7.5.3") {
    varName <- gsub("MAGICC7 AR6|", "AR6 climate diagnostics|", varName, fixed = TRUE)
    varName <- gsub(paste0("\\|([^\\|]+)$", "|MAGICCv", magiccVersion, "|\\1"), varName)
    return(varName)
}