#' exportConfFromYaml
#'
#' Read yaml export configuration. It determines which variables to extract from the MAGICC7 climate assessment data & 
#' to which file they are supposed to be written. Resulting data frame contains `magicc7Variable`, `gamsVariable` and
#' `fileName` columns.
#' @md 
#' @param yamlFileName Location of the configuration file. Default is 'which_var_in_which_file.yaml'
#' @importFrom yaml read_yaml
#' @author Tonn Rüter
#' @export
library(yaml)
exportConfFromYaml <- function(yamlFileName = "default.yaml") {
    # Read yaml file
    associateVariablesAndFiles <- as.data.frame(do.call(rbind, read_yaml(yamlFileName)))
    # Check if all necessary columns are present
    mustHave <- c("magicc7Variable", "gamsVariable", "fileName")
    available <- mustHave %in% names(associateVariablesAndFiles)
    if (!all(available)) {
        stop(paste0(
            "exportConfFromYaml: Need to provide association between climate assessment and gams variable names. ",
            "Missing: ", paste(mustHave[!available], collapse = ", ")
        ))
    }
    return(associateVariablesAndFiles)
}
