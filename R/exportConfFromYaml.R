#' exportConfFromYaml
#'
#' Read yaml export configuration. It determines which variables to extract from the MAGICC7 climate assessment data &
#' to which file they are supposed to be written. Resulting data frame contains `magicc7Variable`, `gamsVariable` and
#' `fileName` columns.
#' @md
#' @param yamlFileName Location of the configuration file. Default is 'which_var_in_which_file.yaml'
#' @importFrom yaml read_yaml
#' @importFrom dplyr bind_rows
#' @return A tibble data frame containing the association between climate assessment and gams variable names
#' @author Tonn Rüter
#' @export
exportConfFromYaml <- function(yamlFileName = "default.yaml") {
  # Reading the yaml file gives a list-of-lists. `bind_rows` converts it to a tibble with appropriate column names
  associateVariablesAndFiles <- bind_rows(read_yaml(yamlFileName))
  # Check if all necessary columns are present
  mustHave <- c("magicc7Variable", "gamsVariable", "fileName")
  available <- mustHave %in% names(associateVariablesAndFiles)
  if (!all(available)) {
    stop(paste0(
      "exportConfFromYaml: Need to provide association between climate assessment and gams variable names. ",
      "Missing: ", paste(mustHave[!available], collapse = ", ")
    ))
  }
  return(
    # Add REMIND variable names based on conversion function
    associateVariablesAndFiles %>% mutate(
      remindVariable = vapply(.data$magicc7Variable,
                              renameVariableMagicc7ToRemind,
                              USE.NAMES = FALSE, FUN.VALUE = character(1))
    )
  )
}
