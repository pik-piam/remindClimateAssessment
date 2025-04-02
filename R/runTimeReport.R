#' Generate a report of run times.
#'
#' This function takes a named vector of run times and generates a formatted report showing the duration of each run.
#' The names of the run times must follow the pattern 'task_id_1 start' and 'task_id_1 end', where 'task_id_1'
#' can be any string identifier.
#'
#' @param runTimes A named vector of run times
#' @param prefix A string to prefix each line of the report. Default is "".
#' @return A formatted string showing the duration of each run.
#' @examples
#' \dontrun{
#' # Typical usage: Set up as empty vector
#' runTimes <- c()
#' # [... run set up ...]
#' runTimes <- c("task1 start" = Sys.time())
#' # [... run task1 ...]
#' runTimes <- c("task1 end" = Sys.time())
#' # [... something untimed in between ...]
#' runTimes <- c("task2 start" = Sys.time())
#' # [... run task2 ...]
#' runTimes <- c("task2 end" = Sys.time())
#' runTimeReport(runTimes)
#' }
#' @export
runTimeReport <- function(runTimes, prefix = "") {
  if (length(runTimes) %% 2 != 0) {
    stop(paste(
      "Invalid number of run times: Must have an even number of elements.",
      "\nrun_time_ids found:", paste(names(runTimes), collapse = ", ")
    ))
  }
  # Indexes of start and end times: Start times are at odd indexes, end times at even indexes
  startIdx <- seq(1, length(runTimes), 2)
  endIdx <- seq(2, length(runTimes), 2)
  # Grab the names assigned to the individual run times
  startNames <- names(runTimes)[startIdx]
  endNames <- names(runTimes)[endIdx]
  # Safety check for names pattern
  validPattern <- all(grepl(" start$", startNames)) &&
    all(grepl(" end$", endNames)) &&
    all(sub(" start$", "", startNames) == sub(" end$", "", endNames))
  if (!validPattern) {
    stop(paste(
      "Invalid pattern: Names of individual run times must follow 'run_time_id start' and 'run_time_id end'",
      "\nrun_time_ids found:", paste(names(runTimes), collapse = ", ")
    ))
  }
  # difftime calculates the difference between two date-time objects. Use it to calculate the run times in seconds
  runTimeDeltas <- difftime(runTimes[endIdx], runTimes[startIdx], units = "secs")
  runTimeNames <- sub(" start$", "", startNames)
  runTimesStrings <- sprintf("%.3f", as.numeric(runTimeDeltas))
  # Determine the maximum length of the names and the formatted difftime values for formatting
  maxDeltaLength <- max(nchar(runTimesStrings))
  maxNameLength <- max(nchar(runTimeNames))
  # Create a string with names and formatted difftime values, aligning the semicolons and values
  return(paste(
    sprintf("%s%-*s : %*s", prefix, maxNameLength, runTimeNames, maxDeltaLength, runTimesStrings),
    collapse = "\n"
  ))
}
