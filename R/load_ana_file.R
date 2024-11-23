#' Load .ANA File and Return Behavior Data
#'
#' @description
#' Reads a `.ANA` file and extracts behavior codes and their corresponding start times.
#' Assumes the file is tab-separated with at least three columns.
#'
#' @param file_path (Optional) The path to the `.ANA` file. If not provided, prompts the user to select a file.
#' @return A `data.table` with columns `behavior` and `start_time`.
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' # Prompt user to select a file
#' behavior_dt <- load_ana_file()
#'
#' # Use a specific file path
#' behavior_dt <- load_ana_file(file_path = "path/to/your/file.ANA")
#' }
load_ana_file <- function(file_path = FALSE) {
  if (identical(file_path, FALSE)) {
    cat("Please select the corresponding .ANA file...\n")
    file_path <- file.choose()
  }

  # Read the .ANA file using read.table with correct encoding
  dt <- read.table(
    file = file_path,
    header = FALSE,
    sep = "\t",
    col.names = c("behavior", "start_time", "signal_mv"),
    fileEncoding = "UTF-16LE",  # Change to "UTF-16BE" if needed
    stringsAsFactors = FALSE
  )

  # Convert to data.table
  dt <- data.table(dt)

  # Remove the signal_mv column
  dt[, signal_mv := NULL]

  # Ensure numeric columns
  dt[, behavior := as.numeric(behavior)]
  dt[, start_time := as.numeric(start_time)]

  return(dt)
}
