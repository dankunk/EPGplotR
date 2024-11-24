#' Load .D0x Files and Return Signal Data
#'
#' @description
#' Reads all `.D0x` files in the same directory that share the same base name as the selected file.
#' Assumes the files contain 32-bit floats (float32) in little-endian format, sampled at 100 Hz.
#' The files start with three header lines that are skipped.
#'
#' @param file_path (Optional) The path to one of the `.D0x` files. If not provided, prompts the user to select a file.
#' @return A `data.table` with columns `time` and `signal`.
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' # Prompt user to select a file
#' signal_dt <- load_d0x_files()
#'
#' # Use a specific file path
#' signal_dt <- load_d0x_files(file_path = "path/to/your/file.D01")
#' }
load_d0x_files <- function(file_path = FALSE) {
  if (identical(file_path, FALSE)) {
    cat("Please select one of the .D0x files...\n")
    file_path <- file.choose()
  }

  # Get directory and base name
  dir_path <- dirname(file_path)
  file_name <- basename(file_path)

  # Extract base name without extension
  base_name <- sub("\\.D0[0-9]+$", "", file_name)

  # List all matching .D0x files
  files <- list.files(
    path = dir_path,
    pattern = paste0("^", base_name, "\\.D0[0-9]+$"),
    full.names = TRUE
  )

  # Sort files (e.g., D01, D02, ...)
  files <- sort(files)

  # Initialize lists for data
  signal_list <- list()
  time_list <- list()
  total_samples <- 0

  # Set the correct sampling rate (adjust if necessary)
  sampling_rate <- 100  # Assuming 100 Hz sampling rate

  for (file in files) {
    # Use tryCatch to ensure the connection is closed properly
    data <- NULL
    con <- file(file, "rb")
    tryCatch({
      # Read and skip the first three header lines
      headers <- readLines(con, n = 3)

      # Read the binary data as 32-bit floats (float32)
      data <- readBin(
        con = con,
        what = "numeric",
        size = 4,         # 4 bytes per sample (32-bit float)
        endian = "little",  # Adjust to "big" if necessary
        n = .Machine$integer.max
      )
    }, finally = {
      # Ensure the connection is closed
      close(con)
    })

    n_samples <- length(data)
    signal_list[[length(signal_list) + 1]] <- data

    # Create time vector for this file
    start_time <- total_samples / sampling_rate  # Since 100 Hz
    time <- seq(start_time, by = 1 / sampling_rate, length.out = n_samples)
    time_list[[length(time_list) + 1]] <- time

    total_samples <- total_samples + n_samples
  }

  # Combine all data
  signal <- unlist(signal_list)
  time <- unlist(time_list)

  # Create data.table
  dt <- data.table(time = time, signal = signal)

  return(dt)
}
