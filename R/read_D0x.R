# Load necessary libraries
library(DBI)
library(duckdb)

#' Read Single or Multiple Binary Files with Signal Data
#'
#' @description
#' This function allows you to read either a single binary `.D0x` file, a subset of `.D0x` files,
#' or multiple `.D0x` files associated with a common base recording name. When reading multiple
#' files, it concatenates the signal data and generates a continuous time sequence
#' based on the provided sample rate. Optionally, it can utilize DuckDB for efficient
#' data storage and retrieval.
#'
#' @param base_file_name A string specifying the common prefix of the files to read.
#' @param hour An integer, vector of integers, or `NA` specifying which `.D0x` files to load.
#'   - If a single integer (e.g., `5`), the function will load only the corresponding `.D05` file.
#'   - If a vector of integers (e.g., `1:5` or `c(1,3,5)`), the function will load the specified `.D0x` files.
#'   - If set to `NA` (default), all `.D0x` files matching the `base_file_name` will be loaded.
#' @param sample_rate A numeric value indicating the sample rate used to calculate
#'   time intervals (default is `100`).
#' @param endian A string specifying the byte order of the binary files.
#'   Options are `"little"` (default) or `"big"`.
#' @param dir A string specifying the directory containing the binary files
#'   (default is the current directory `"."`).
#' @param use_duckdb A logical value indicating whether to use DuckDB for data storage
#'   (default is `FALSE`).
#' @param db_file A string specifying the path to the DuckDB database file.
#'   If `NULL` (default), an in-memory database is used.
#'
#' @return A data frame containing the signal data and a continuous time sequence.
#'   The data frame has two columns:
#'   \describe{
#'     \item{Signal}{Numeric vector of signal values from the binary file(s).}
#'     \item{Time}{Numeric vector representing the continuous time sequence.}
#'   }
#'
#' @details
#' - **Single File Mode**: If `hour` is specified as a single integer (e.g., `hour = 5`),
#'   the function will load only the corresponding `.D0x` file (e.g., `.D05`) associated
#'   with the provided `base_file_name`. It generates a `Time` sequence starting from `0`.
#'
#' - **Subset File Mode**: If `hour` is specified as a vector of integers (e.g., `hour = 1:5`),
#'   the function will load the corresponding `.D0x` files (e.g., `.D01` to `.D05`).
#'   It generates a continuous `Time` sequence across the specified files based on the `sample_rate`.
#'
#' - **Multiple Files Mode**: If `hour` is set to `NA`, the function searches for all
#'   `.D0x` files matching the `base_file_name` and loads them in sequential order.
#'   It concatenates the signal data and generates a continuous `Time` sequence based
#'   on the `sample_rate`.
#'
#' - **DuckDB Integration**: When `use_duckdb` is `TRUE`, the function stores the data
#'   in a DuckDB database, which is efficient for handling large datasets. Otherwise,
#'   data frames are used for in-memory storage.
#'
#' - **Database File Handling**:
#'   - The `db_file` parameter should be a string specifying the full or relative path
#'     to the DuckDB database file, including the desired filename.
#'   - Recommended file extensions include `.duckdb` for clarity, `.db` for general database purposes,
#'     or any custom extension that suits your naming conventions.
#'   - If the directory specified in `db_file` does not exist, the function will attempt to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: Read a specific binary file (e.g., .D05)
#' signal_data_single <- read_D0x(
#'   base_file_name = "recording1",
#'   hour = 5,
#'   sample_rate = 100,
#'   endian = "little",
#'   dir = "path/to/your/directory",
#'   use_duckdb = FALSE
#' )
#'
#' # Example 2: Read a subset of binary files (e.g., .D01 to .D05)
#' signal_data_subset <- read_D0x(
#'   base_file_name = "recording1",
#'   hour = 1:5,
#'   sample_rate = 100,
#'   endian = "little",
#'   dir = "path/to/your/directory",
#'   use_duckdb = FALSE
#' )
#'
#' # Example 3: Read and concatenate all binary files starting with "recording1"
#' combined_data <- read_D0x(
#'   base_file_name = "recording1",
#'   hour = NA,
#'   sample_rate = 100,
#'   endian = "little",
#'   dir = "path/to/your/directory",
#'   use_duckdb = FALSE  # Set to TRUE to use DuckDB
#' )
#'
#' # Example 4: Read and concatenate a range of binary files using DuckDB with a specified database file
#' combined_data_duckdb <- read_D0x(
#'   base_file_name = "recording1",
#'   hour = 6:12,
#'   sample_rate = 100,
#'   endian = "little",
#'   dir = "path/to/your/directory",
#'   use_duckdb = TRUE,
#'   db_file = "databases/recording1_signals.duckdb"
#' )
#'
#' # View the combined data
#' head(combined_data)
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbGetQuery dbExecute
#' @importFrom duckdb duckdb
#' @export
read_D0x <- function(base_file_name,
                     hour = NA,
                     sample_rate = 100,
                     endian = "little",
                     dir = ".",
                     use_duckdb = FALSE,
                     db_file = NULL) {
  # Helper function to read a single .D0x file
  read_one_D0x <- function(file_path, endian = "little") {
    # Open a connection to the binary file in read-binary mode
    con <- file(file_path, "rb")

    # Ensure the connection is closed even if an error occurs
    on.exit(close(con))

    # Read the first three header lines
    headers <- readLines(con, n = 3)

    # Optional: Print headers to verify
    message("Headers from ", basename(file_path), ":")
    print(headers)

    # Read the binary data as 32-bit floats (float32)
    x_values <- readBin(
      con = con,
      what = "numeric",
      size = 4,         # 4 bytes for float32
      endian = endian,  # "little" or "big"
      n = .Machine$integer.max
    )

    # Create a data frame with the data
    x <- data.frame(Signal = x_values)

    return(x)
  }

  # Validate 'hour' parameter
  if (!all(is.na(hour))) {
    if (!is.numeric(hour) || any(hour < 0) || any(hour != as.integer(hour))) {
      stop("'hour' must be NA, a single non-negative integer, or a vector of non-negative integers.")
    }
  }

  # Determine the files to load based on the 'hour' parameter
  if (!all(is.na(hour))) {
    # If 'hour' is a single integer, ensure it's a vector
    if (length(hour) == 1) {
      hour <- as.integer(hour)
    } else {
      hour <- as.integer(hour)
    }

    # Format each hour with leading zero (e.g., 1 -> D01)
    hours_formatted <- sprintf("D%02d", hour)

    # Create a regex pattern to match any of the specified hours
    # e.g., "^recording1\\.(D01|D02|D05)$"
    pattern <- paste0("^", base_file_name, "\\.(", paste(hours_formatted, collapse = "|"), ")$")

    # List files matching the pattern
    files <- list.files(path = dir, pattern = pattern, full.names = TRUE)

    if (length(files) == 0) {
      stop(paste0("No files found matching ", base_file_name, " with hour(s) ", paste(hour, collapse = ", "), "."))
    }

    # Sort files to ensure correct order
    files <- sort(files)

    # Initialize a list to store data frames
    data <- vector("list", length(files))
    cumulative_time <- 0
    for (i in seq_along(files)) {
      file_path <- files[i]
      x <- read_one_D0x(file_path, endian)
      n_samples <- nrow(x)

      # Generate Time sequence
      x$Time <- seq(
        from = cumulative_time,
        by = 1 / sample_rate,
        length.out = n_samples
      )

      # Update cumulative_time
      cumulative_time <- cumulative_time + (n_samples / sample_rate)

      # Store in list
      data[[i]] <- x
    }

    # Combine data frames
    combined_data <- do.call(rbind, data)
    rownames(combined_data) <- NULL  # Clean up row names
    return(combined_data)
  }

  # If multiple files are to be loaded (hour = NA)
  if (use_duckdb) {
    # Handle db_file path
    if (!is.null(db_file)) {
      # Extract directory path from db_file
      dir_path <- dirname(db_file)

      # Check if directory exists; if not, attempt to create it
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        if (!dir.exists(dir_path)) {
          stop(paste0("The directory ", dir_path, " does not exist and could not be created. Please provide a valid path."))
        }
        message(paste0("Created directory: ", dir_path))
      }
    }

    # Initialize DuckDB connection
    if (is.null(db_file)) {
      # Use in-memory database
      con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      message("Using in-memory DuckDB database.")
    } else {
      # Use specified database file
      con <- dbConnect(duckdb::duckdb(), dbdir = db_file)
      message(paste0("Using DuckDB database at: ", db_file))
    }
    on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

    # Initialize DuckDB table
    dbExecute(con, "CREATE TABLE IF NOT EXISTS signals (Signal DOUBLE, Time DOUBLE)")

    # List all matching .D0x files
    pattern_all <- paste0("^", base_file_name, "\\.D\\d+$")
    files_all <- list.files(path = dir, pattern = pattern_all, full.names = TRUE)

    if (length(files_all) == 0) {
      stop("No files found matching the recording name.")
    }

    # Sort files to ensure correct order
    files_all <- sort(files_all)

    cumulative_time <- 0
    for (file_path in files_all) {
      x <- read_one_D0x(file_path, endian)
      n_samples <- nrow(x)

      # Generate Time sequence
      x$Time <- seq(
        from = cumulative_time,
        by = 1 / sample_rate,
        length.out = n_samples
      )

      # Update cumulative_time
      cumulative_time <- cumulative_time + (n_samples / sample_rate)

      # Write data to DuckDB table
      dbWriteTable(con, "signals", x, append = TRUE, row.names = FALSE)
    }

    # Retrieve data from DuckDB
    data <- dbGetQuery(con, "SELECT * FROM signals")
    return(data)
  } else {
    # Use data frames for storage
    # Load all .D0x files associated with the base_file_name
    pattern <- paste0("^", base_file_name, "\\.D\\d+$")
    files <- list.files(path = dir, pattern = pattern, full.names = TRUE)

    if (length(files) == 0) {
      stop("No files found matching the recording name.")
    }

    # Sort files to ensure correct order
    files <- sort(files)

    data_list <- vector("list", length(files))
    cumulative_time <- 0
    for (i in seq_along(files)) {
      file_path <- files[i]
      x <- read_one_D0x(file_path, endian)
      n_samples <- nrow(x)

      # Generate Time sequence
      x$Time <- seq(
        from = cumulative_time,
        by = 1 / sample_rate,
        length.out = n_samples
      )

      # Update cumulative_time
      cumulative_time <- cumulative_time + (n_samples / sample_rate)

      # Store in list
      data_list[[i]] <- x
    }

    # Combine data frames
    combined_data <- do.call(rbind, data_list)
    rownames(combined_data) <- NULL  # Clean up row names
    return(combined_data)
  }
}
