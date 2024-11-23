#' Assign Behavior Codes to Signal Data
#'
#' @description
#' Assigns behavior codes to each time point in the signal data based on behavior intervals.
#' The `end_time` column is automatically created by shifting the `start_time` column,
#' assuming that the end time of each behavior is the start time of the next behavior.
#'
#' @param rec_dt A `data.table` with columns `time` and `signal`.
#' @param ana_dt A `data.table` with columns `behavior` and `start_time`.
#' @return The `rec_dt` `data.table` with an added column `behavior`.
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' rec_dt <- assign_behavior_codes(rec_dt, ana_dt)
#' }
assign_behavior_codes <- function(rec_dt, ana_dt) {
  # Ensure ana_dt is sorted by start_time
  setorder(ana_dt, start_time)

  # Create 'end_time' by shifting 'start_time' forward
  max_time <- max(rec_dt$time, na.rm = TRUE)
  ana_dt[, end_time := shift(start_time, type = "lead", fill = max_time)]

  # Time vectors
  times <- rec_dt$time
  starts <- ana_dt$start_time
  ends <- ana_dt$end_time

  # Interval indices
  interval_indices <- findInterval(times, starts)

  # Validity check: Ensure indices are within bounds
  valid_indices <- interval_indices > 0 & interval_indices <= nrow(ana_dt)

  # Assign behavior codes
  behavior_code <- rep(NA_integer_, length(times))

  valid_times <- valid_indices & times >= starts[interval_indices] & times < ends[interval_indices]
  behavior_code[valid_times] <- ana_dt$behavior[interval_indices[valid_times]]

  rec_dt[, behavior := behavior_code]

  return(rec_dt)
}
