#' Plot Signal Data with Behavior Codes Using ggplot2
#'
#' @description
#' Creates a static plot of the signal data using ggplot2, color-coded by behavior.
#' Allows plotting the x-axis in hours or seconds, with customizable time units.
#' You can adjust the line transparency, specify a custom plot title, save the plot to disk,
#' choose the line width, specify a time range to plot, and choose between a wide or square figure.
#'
#' @param rec_dt A `data.table` with columns `time`, `signal`, and `behavior`.
#' @param downsample_rate The rate to downsample the data (e.g., `10` for 10 Hz). Default is `10`.
#' @param time_unit The time unit for the x-axis: `"hours"` or `"seconds"`. Default is `"hours"`.
#' @param alpha The transparency level for the lines (0 = fully transparent, 1 = fully opaque). Default is `0.5`.
#' @param plot_title The title of the plot. Default is `"EPG Signal"`.
#' @param linewidth The linewidth argument for a ggplot2 line plot. See ggplot2 for more details. Default is `0.5`.
#' @param output_file The file path where the plot will be saved (e.g., `"path/to/plot.png"`).
#'   If `NULL`, the plot is displayed in the R plotting window. Default is `NULL`.
#' @param figure_type The aspect ratio of the plot: `"wide"` for rectangular or `"square"` for square. Default is `"wide"`.
#' @param range A numeric vector of length 2 specifying the start and end times for plotting.
#'   If `NULL`, the entire time range is plotted. Default is `NULL`.
#' @param range_unit The time unit for the `range` parameter: `"hours"` or `"seconds"`.
#'   Must be specified if `range` is provided. Default is `NULL`.
#' @return A `ggplot` object.
#' @import data.table
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' # Display the plot in the R plotting window
#' p <- gg_plot_signal(rec_dt)
#'
#' # Save the plot to disk as a PNG file with a wide aspect ratio
#' gg_plot_signal(rec_dt, output_file = "path/to/plot.png", figure_type = "wide")
#'
#' # Plot a specific time range from 500 to 1000 seconds
#' gg_plot_signal(rec_dt, range = c(500, 1000), range_unit = "seconds")
#'
#' # Plot a specific time range from 5 to 8 hours
#' gg_plot_signal(rec_dt, range = c(5, 8), range_unit = "hours")
#' }
gg_plot_signal <- function(rec_dt, downsample_rate = 10, time_unit = "hours", alpha = 0.5,
                           plot_title = "EPG Signal",
                           linewidth = 0.5,
                           output_file = NULL,
                           figure_type = "wide",
                           range = NULL,
                           range_unit = NULL) {
  # Load required libraries
  library(data.table)
  library(ggplot2)

  # Validate arguments
  if (!(time_unit %in% c("hours", "seconds"))) {
    stop("time_unit must be either 'hours' or 'seconds'.")
  }
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric value between 0 and 1.")
  }
  if (!(figure_type %in% c("wide", "square"))) {
    stop("figure_type must be either 'wide' or 'square'.")
  }
  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 2) {
      stop("range must be a numeric vector of length 2.")
    }
    if (is.null(range_unit)) {
      stop("range_unit must be specified when range is provided.")
    }
    if (!(range_unit %in% c("hours", "seconds"))) {
      stop("range_unit must be either 'hours' or 'seconds'.")
    }
  }

  # Downsample data
  rec_dt_ds <- rec_dt[seq(1, .N, by = downsample_rate)]

  # Adjust time based on time_unit
  if (time_unit == "hours") {
    rec_dt_ds[, time_adj := time / 3600]  # Convert seconds to hours
    xaxis_title <- "Time (hours)"
  } else {
    rec_dt_ds[, time_adj := time]
    xaxis_title <- "Time (s)"
  }

  # Apply range filtering if range is specified
  if (!is.null(range)) {
    # Convert range to time_unit
    if (range_unit != time_unit) {
      if (range_unit == "hours" && time_unit == "seconds") {
        # Convert range from hours to seconds
        range_adj <- range * 3600
      } else if (range_unit == "seconds" && time_unit == "hours") {
        # Convert range from seconds to hours
        range_adj <- range / 3600
      }
    } else {
      range_adj <- range
    }
    # Filter data based on time_adj
    rec_dt_ds <- rec_dt_ds[time_adj >= range_adj[1] & time_adj <= range_adj[2]]
  }

  # Check if rec_dt_ds is empty after filtering
  if (nrow(rec_dt_ds) == 0) {
    stop("No data available in the specified range.")
  }

  # Adjust x-axis limits based on specified range or data
  if (!is.null(range)) {
    min_time <- range_adj[1]
    max_time <- range_adj[2]
  } else {
    min_time <- min(rec_dt_ds$time_adj)
    max_time <- max(rec_dt_ds$time_adj)
  }

  # Set x-axis breaks
  # For hours, breaks every 0.5 hours (30 minutes)
  # For seconds, breaks every 1800 seconds (30 minutes)
  if (time_unit == "hours") {
    x_breaks <- seq(floor(min_time * 2) / 2, ceiling(max_time * 2) / 2, by = 1)
  } else {
    x_breaks <- seq(floor(min_time / 1800) * 1800, ceiling(max_time / 1800) * 1800, by = 1800)
  }

  # Map behavior codes to labels
  behavior_labels <- c(
    "1" = "np",
    "2" = "C",
    "4" = "E1",
    "5" = "E2",
    "6" = "F",
    "7" = "G",
    "8" = "pd"
  )

  rec_dt_ds[, behavior_label := behavior_labels[as.character(behavior)]]

  # Replace NA behavior labels with 'Unknown'
  rec_dt_ds[is.na(behavior_label), behavior_label := 'Unknown']

  # Map behavior labels to colors
  behavior_colors <- c(
    "np" = "#1f77b4",
    "C" = "#ff7f0e",
    "E1" = "#2ca02c",
    "E2" = "#d62728",
    "F" = "#9467bd",
    "G" = "#8c564b",
    "pd" = "#e377c2",
    "Unknown" = "#7f7f7f"
  )

  # Create a run ID to split data into segments where behavior is continuous
  rec_dt_ds[, run_id := rleid(behavior_label)]

  # Create a grouping variable for continuous behavior segments
  rec_dt_ds[, group := .GRP, by = .(behavior_label, run_id)]

  # Create the ggplot
  p <- ggplot(rec_dt_ds, aes(x = time_adj, y = signal, color = behavior_label, group = group)) +
    geom_line(alpha = alpha, linewidth = linewidth) +
    scale_color_manual(values = behavior_colors, name = "Behavior") +
    scale_x_continuous(name = xaxis_title, breaks = x_breaks, limits = c(min_time, max_time)) +
    labs(
      title = plot_title,
      y = "Signal (mV)"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(hjust = 0.5)
    )

  # Save the plot to disk if output_file is provided
  if (!is.null(output_file)) {
    # Set figure dimensions based on figure_type
    if (figure_type == "wide") {
      fig_width <- 16
      fig_height <- 9
    } else {  # "square"
      fig_width <- 9
      fig_height <- 9
    }
    # Save the plot
    ggsave(filename = output_file, plot = p, width = fig_width, height = fig_height, units = "in")
  } else {
    # Display the plot in the R plotting window
    print(p)
  }

  # Return the ggplot object invisibly
  invisible(p)
}
