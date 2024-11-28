#' Plot Signal Data with or without Behavior Codes Using ggplot2
#'
#' @description
#' Creates a static plot of the signal data using ggplot2. If behavior codes are available,
#' the plot can be color-coded by behavior. When a specific range is provided, the function
#' automatically adjusts the time unit (seconds or hours) based on the range. The x-axis labels
#' are formatted to one decimal place. You can adjust the line transparency, specify a custom plot
#' title, save the plot to disk, choose the line width, specify a time range to plot, and
#' choose between a wide or square figure.
#'
#' @param rec_dt A `data.table` with columns `time` and `signal`. If behavior codes are available,
#'   it should also include a `behavior` column.
#' @param downsample_rate The rate to downsample the data (e.g., `10` for every 10th point). Default is `10`.
#' @param time_unit The time unit for the x-axis: `"hours"`, `"seconds"`, or `NULL` for automatic selection.
#'   Default is `NULL`.
#' @param alpha The transparency level for the lines (0 = fully transparent, 1 = fully opaque). Default is `0.5`.
#' @param plot_title The title of the plot. Default is `"EPG Signal"`.
#' @param linewidth The line width for the ggplot2 line plot. Default is `0.5`.
#' @param output_file The file path where the plot will be saved (e.g., `"path/to/plot.png"`).
#'   If `NULL`, the plot is displayed in the R plotting window. Default is `NULL`.
#' @param figure_type The aspect ratio of the plot: `"wide"` for rectangular or `"square"` for square.
#'   Default is `"wide"`.
#' @param range A numeric vector of length 2 specifying the start and end times for plotting.
#'   If `NULL`, the entire time range is plotted. Default is `NULL`.
#' @param range_unit The time unit for the `range` parameter: `"hours"` or `"seconds"`.
#'   Must be specified if `range` is provided. Default is `NULL`.
#' @param no_ana Logical value indicating whether to plot without behavior annotations.
#'   If `TRUE`, behavior annotations are ignored and the plot shows only the raw signal.
#'   Default is `FALSE`.
#' @return A `ggplot` object.
#' @import data.table
#' @import ggplot2
#' @import scales
#' @export
#' @examples
#' \dontrun{
#' # Plot without behavior annotations, automatic time unit
#' p <- gg_plot_signal(rec_dt, no_ana = TRUE)
#'
#' # Plot with behavior annotations (default behavior), automatic time unit
#' p <- gg_plot_signal(rec_dt)
#'
#' # Plot with specified range, automatic time unit
#' p <- gg_plot_signal(rec_dt, range = c(0, 526), range_unit = "seconds", no_ana = TRUE)
#' }
gg_plot_signal <- function(rec_dt, downsample_rate = 10, time_unit = NULL, alpha = 0.5,
                           plot_title = "EPG Signal",
                           linewidth = 0.5,
                           output_file = NULL,
                           figure_type = "wide",
                           range = NULL,
                           range_unit = NULL,
                           no_ana = FALSE) {
  # Load required libraries
  library(data.table)
  library(ggplot2)
  library(scales)  # For pretty_breaks()

  # Validate arguments
  if (!is.null(time_unit) && !(time_unit %in% c("hours", "seconds"))) {
    stop("time_unit must be either 'hours', 'seconds', or NULL for automatic selection.")
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

  # Apply range filtering if range is specified
  if (!is.null(range)) {
    # Convert range to seconds for comparison with 'time' column
    if (range_unit == "hours") {
      range_sec <- range * 3600
    } else {
      range_sec <- range
    }
    # Filter data based on time
    rec_dt_ds <- rec_dt_ds[time >= range_sec[1] & time <= range_sec[2]]
  }

  # Check if rec_dt_ds is empty after filtering
  if (nrow(rec_dt_ds) == 0) {
    stop("No data available in the specified range.")
  }

  # Adjust x-axis limits based on specified range or data
  if (!is.null(range)) {
    min_time_sec <- range_sec[1]
    max_time_sec <- range_sec[2]
  } else {
    min_time_sec <- min(rec_dt_ds$time)
    max_time_sec <- max(rec_dt_ds$time)
  }

  # Determine time_unit if NULL
  if (is.null(time_unit)) {
    total_time <- max_time_sec - min_time_sec  # in seconds
    if (total_time <= 3600) {
      time_unit <- "seconds"
    } else {
      time_unit <- "hours"
    }
  }

  # Adjust time based on time_unit
  if (time_unit == "hours") {
    rec_dt_ds[, time_adj := time / 3600]  # Convert seconds to hours
    xaxis_title <- "Time (hours)"
    min_time <- min_time_sec / 3600
    max_time <- max_time_sec / 3600
  } else {
    rec_dt_ds[, time_adj := time]
    xaxis_title <- "Time (s)"
    min_time <- min_time_sec
    max_time <- max_time_sec
  }

  # Set x-axis breaks
  breaks_number <- 5  # Desired number of breaks
  x_breaks <- scales::pretty_breaks(n = breaks_number)(c(min_time, max_time))

  # Format x-axis labels to one decimal place
  label_format <- function(x) sprintf("%.1f", x)

  if (no_ana) {
    # Plot without behavior annotations
    p <- ggplot(rec_dt_ds, aes(x = time_adj, y = signal)) +
      geom_line(alpha = alpha, linewidth = linewidth) +
      scale_x_continuous(
        name = xaxis_title,
        breaks = x_breaks,
        limits = c(min_time, max_time),
        labels = label_format,
        expand = c(0, 0)
      ) +
      labs(
        title = plot_title,
        y = "Signal (V)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  } else {
    # Plot with behavior annotations
    if (!"behavior" %in% names(rec_dt_ds)) {
      stop("The 'behavior' column is missing in 'rec_dt'. Please provide data with behavior annotations or set no_ana = TRUE.")
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
      scale_x_continuous(
        name = xaxis_title,
        breaks = x_breaks,
        limits = c(min_time, max_time),
        labels = label_format,
        expand = c(0, 0)
      ) +
      labs(
        title = plot_title,
        y = "Signal (V)"
      ) +
      theme_minimal() +
      theme(
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)
      )
  }

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
