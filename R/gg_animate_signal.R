#' Animate Signal Data with Behavior Codes Using ggplot2 and gganimate
#'
#' @description
#' Creates an animated plot of the signal data using ggplot2 and gganimate, color-coded by behavior.
#' The animation shows the signal over time, with an option to bin the data.
#' By default, all data points are used without binning.
#'
#' @param rec_dt A `data.table` with columns `time`, `signal`, and `behavior`.
#' @param output_file The file path where the GIF will be saved (e.g., `"path/to/output.gif"`).
#' @param duration Duration of the animation in seconds. Default is `30`.
#' @param fps Frames per second for the animation. Default is `10`.
#' @param time_unit The time unit for the x-axis: `"hours"` or `"seconds"`. Default is `"hours"`.
#' @param alpha The transparency level for the lines (0 = fully transparent, 1 = fully opaque). Default is `0.8`.
#' @param plot_title The title of the plot. Default is `"EPG Signal Animation"`.
#' @param figure_type The aspect ratio of the plot: `"wide"` for rectangular or `"square"` for square. Default is `"wide"`.
#' @param bin_width The width of time bins for aggregating data (in seconds). Default is `NULL` (no binning).
#' @return An animated `ggplot` object saved as a GIF file.
#' @import data.table
#' @import ggplot2
#' @import gganimate
#' @export
#' @examples
#' \dontrun{
#' # Animate and save the GIF without binning (default)
#' gg_animate_signal(
#'   rec_dt,
#'   output_file = "C:/Users/danie/Desktop/epg_signal.gif"
#' )
#'
#' # Animate with binning (e.g., bin width of 60 seconds)
#' gg_animate_signal(
#'   rec_dt,
#'   output_file = "C:/Users/danie/Desktop/epg_signal_binned.gif",
#'   bin_width = 60
#' )
#' }
gg_animate_signal <- function(rec_dt, output_file, duration = 30, fps = 10,
                              time_unit = "hours", alpha = 0.8,
                              plot_title = "EPG Signal Animation",
                              figure_type = "wide", bin_width = NULL) {
  # Load required libraries
  library(data.table)
  library(ggplot2)
  library(gganimate)
  library(gifski)

  # Validate arguments
  if (!(time_unit %in% c("hours", "seconds"))) {
    stop("time_unit must be either 'hours' or 'seconds'.")
  }
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(fps) || fps <= 0) {
    stop("fps must be a positive numeric value.")
  }
  if (!is.numeric(duration) || duration <= 0) {
    stop("duration must be a positive numeric value.")
  }
  if (!is.character(output_file) || length(output_file) != 1) {
    stop("output_file must be provided as a single character string with a file name and extension.")
  }
  if (!(figure_type %in% c("wide", "square"))) {
    stop("figure_type must be either 'wide' or 'square'.")
  }
  if (!is.null(bin_width) && (!is.numeric(bin_width) || bin_width <= 0)) {
    stop("bin_width must be a positive numeric value.")
  }

  # Adjust time based on time_unit
  if (time_unit == "hours") {
    rec_dt[, time_adj := time / 3600]  # Convert seconds to hours
    xaxis_title <- "Time (hours)"
  } else {
    rec_dt[, time_adj := time]
    xaxis_title <- "Time (s)"
  }

  # Bin data if bin_width is provided
  if (!is.null(bin_width)) {
    # Adjust bin_width based on time_unit
    if (time_unit == "hours") {
      bin_width_adj <- bin_width / 3600  # Convert bin_width to hours
    } else {
      bin_width_adj <- bin_width
    }
    rec_dt[, time_bin := floor(time_adj / bin_width_adj) * bin_width_adj]

    # Aggregate data by time bins
    rec_dt_agg <- rec_dt[, .(
      time_adj = mean(time_adj),
      signal = mean(signal, na.rm = TRUE),
      behavior = behavior[.N]
    ), by = time_bin]
  } else {
    rec_dt_agg <- rec_dt
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

  rec_dt_agg[, behavior_label := behavior_labels[as.character(behavior)]]
  rec_dt_agg[is.na(behavior_label), behavior_label := 'Unknown']

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

  # Assign unique group IDs for continuous behavior segments
  rec_dt_agg[, run_id := rleid(behavior_label)]
  rec_dt_agg[, group_id := paste0(behavior_label, "_", run_id)]

  # Determine figure dimensions based on figure_type
  if (figure_type == "wide") {
    fig_width <- 1600
    fig_height <- 900
  } else {  # "square"
    fig_width <- 900
    fig_height <- 900
  }

  # Create the ggplot
  p <- ggplot(rec_dt_agg, aes(x = time_adj, y = signal, color = behavior_label, group = group_id)) +
    geom_line(alpha = alpha, linewidth = 0.7) +
    scale_color_manual(values = behavior_colors, name = "Behavior") +
    labs(
      title = plot_title,
      x = xaxis_title,
      y = "Signal (mV)"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(hjust = 0.5)
    ) +
    transition_reveal(time_adj) +
    ease_aes('linear')

  # Animate and save the plot using gifski_renderer
  anim <- animate(
    p,
    fps = fps,
    duration = duration,
    renderer = gifski_renderer(output_file),
    width = fig_width,
    height = fig_height,
    units = "px",
    end_pause = 10
  )

  # Return the animated plot object
  return(anim)
}
