#' Plot Signal Data with Behavior Codes Using Plotly
#'
#' @description
#' Creates an interactive plot of the signal data, color-coded by behavior.
#' Allows plotting the x-axis in hours or seconds, with customizable time units.
#' You can also adjust the line transparency and specify a custom plot title.
#'
#' @param rec_dt A `data.table` with columns `time`, `signal`, and `behavior`.
#' @param downsample_rate The rate to downsample the data (e.g., `10` for 10 Hz). Default is `10`.
#' @param time_unit The time unit for the x-axis: `"hours"` or `"seconds"`. Default is `"hours"`.
#' @param alpha The transparency level for the lines (0 = fully transparent, 1 = fully opaque). Default is `0.5`.
#' @param plot_title The title of the plot. Default is `"EPG Signal"`.
#' @return A `plotly` object.
#' @import data.table
#' @import plotly
#' @export
#' @examples
#' \dontrun{
#' # Plot with default settings
#' p <- plot_signal(rec_dt)
#' p
#'
#' # Plot with custom alpha and title
#' p <- plot_signal(rec_dt, alpha = 0.7, plot_title = "Custom EPG Plot")
#' p
#' }
plot_signal <- function(rec_dt, downsample_rate = 10, time_unit = "hours", alpha = 0.5, plot_title = "EPG Signal") {
  # Load required libraries
  library(data.table)
  library(plotly)

  # Validate time_unit argument
  if (!(time_unit %in% c("hours", "seconds"))) {
    stop("time_unit must be either 'hours' or 'seconds'.")
  }

  # Validate alpha argument
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a numeric value between 0 and 1.")
  }

  # Downsample data
  rec_dt_ds <- rec_dt[seq(1, .N, by = downsample_rate)]

  # Adjust time based on time_unit
  if (time_unit == "hours") {
    rec_dt_ds[, time_adj := time / 3600]  # Convert seconds to hours
    xaxis_title <- "Time (hours)"
    # Adjust max_time to include the last hour mark
    max_time <- ceiling(max(rec_dt_ds$time_adj) * 2) / 2  # Round up to the next 0.5 hour
    x_ticks <- seq(0, max_time, by = 0.5)  # 0.5 hours = 30 minutes
  } else {
    rec_dt_ds[, time_adj := time]
    xaxis_title <- "Time (s)"
    # Adjust max_time to include the last hour mark in seconds
    max_time <- ceiling(max(rec_dt_ds$time_adj) / 1800) * 1800  # Round up to the next 1800 seconds
    x_ticks <- seq(0, max_time, by = 1800)
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

  # Initialize plotly object
  p <- plot_ly()

  # Keep track of behaviors added to the legend
  behaviors_in_legend <- c()

  # Loop over each unique run
  for (run in unique(rec_dt_ds$run_id)) {
    data_run <- rec_dt_ds[run_id == run]
    beh <- data_run$behavior_label[1]

    # Determine whether to show the legend entry for this behavior
    showlegend_flag <- !(beh %in% behaviors_in_legend)
    behaviors_in_legend <- unique(c(behaviors_in_legend, beh))

    # Add trace for the current run
    p <- add_trace(
      p,
      data = data_run,
      x = ~time_adj,
      y = ~signal,
      type = 'scatter',
      mode = 'lines',
      name = beh,
      line = list(color = behavior_colors[beh], width = 2),
      opacity = alpha,
      legendgroup = beh,
      showlegend = showlegend_flag,
      hoverinfo = 'text',
      text = ~paste(
        'Time:', round(time_adj, 2),
        ifelse(time_unit == "hours", "hours", "s"),
        '<br>Signal:', signal,
        '<br>Behavior:', behavior_label
      )
    )
  }

  # Set layout
  p <- layout(
    p,
    title = plot_title,
    xaxis = list(
      title = xaxis_title,
      tickmode = 'array',
      tickvals = x_ticks,
      ticktext = x_ticks
    ),
    yaxis = list(title = 'Signal (V)'),
    legend = list(title = list(text = 'Behavior'))
  )

  return(p)
}
