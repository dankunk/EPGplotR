#' Plot Signal Data with Behavior Codes Using Plotly
#'
#' @description
#' Creates an interactive plot of the signal data, color-coded by behavior.
#' Ensures that lines are not drawn between discontinuous segments or across different behaviors.
#' Manages the legend so each behavior appears only once.
#'
#' @param rec_dt A `data.table` with columns `time`, `signal`, and `behavior`.
#' @param downsample_rate The rate to downsample the data (e.g., `10` for 10 Hz). Default is `10`.
#' @return A `plotly` object.
#' @import data.table
#' @import plotly
#' @export
#' @examples
#' \dontrun{
#' p <- plot_signal(rec_dt, downsample_rate = 10)
#' p
#' }
plot_signal <- function(rec_dt, downsample_rate = 10) {
  # Load required libraries
  library(data.table)
  library(plotly)

  # Downsample data
  rec_dt_ds <- rec_dt[seq(1, .N, by = downsample_rate)]

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
      x = ~time,
      y = ~signal,
      type = 'scatter',
      mode = 'lines',
      name = beh,
      line = list(color = behavior_colors[beh]),
      legendgroup = beh,
      showlegend = showlegend_flag,
      hoverinfo = 'text',
      text = ~paste('Time:', time, '<br>Signal:', signal, '<br>Behavior:', behavior_label)
    )
  }

  # Set layout
  p <- layout(
    p,
    title = 'Insect Electrical Behavior',
    xaxis = list(title = 'Time (s)'),
    yaxis = list(title = 'Signal (mV)'),
    legend = list(title = list(text = 'Behavior'))
  )

  return(p)
}
