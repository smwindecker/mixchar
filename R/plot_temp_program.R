#' Plot temperature program over time
#'
#' Draw a temperature versus time profile with shaded bands for each program
#' stage. The default layout matches the six-stage program supplied in
#' `beech`, but any dataset with time, temperature, and stage columns can be
#' used.
#'
#' @param data data.frame containing the temperature program.
#' @param time_col column name for time (in minutes); defaults to `time`.
#' @param temp_col column name for temperature (in Celsius). If `NULL`, the
#'   function looks for `temp_C` and then `temp`.
#' @param stage_col column name that identifies stage membership.
#' @param stage_labels labels to show beneath each stage band. Defaults to
#'   Roman numerals ordered by stage start time.
#' @param stage_colors fill colours for stage bands. Recycled if fewer colours
#'   are provided than stages.
#' @param bg_alpha alpha applied to the stage fill colours.
#' @param line_col colour for the temperature trace.
#' @param line_lwd line width for the temperature trace.
#' @param label_cex scaling for the stage labels.
#' @param draw_boundaries logical; if `TRUE`, dashed lines are drawn at stage
#'   boundaries.
#' @param main_title title placed above the plot. Set to `""` to suppress.
#' @param ... additional arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns a data.frame with start/end times and
#'   temperature ranges for each stage.
#'
#' @examples
#' data(beech)
#' plot_temp_program(beech)
#'
#' @export
plot_temp_program <- function(data,
                              time_col = "time",
                              temp_col = NULL,
                              stage_col = "stage",
                              stage_labels = NULL,
                              stage_colors = NULL,
                              bg_alpha = 0.55,
                              line_col = "black",
                              line_lwd = 2,
                              label_cex = 1,
                              draw_boundaries = TRUE,
                              main_title = "Temperature Program (TGA)",
                              ...) {

  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  available <- names(data)
  if (is.null(temp_col)) {
    if ("temp_C" %in% available) {
      temp_col <- "temp_C"
    } else if ("temp" %in% available) {
      temp_col <- "temp"
    } else {
      stop("temp_col is NULL and neither 'temp_C' nor 'temp' were found.")
    }
  }

  required <- c(time_col, temp_col, stage_col)
  missing_cols <- setdiff(required, available)
  if (length(missing_cols) > 0) {
    stop("data is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- data[, required]
  names(df) <- c("time", "temp", "stage")
  df <- df[stats::complete.cases(df), ]

  if (nrow(df) == 0) {
    stop("no complete rows available after removing missing values.")
  }

  df <- df[order(df$time), ]

  stage_order <- unique(df$stage)
  stage_order <- stage_order[order(vapply(stage_order, function(stg) {
    min(df$time[df$stage == stg], na.rm = TRUE)
  }, numeric(1)))]

  stage_summaries <- lapply(stage_order, function(stg) {
    sub <- df[df$stage == stg, , drop = FALSE]
    data.frame(
      stage = stg,
      time_start = min(sub$time, na.rm = TRUE),
      time_end = max(sub$time, na.rm = TRUE),
      temp_min = min(sub$temp, na.rm = TRUE),
      temp_max = max(sub$temp, na.rm = TRUE)
    )
  })
  stage_df <- do.call(rbind, stage_summaries)

  if (is.null(stage_labels)) {
    stage_labels <- as.character(utils::as.roman(seq_along(stage_order)))
  }

  # pastel palette aligned with reference figure (I–VI)
  base_cols <- c("#9DC8E2", "#C9EBC7", "#E8D5B4",
                 "#F3C7BF", "#E5C2E6", "#B7CCE8")
  if (is.null(stage_colors)) {
    if (length(stage_order) <= length(base_cols)) {
      stage_colors <- base_cols[seq_along(stage_order)]
    } else {
      stage_colors <- grDevices::colorRampPalette(base_cols)(length(stage_order))
    }
  } else if (length(stage_colors) < length(stage_order)) {
    stage_colors <- rep(stage_colors, length.out = length(stage_order))
  }

  y_range <- range(df$temp, na.rm = TRUE)
  y_span <- diff(y_range)
  if (y_span == 0) {
    y_span <- max(1, abs(y_range[1]) * 0.05)
  }
  y_pad <- y_span * 0.05
  y_bottom <- y_range[1] - y_pad
  y_top_plot <- y_range[2] + y_pad
  label_offset <- y_span * 0.12
  y_top_ext <- y_top_plot + label_offset

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  x_ticks <- seq(0, ceiling(max(df$time, na.rm = TRUE) / 50) * 50, by = 50)
  y_ticks <- seq(floor(y_bottom / 100) * 100,
                 ceiling(y_top_plot / 100) * 100,
                 by = 100)

  xlim <- range(c(df$time, x_ticks), na.rm = TRUE)

  graphics::plot(df$time, df$temp,
                 type = "n",
                 xlab = "Time [min]",
                 ylab = "Temperature [℃]",
                 xlim = xlim,
                 ylim = c(y_bottom, y_top_ext),
                 axes = FALSE,
                 main = "",
                 ...)

  if (!is.null(main_title) && nzchar(main_title)) {
    graphics::title(main = main_title, line = 1)
  }

  graphics::axis(1, at = x_ticks, labels = x_ticks)
  graphics::axis(2, at = y_ticks, labels = y_ticks, las = 1)
  graphics::box()

  for (i in seq_along(stage_order)) {
    graphics::rect(
      xleft = stage_df$time_start[i],
      xright = stage_df$time_end[i],
      ybottom = y_bottom,
      ytop = y_top_plot,
      col = grDevices::adjustcolor(stage_colors[i], alpha.f = bg_alpha),
      border = NA
    )
  }

  if (isTRUE(draw_boundaries) && nrow(stage_df) > 1) {
    boundaries <- stage_df$time_end[-nrow(stage_df)]
    graphics::segments(boundaries, y_bottom, boundaries, y_top_plot,
                       col = "grey50", lty = 3)
  }

  graphics::lines(df$time, df$temp, col = line_col, lwd = line_lwd)

  label_height <- y_top_plot + label_offset * 0.6
  for (i in seq_along(stage_order)) {
    center_x <- mean(c(stage_df$time_start[i], stage_df$time_end[i]))
    graphics::text(center_x, label_height,
                   labels = stage_labels[i],
                   cex = label_cex)
  }

  invisible(stage_df)
}
