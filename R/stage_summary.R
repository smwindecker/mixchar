#' Summarise stage start/end time and mass
#'
#' @param process_obj object returned from [process()]
#' @return data.frame with stage, time_start/time_end, mass_start/mass_end
#' @export
stage_summary <- function(process_obj) {

  if (is.null(process_obj$all_data)) {
    stop("stage_summary() expects the output of process().")
  }

  df <- process_obj$all_data
  needed <- c("time", "mass_T", "stage")
  missing <- setdiff(needed, names(df))

  if (length(missing) > 0) {
    stop("process_obj$all_data is missing columns: ",
         paste(missing, collapse = ", "))
  }

  # known stage order; include any extras at the end
  stage_order <- c("moisture_content", "volatile_matter", "fixed_carbon")
  extra_stages <- setdiff(unique(df$stage), stage_order)
  stage_order <- c(stage_order, extra_stages)

  stage_rows <- lapply(stage_order, function(stg) {
    sub <- df[df$stage == stg, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)

    sub <- sub[order(sub$time), ]
    data.frame(
      stage = stg,
      time_start = sub$time[1],
      time_end = sub$time[nrow(sub)],
      mass_start = sub$mass_T[1],
      mass_end = sub$mass_T[nrow(sub)],
      row.names = NULL
    )
  })

  res <- do.call(rbind, stage_rows)

  if (is.null(res)) {
    res <- data.frame(
      stage = character(),
      time_start = numeric(),
      time_end = numeric(),
      mass_start = numeric(),
      mass_end = numeric()
    )
  }

  res
}
