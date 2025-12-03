#' Calculate moisture content as percent of total sample mass
#'
#' @param processed_data thermogravimetric mass loss data processed by
#'   process() function
#'
#' @returns moisture content %
#' @export
#'
calculate_moisture_content <- function (processed_data) {

  df <- processed_data$all_data
  m_t0 <- max(df$mass_T[df$stage == 'moisture_content'])
  m_t2 <- min(df$mass_T[df$stage == 'moisture_content'])
  100 * ((m_t0 - m_t2)/m_t0)

}

#' Calculate ash content as percent of dry mass of sample
#'
#' @param processed_data thermogravimetric mass loss data processed by
#'   process() function
#'
#' @returns ash content %
#' @export
#'
calculate_ash_content <- function (processed_data) {

  df <- processed_data$all_data
  m_t2 <- min(df$mass_T[df$stage == 'moisture_content'])
  m_t6 <- min(df$mass_T[df$stage == 'fixed_carbon'])
  100 * (m_t6/m_t2)

}
