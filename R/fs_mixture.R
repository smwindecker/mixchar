#' Fraser-Suzuki mixture model
#'
#' @param temp temperature values
#' @param height_1 height value for hemicellulose
#' @param skew_1 shape value for hemicellulose
#' @param position_1 position value for hemicellulose
#' @param width_1 width value for hemicellulose
#' @param height_2 height value for cellulose
#' @param skew_2 shape value for cellulose
#' @param position_2 position value for cellulose
#' @param width_2 width value for cellulose
#' @param height_3 height value for lignin
#' @param skew_3 shape value for lignin
#' @param position_3 position value for lignin
#' @param width_3 width value for lignin
#' @param height_0 height value for second hemicellulose curve, if present
#' @param skew_0 shape value for second hemicellulose curve, if present
#' @param position_0 position value for second hemicellulose curve, if present
#' @param width_0 width value for second hemicellulose curve, if present
#' @return Fraser-Suzuki model output
#' @examples
#' temp <- 150:600
#' fs_mixture_output <- fs_mixture(temp,
#' height_1 = 0.003, skew_1 = -0.15, position_1 = 250, width_1 = 50,
#' height_2 = 0.006, skew_2 = -0.15, position_2 = 320, width_2 = 30,
#' height_3 = 0.001, skew_3 = -0.15, position_3 = 390, width_3 = 200)
#'
#' @export

fs_mixture <- function (temp,
                        height_1, skew_1, position_1, width_1,
                        height_2, skew_2, position_2, width_2,
                        height_3, skew_3, position_3, width_3,
                        height_0 = NULL, skew_0 = NULL,
                        position_0 = NULL, width_0 = NULL) {

  n_params <- length(c(height_1, skew_1, position_1, width_1,
                       height_2, skew_2, position_2, width_2,
                       height_3, skew_3, position_3, width_3,
                       height_0, skew_0, position_0, width_0))

  if (n_params != 12 & n_params != 16) {
    stop('Specify correct number of parameters')
  }

  output <- fs_function(temp, height_1, skew_1, position_1, width_1) +
    fs_function(temp, height_2, skew_2, position_2, width_2) +
    fs_function(temp, height_3, skew_3, position_3, width_3)

  if (!is.null(height_0) & !is.null(skew_0) &
      !is.null(position_0) & !is.null(width_0)) {
    output <- output + fs_function(temp,
                                   height_0,
                                   skew_0,
                                   position_0,
                                   width_0)
  }

  output
}

