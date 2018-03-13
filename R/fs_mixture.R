#' Mixture model for TGA data
#'
#' @param x temperature values
#' @param h1 height value for curve 1
#' @param s1 shape value for curve 1
#' @param p1 position value for curve 1
#' @param w1 width value for curve 1
#' @param h2 height value for curve 2
#' @param s2 shape value for curve 2
#' @param p2 position value for curve 2
#' @param w2 width value for curve 2
#' @param h3 height value for curve 3
#' @param s3 shape value for curve 3
#' @param p3 position value for curve 3
#' @param w3 width value for curve 3
#' @param h4 height value for curve 4, if present
#' @param s4 shape value for curve 4, if present
#' @param p4 position value for curve 4, if present
#' @param w4 width value for curve 4, if present
#' @return Fraser-Suzuki model output
#'
#' @export

fs_mixture <- function (x, h1, s1, p1, w1,
                        h2, s2, p2, w2,
                        h3, s3, p3, w3,
                        h4 = NULL, s4 = NULL, p4 = NULL, w4 = NULL) {

  output <- fs_function(x, h1, s1, p1, w1) +
    fs_function(x, h2, s2, p2, w2) +
    fs_function(x, h3, s3, p3, w3)

  if (!is.null(h4) & !is.null(s4) & !is.null(p4) & !is.null(w4)) {
    output <- output + fs_function(x, h4, s4, p4, w4)
  }

  output
}
