#' Wrapper for Fraser-Suzuki mixture model
#'
#' This function is a wrapper for the fsTotal function that modifies so it uses all 12 parameters as input.
#'
#' @param x temperature values
#' @param h1 height value pc1
#' @param h2 height value pc2
#' @param h3 height value pc3
#' @param s1 shape value pc1
#' @param s2 shape value pc2
#' @param s3 shape value pc3
#' @param p1 position value pc1
#' @param p2 position value pc2
#' @param p3 position value pc3
#' @param w1 width value pc1
#' @param w2 width value pc2
#' @param w3 width value pc3
#' @return three-part Fraser-Suzuki mixture model output
#' @keywords internal

# create wrapper to separately identify the 12 parameters
fs_mixture_wrap <- function (x, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3) {

  params <- c(h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3)
  deconvolve:::fs_mixture(x, params)

}
