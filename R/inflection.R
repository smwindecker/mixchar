#' Function to convert inflection point to logical argument for presence of fourth peak
#'
#' @param x numeric
#' @keywords internal

three_peaks <- function (x) {
  return (length(x) == 0)
}

#' Function to determine x-value of peak < 500 K
#'
#' @param x x-values
#' @param y y-values
#' @keywords internal
#' @return returns a list of values. $x returns the inflections points within x.
#' @importFrom zoo rollapply zoo
#' @importFrom stats loess
## script cred https://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset

inflection <- function(x, y, w = 1, ...) {
  n <- length(y)
  y.smooth <- stats::loess(y ~ x, ...)$fitted
  y.max <- zoo::rollapply(zoo::zoo(y.smooth),
                          2*w+1,
                          max,
                          align = "center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x = x[i.max], i = i.max, y.hat = y.smooth)
}
