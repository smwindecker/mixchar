# Function to determine x-value of peak < 500 K
#
# @param x x-values
# @param y y-values
# @keywords internal
# @import zoo
## script cred https://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset

inflection <- function(x, y, w=1, ...) {
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}
