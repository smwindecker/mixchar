#' Calculate mg/mg value
#'
#' This function produces a list of the final mg pseudo-component per
#' mg of sample for each of the three components.
#'
#' @param fit model fit output from deconvolve function
#' @param lower lower bound for temperature for cropping dataset
#' @param upper upper bound for temperature for cropping dataset
#' @param mass_init numeric value of mg of initial sample
#' @param mass_loss_vec vector of mass loss values from TGA
#' @return list of mg pseudo-component / mg sample for pseudo-components
#' 1, 2, and 3
#' @keywords thermogravimetry fraser-suzuki deconvolution
#' @importFrom stats integrate setNames
#' @export

#t <- mass_fractions(model, 400, 900, 18.9615, test$m_T)

mass_fractions <- function (fit, lower, upper, mass_init, mass_loss_vec) {

  W <- mass_init + mass_loss_vec
  n <- length(W)

  # get the proportions
  a_j <- vector(length = 3)

  for (j in 1:3) {

    f_j <- function (x) {

      h <- single_param(fit, 'h', j)
      s <- single_param(fit, 's', j)
      p <- single_param(fit, 'p', j)
      w <- single_param(fit, 'w', j)

      fs_function(x, h, s, p, w)

    }

    a_j[j] <- integrate(Vectorize(f_j), lower = lower,
                         upper = upper)$value / (W[1] - W[n])

  }

  mg_p1 <- a_j[1] * (W[1] - W[n]) / mass_init
  mg_p2 <- a_j[2] * (W[1] - W[n]) / mass_init
  mg_p3 <- a_j[3] * (W[1] - W[n]) / mass_init

  mass_frac <- setNames(as.list(c(mg_p1, mg_p2, mg_p3)), c('pc1', 'pc2', 'pc3'))

  return(mass_frac)

}


