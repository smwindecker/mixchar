

get_weights <- function (param_vec, output) {

  n_peaks <- output$n_peaks
  lower_temp <- output$bounds[1]
  upper_temp <- output$bounds[2]

  if (n_peaks == 3) {
    wt_percent <- c('HC' = NA, 'CL' = NA, 'LG' = NA)
  }
  if (n_peaks == 4) {
    wt_percent <- c('HC_1' = NA, 'HC_2' = NA, 'CL' = NA, 'LG' = NA)
  }

  # get the proportions of the pseudo-components
  for (j in 1:n_peaks) {

    # extract relevant parameter vector
    names <- paste0(c("h", "s", "p", "w"), j)
    idx <- match(names, names(param_vec))
    param_sub <- param_vec[idx]

    f_j <- function (x) {

      fs_function(x, param_sub[1], param_sub[2], param_sub[3], param_sub[4])

    }

    # weight percent of each component, where the integral is the fraction
    # of initial mass of that compoenent.
    wt_percent[j] <- integrate(Vectorize(f_j), lower = lower_temp,
                               upper = upper_temp)$value * 100

  }

  wt_percent
}
