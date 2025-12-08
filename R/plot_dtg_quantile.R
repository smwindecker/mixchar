#' Plot DTG curve at a chosen quantile (or mean) with component curves
#'
#' @param decon_obj deconvolve() output
#' @param prob Quantile, values such as 0.5/0.025/0.975; when set to “mean,” estimated using fitted points
#' @param n_draws Number of samples
#' @param bw TRUE Black and white, FALSE Color
#' @param seed Random seed
#' @export
#' @importFrom stats quantile
#' @importFrom graphics plot lines legend

plot_dtg_quantile <- function(decon_obj,
                              prob = 0.5,
                              n_draws = 500,
                              bw = FALSE,
                              seed = NULL) {

  temp <- seq(decon_obj$pyrolysis_temp_range[1],
              decon_obj$pyrolysis_temp_range[2],
              length.out = nrow(decon_obj$pyrolysis_data))
  data   <- decon_obj$pyrolysis_data
  fit    <- decon_obj$model_fit
  p_est  <- stats::coef(fit)
  n_peaks <- decon_obj$n_peaks

  if (n_peaks != 3) {
    stop("plot_dtg_quantile currently implemented for n_peaks = 3.")
  }

  if (identical(prob, "mean")) {
    cur_total <- fs_mixture(
      temp,
      p_est["height_1"], p_est["skew_1"], p_est["position_1"], p_est["width_1"],
      p_est["height_2"], p_est["skew_2"], p_est["position_2"], p_est["width_2"],
      p_est["height_3"], p_est["skew_3"], p_est["position_3"], p_est["width_3"]
    )

    comps <- list(
      fs_function(temp, p_est["height_1"], p_est["skew_1"],
                  p_est["position_1"], p_est["width_1"]),
      fs_function(temp, p_est["height_2"], p_est["skew_2"],
                  p_est["position_2"], p_est["width_2"]),
      fs_function(temp, p_est["height_3"], p_est["skew_3"],
                  p_est["position_3"], p_est["width_3"])
    )

  } else {
    sry      <- summary(fit)
    residvar <- stats::deviance(fit) / sry$df[2]
    vcov     <- sry$cov.unscaled * residvar

    lower <- c(0, -Inf, 0, 0,
               0, -Inf, 0, 0,
               0, -Inf, 0, 0)
    upper <- rep(Inf, 12)

    if (!is.null(seed)) set.seed(seed)

    draws <- tmvtnorm::rtmvnorm(
      n_draws, mean = p_est, sigma = vcov,
      lower = lower, upper = upper
    )

    total_mat <- matrix(NA_real_, n_draws, length(temp))
    comp_arr  <- array(NA_real_, dim = c(n_draws, length(temp), 3))

    for (i in seq_len(n_draws)) {
      total_mat[i, ] <- fs_mixture(
        temp,
        draws[i, 1], draws[i, 2], draws[i, 3], draws[i, 4],
        draws[i, 5], draws[i, 6], draws[i, 7], draws[i, 8],
        draws[i, 9], draws[i,10], draws[i,11], draws[i,12]
      )

      comp_arr[i, , 1] <- fs_function(
        temp, draws[i,1], draws[i,2], draws[i,3], draws[i,4]
      )
      comp_arr[i, , 2] <- fs_function(
        temp, draws[i,5], draws[i,6], draws[i,7], draws[i,8]
      )
      comp_arr[i, , 3] <- fs_function(
        temp, draws[i,9], draws[i,10], draws[i,11], draws[i,12]
      )
    }

    cur_total <- apply(total_mat, 2, quantile, probs = prob, na.rm = TRUE)
    comps <- list(
      apply(comp_arr[ , , 1], 2, quantile, probs = prob, na.rm = TRUE),
      apply(comp_arr[ , , 2], 2, quantile, probs = prob, na.rm = TRUE),
      apply(comp_arr[ , , 3], 2, quantile, probs = prob, na.rm = TRUE)
    )
  }

  ltys <- c(3, 4, 5)

  if (bw) {
    col_data  <- "black"
    col_total <- "black"
    col_comps <- rep("black", 3)
    lwd_total <- 1.7
    lwd_comp  <- 2
  } else {
    col_data  <- "black"
    col_total <- "black"
    col_comps <- c("#440154FF", "#B8DE29FF", "#3CBB75FF")
    lwd_total <- 2
    lwd_comp  <- 3.5
  }

  if (identical(prob, "mean")) {
    prob_lab <- "(mean)"
  } else {
    prob_lab <- sprintf("(%g%%)", as.numeric(prob) * 100)
  }

  par(mar = c(5, 5, 1, 1))
  plot(data$temp_C, data$deriv,
       xlab = "Temperature (C)",
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
       yaxs = "i",
       ylim = c(0, max(data$deriv) + 0.06 * max(data$deriv)),
       pch = 20,
       cex = 0.5,
       cex.axis = 1.2,
       cex.lab  = 1.5)

  lines(temp, cur_total,
        lty = 1,
        lwd = lwd_total,
        col = col_total)

  for (j in seq_len(3)) {
    lines(temp, comps[[j]],
          lty = ltys[j],
          lwd = lwd_comp,
          col = col_comps[j])
  }

  legend_labels <- c(
    "DTG data",
    paste0("DTG model ", prob_lab),
    paste0("Hemicellulose ",  prob_lab),
    paste0("Cellulose ",      prob_lab),
    paste0("Lignin ",         prob_lab)
  )

  if (bw) {
    legend_cols <- rep("black", 5)
  } else {
    legend_cols <- c("black", "black", col_comps)
  }

  legend("topright",
         legend = legend_labels,
         ncol   = 1,
         cex    = 1.2,
         bty    = "n",
         lty    = c(NA, 1, ltys),
         pch    = c(20, NA, NA, NA, NA),
         col    = legend_cols,
         lwd    = 2)
}
