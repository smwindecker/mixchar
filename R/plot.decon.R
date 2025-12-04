#' Default S3 plot method for decon objects (derived from `deconvolve()`)
#' @param show_quantile_band logical; add uncertainty ribbons from posterior draws
#' @param quantile_probs vector; lower/median/upper probs, e.g. c(0.025,0.5,0.975)
#' @param n_draws number of draws for quantile ribbons
#' @param seed optional seed for reproducibility
#' @param band_alpha alpha for ribbons
#' @importFrom grDevices adjustcolor
#' @method plot decon
plot.decon <- function (x,
                        bw = TRUE,
                        show_quantile_band = FALSE,
                        quantile_probs = c(0.025, 0.5, 0.975),
                        n_draws = 500,
                        seed = NULL,
                        band_alpha = 0.20,
                        ...) {

  temp <- seq(x$pyrolysis_temp_range[1], x$pyrolysis_temp_range[2],
              length.out = nrow(x$pyrolysis_data))
  data <- x$pyrolysis_data
  fit <- x$model_fit
  params <- as.data.frame(summary(fit)$coefficients[,1])
  p_est <- stats::coef(fit)

  quant_total <- NULL
  quant_comps <- NULL
  probs_sorted <- sort(unique(quantile_probs))
  if (isTRUE(show_quantile_band) && x$n_peaks == 3) {
    if (!is.null(seed)) set.seed(seed)
    sry <- summary(fit)
    residvar <- stats::deviance(fit) / sry$df[2]
    vcov <- sry$cov.unscaled * residvar

    lower <- rep(c(0, -Inf, 0, 0), 3)
    upper <- rep(Inf, length(lower))

    draws <- tmvtnorm::rtmvnorm(n_draws, mean = p_est, sigma = vcov,
                                lower = lower, upper = upper)

    total_mat <- matrix(NA_real_, n_draws, length(temp))
    comp_arr  <- array(NA_real_, dim = c(n_draws, length(temp), 3))

    for (i in seq_len(n_draws)) {
      total_mat[i, ] <- fs_mixture(
        temp,
        draws[i, 1], draws[i, 2], draws[i, 3], draws[i, 4],
        draws[i, 5], draws[i, 6], draws[i, 7], draws[i, 8],
        draws[i, 9], draws[i,10], draws[i,11], draws[i,12]
      )
      comp_arr[i, , 1] <- fs_function(temp, draws[i,1], draws[i,2], draws[i,3], draws[i,4])
      comp_arr[i, , 2] <- fs_function(temp, draws[i,5], draws[i,6], draws[i,7], draws[i,8])
      comp_arr[i, , 3] <- fs_function(temp, draws[i,9], draws[i,10], draws[i,11], draws[i,12])
    }

    quant_total <- apply(total_mat, 2, stats::quantile,
                         probs = probs_sorted, na.rm = TRUE)
    rownames(quant_total) <- as.character(probs_sorted)

    quant_comps <- lapply(1:3, function(j) {
      out <- apply(comp_arr[ , , j], 2, stats::quantile,
                   probs = probs_sorted, na.rm = TRUE)
      rownames(out) <- as.character(probs_sorted)
      out
    })
  }

  par(mar = c(5, 5, 1, 1))
  plot(data$temp_C, data$deriv, xlab = 'Temperature (C)',
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
       yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       pch = 20, cex = 0.5, cex.axis = 1.2, cex.lab = 1.5)

  if (!is.null(quant_total)) {
    prob_lo <- as.character(probs_sorted[1])
    prob_hi <- as.character(probs_sorted[length(probs_sorted)])
    col_total_band <- grDevices::adjustcolor(if (bw) "grey50" else "black",
                                             alpha.f = band_alpha)
    polygon(c(temp, rev(temp)),
            c(quant_total[prob_lo, ],
              rev(quant_total[prob_hi, ])),
            border = NA, col = col_total_band)

    band_cols <- if (bw) rep(grDevices::adjustcolor("grey70", band_alpha), 3) else
      grDevices::adjustcolor(c('#440154FF', '#B8DE29FF', '#3CBB75FF'), band_alpha)
    for (j in 1:3) {
      polygon(c(temp, rev(temp)),
              c(quant_comps[[j]][prob_lo, ],
                rev(quant_comps[[j]][prob_hi, ])),
              border = NA, col = band_cols[j])
    }
  }

  if (isTRUE(bw)) {

    if (x$n_peaks == 4) {
      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',],
                       height_0 = params['height_0',],
                       skew_0 = params['skew_0',],
                       position_0 = params['position_0',],
                       width_0 = params['width_0',])

      y5 <- fs_function(temp,
                        params['height_0',], params['skew_0',],
                        params['position_0',], params['width_0',])
      lines(temp, y5, lty = 5, lwd = 2)

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose 1', 'Hemicellulose 2',
                        'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 6, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA, NA),
             lwd = 2)

    }

    if (x$n_peaks == 3) {

      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',])

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose', 'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA),
             lwd = 2)

    }

    y2 <- fs_function(temp,
                      params['height_1',], params['skew_1',],
                      params['position_1',], params['width_1',])
    y3 <- fs_function(temp,
                      params['height_2',], params['skew_2',],
                      params['position_2',], params['width_2',])
    y4 <- fs_function(temp,
                      params['height_3',], params['skew_3',],
                      params['position_3',], params['width_3',])

    lines(temp, y1, lty = 1, lwd = 1.7)
    lines(temp, y2, lty = 3, lwd = 2)
    lines(temp, y3, lty = 4, lwd = 2)
    lines(temp, y4, lty = 5, lwd = 2)

  }

  if (!isTRUE(bw)) {

    if (x$n_peaks == 4) {
      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',],
                       height_0 = params['height_0',],
                       skew_0 = params['skew_0',],
                       position_0 = params['position_0',],
                       width_0 = params['width_0',])

      y5 <- fs_function(temp,
                        params['height_0',], params['skew_0',],
                        params['position_0',], params['width_0',])

      lines(temp, y5, lty = 6, lwd = 2.5, col = '#33638DFF')

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose 1', 'Hemicellulose 2',
                        'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 6, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA, NA),
             col = c('black', 'black', '#33638DFF',
                     '#440154FF', '#B8DE29FF', '#3CBB75FF'),
             lwd = 2)

    }

    if (x$n_peaks == 3) {

      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',])

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose', 'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA),
             col = c('black', 'black', '#440154FF',
                     '#B8DE29FF', '#3CBB75FF'),
             lwd = 2)

    }

    y2 <- fs_function(temp,
                      params['height_1',], params['skew_1',],
                      params['position_1',], params['width_1',])
    y3 <- fs_function(temp,
                      params['height_2',], params['skew_2',],
                      params['position_2',], params['width_2',])
    y4 <- fs_function(temp,
                      params['height_3',], params['skew_3',],
                      params['position_3',], params['width_3',])

    lines(temp, y1, lty = 1, lwd = 2)
    lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
    lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
    lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')

  }
}
