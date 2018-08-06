#' Non-linear model using Fraser-Suzuki mixture model
#'
#' Non-linear model output using optimised parameter values
#' with a three-part mixture model using Fraser-Suzuki equation
#'
#' @param dataframe dataframe
#' @param params starting parameter values
#' @param lb lower bounds for model
#' @param ub upper bounds for model
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @return model output
#'
#' @export

fs_model <- function (dataframe, params, lb, ub) {

  if (length(params) == 12) {

    frm <- deriv ~ fs_mixture(temp_C,
                              height_1, skew_1, position_1, width_1,
                              height_2, skew_2, position_2, width_2,
                              height_3, skew_3, position_3, width_3)

    start_list <- list(height_1 = params[1], skew_1 = params[2],
                       position_1 = params[3], width_1 = params[4],
                       height_2 = params[5], skew_2 = params[6],
                       position_2 = params[7], width_2 = params[8],
                       height_3 = params[9], skew_3 = params[10],
                       position_3 = params[11], width_3 = params[12])

  }

  if (length(params) == 16) {

    frm <- deriv ~ fs_mixture(temp_C,
                              height_1, skew_1, position_1, width_1,
                              height_2, skew_2, position_2, width_2,
                              height_3, skew_3, position_3, width_3,
                              height_0, skew_0, position_0, width_0)

    start_list <- list(height_0 = params[1], skew_0 = params[2],
                       position_0 = params[3], width_0 = params[4],
                       height_1 = params[5], skew_1 = params[6],
                       position_1 = params[7], width_1 = params[8],
                       height_2 = params[9], skew_2 = params[10],
                       position_2 = params[11], width_2 = params[12],
                       height_3 = params[13], skew_3 = params[14],
                       position_3 = params[15], width_3 = params[16])

  }

  minpack.lm::nlsLM(frm,
                    start = start_list,
                    data = dataframe,
                    control = minpack.lm::nls.lm.control(maxiter = 1024,
                                                         maxfev = 1e6),
                    lower = lb,
                    upper = ub)

}
