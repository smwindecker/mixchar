# Non-linear model using Fraser-Suzuki mixture model
#
# Non-linear model output using optimised parameter values
# with a three-part mixture model using Fraser-Suzuki equation
#
# @param dataframe dataframe
# @param params starting parameter values
# @return model output
# @keywords internal
# @import minpack.lm

# function to do the nls fit with the correct starting values
fs_model_4 <- function (dataframe, params, lb, ub) {

  nlsLM(deriv ~ fs_mixture_wrap_4(temp_C, h1, h2, h3, h4, s1, s2, s3, s4, p1, p2, p3, p4, w1, w2, w3, w4),
        start = list(h1 = params[1], h2 = params[2], h3 = params[3], h4 = params[4],
                     s1 = params[5], s2 = params[6], s3 = params[7], s4 = params[8],
                     p1 = params[9], p2 = params[10], p3 = params[11], p4 = params[12],
                     w1 = params[13], w2 = params[14], w3 = params[15], w4 = params[16]),
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, maxfev = 1e6),
        lower = lb,
        upper = ub)

}
