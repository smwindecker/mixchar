# Fraser-Suzuki three-part mixture
#
# This function combines three Fraser-Suzuki functions for the total mixture model.
#
# @param x temperature values
# @param params parameter starting values
# @return Fraser-Suzuki mixture model output
# @keywords internal

# combine the FS function three times for the three pseudo-components
fs_mixture_4 <- function (x, params) {

  fs_mixture_function <- fs_function(x, params[1], params[5], params[9], params[13]) +
    fs_function(x, params[2], params[6], params[10], params[14]) +
    fs_function(x, params[3], params[7], params[11], params[15]) +
    fs_function(x, params[4], params[8], params[12], params[16])

}
