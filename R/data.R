#' Thermogravimetric analysis of beech tree data
#'
#' TGA of beech tree sample executed over a 6 stage temperature program. Stage 1: drying phase with dynamic heating of the sample (10C/min) to a drying temperature of 105C under a nitrogen atmosphere. Stage 2: isothermal drying of the sample for 120 minutes at 105C under a nitrogen atmosphere. Stage 3: dynamic heating (pyrolysis phase) of the sample (10C/min) to 750C under a nitrogen atmosphere. Stage 4: Dynamic cooling of the sample (10C/min) to 600C under a nitrogen atmosphere. Stage 5: dynamic heating (combustion phase) of the sample (2.3C/min) to 815C in air. Stage 6: Isothermal combustion (ashing phase) of the sample for 60 minutes to 815C in air.
#'
#' @format ## `beech`
#' A data frame with 1544 rows and 4 columns:
#' \describe{
#'   \item{time}{Numeric, time in minutes}
#'   \item{temp}{Numeric, temperature in degrees C}
#'   \item{mass_loss}{Numeric, mass loss compared to an initial mass of 10.64}
#'   \item{stage}{Integer, stage of thermogravimetric analysis}
#' }
#' @source <>
#' @srrstats {G5.1} Dataset `beech` is exported and described in detail here.
#'   It is used in both example code and in testing.
#' @srrstats {G1.4} uses `Roxygen2` documentation
"beech"

#' Thermogravimetric analysis of juncus reed data
#'
#' TGA of juncus reed sample under a single a dynamic heating protocol (10C/min) from XXXX to 750C under a nitrogen atmosphere.
#'
#' @format ## `juncus`
#' A data frame with 46080 rows and 2 columns:
#' \describe{
#'   \item{temp_C}{Numeric, temperature in degrees C}
#'   \item{mass_loss}{Numeric, mass loss compared to an initial mass of 18.96}
#' }
#' @source <>
#' @srrstats {G5.1} Dataset `juncus` is exported and described in detail here.
#'   It is used in both example code and in testing.
#' @srrstats {G1.4} uses `Roxygen2` documentation
"juncus"

#' Thermogravimetric analysis of marsilea herb data
#'
#' TGA of marsilea herb sample under a single a dynamic heating protocol (10C/min) from XXXX to 750C under a nitrogen atmosphere.
#'
#' @format ## `marsilea`
#' A data frame with 46080 rows and 2 columns:
#' \describe{
#'   \item{temp_C}{Numeric, temperature in degrees C}
#'   \item{mass_loss}{Numeric, mass loss compared to an initial mass of 15.29}
#' }
#' @source <>
#' @srrstats {G5.1} Dataset `marsilea` is exported and described in detail here.
#'   It is used in both example code and in testing.
#' @srrstats {G1.4} uses `Roxygen2` documentation
"marsilea"
