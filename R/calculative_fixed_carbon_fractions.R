#' Calculate fixed carbon fractions
#'
#' @param deconvolution_output output from the pyrolysis phase deconvolution
#' @param dry_basis_fixed_carbon_hemicellulose reference value for proportion
#'   of pure hemicellulose sample that is fixed carbon
#' @param dry_basis_fixed_carbon_cellulose reference value for proportion
#'   of pure cellulose sample that is fixed carbon
#' @param dry_basis_fixed_carbon_lignin reference value for proportion
#'   of pure lignin sample that is fixed carbon
#'
#' @returns list of volatile hemicellulose fraction, volatile cellulose
#'   fraction, volatile lignin fraction, fixed carbon hemicellulose fraction,
#'   fixed carbon cellulose fraction, and fixed carbon lignin fraction
#' @export
#'
calculate_fixed_carbon_fractions <- function (
    deconvolution_output,
    dry_basis_fixed_carbon_hemicellulose = 13.8,
    dry_basis_fixed_carbon_cellulose = 6.4,
    dry_basis_fixed_carbon_lignin = 34) {

  df <- deconvolution_output$all_data
  m_t2 <- min(df$mass_T[df$stage == 'moisture_content'])
  m_t3 <- min(df$mass_T[df$stage == 'volatile_matter'])
  m_t6 <- min(df$mass_T[df$stage == 'fixed_carbon'])
  # fixed carbon
  FC <- 100 * ((m_t3-m_t6)/m_t2)

  # deconvolution of pyrolysis of volatile components
  vol_frac <- deconvolution_output$weights

  if ("HC_1" %in% names(vol_frac)) {
    H1vp <- vol_frac$HC_1[vol_frac$value_type == 'mean']
    H2vp <- vol_frac$HC_2[vol_frac$value_type == 'mean']
    Hvp <- H1vp + H2vp
  }
  if ('HC' %in% names(vol_frac)) {
    Hvp <- vol_frac$HC[vol_frac$value_type == 'mean']
  }
  Cvp <- vol_frac$CL[vol_frac$value_type == 'mean']
  Lvp <- vol_frac$LG[vol_frac$value_type == 'mean']
  # total volatile component
  V <- Hvp + Cvp + Lvp
  # partial fractions of each pseudocomponent in the volatile matter
  Hv1 <- Hvp/V
  Cv1 <- Cvp/V
  Lv1 <- Lvp/V

  # ash-free, dry basis proximate analysis
  Hf <- dry_basis_fixed_carbon_hemicellulose
  Cf <- dry_basis_fixed_carbon_cellulose
  Lf <- dry_basis_fixed_carbon_lignin
  # pseudo-component ratios in the fixed carbon
  Hf1 <- Hv1*Hf
  Cf1 <- Cv1*Cf
  Lf1 <- Lv1*Lf
  # sun of ratios in the fixed carbon
  F1 <- Hf1 + Cf1 + Lf1
  # partial fractions of pseudo-components in the fixed carbon
  Hf2 <- Hf1/F1
  Cf2 <- Cf1/F1
  Lf2 <- Lf1/F1
  # content of pseudo-components in the fixed carbon
  Hfp <- Hf2*FC
  Cfp <- Cf2*FC
  Lfp <- Lf2*FC

  list(volatile_HC_fraction = Hvp,
       volatile_CL_fraction = Cvp,
       volatile_LG_fraction = Lvp,
       fixed_carbon_HC_fraction = Hfp,
       fixed_carbon_CL_fraction = Cfp,
       fixed_carbon_LG_fraction = Lfp)
}
