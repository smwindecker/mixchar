calculate_total_fractions <- function (deconvolution_output,
                                       fixed_carbon_fractions) {

  vol_frac <- deconvolution_output$weights

  # resulting final content of each pseudo-component is the sum of their
  # quantities from deconvolution and the quantities transferred to fixed carbon
  Hvp <- vol_frac$HC[vol_frac$value_type == 'mean']
  Cvp <- vol_frac$CL[vol_frac$value_type == 'mean']
  Lvp <- vol_frac$LG[vol_frac$value_type == 'mean']

  Hfp <- fixed_carbon_fractions$Hfp
  Cfp <- fixed_carbon_fractions$Cfp
  Lfp <- fixed_carbon_fractions$Lfp

  H_total <- Hvp + Hfp
  C_total <- Cvp + Cfp
  L_total <- Lvp + Lfp

  list(H_total = H_total, C_total = C_total, L_total = L_total)
}
