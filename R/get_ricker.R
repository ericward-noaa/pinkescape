#' get_ricker returns the default parameters of the Ricker simulations
#' @return data frame of default parameters for simulations
#'
#' @export
#'
get_ricker = function() {
  ricker_pars <- data.frame("regime" = c("pre","post","pre","post"),
                            "regime_id" = c(1,2,1,2),
                            "run" = c("odd","odd","even","even"),
                            "a" = c(1.89763548,1.28512935,0.95639419,2.83733629),
                            "b" = c(-0.16520772,-0.04008856,-0.0937247,-0.47663675),
                            "S0" = c(3.962855, 3.962855, 3.675333,3.675333), # escapement in 2018-19
                            "S_star"=c(6.14274,14.8476,6.17917,2.59796), # escapement rule
                            "real_price" = c(2099586.443,2099586.443,2132653.372,2132653.372), # price in USD per million fish
                            "cst_param_calib"=c(13140389.91,13140389.91,9435607.205,9435607.205), # based on million fish
                            "S_star_stochastic" = c(6.59548, 12.9706, 5.63686, 2.74081))
  return(ricker_pars)
}
