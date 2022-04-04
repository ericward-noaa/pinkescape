#' fit_bycatch is the primary function for fitting bycatch models to time series of takes and effort
#' @param sims The number of simulations, defaults to 1000
#' @param time_steps The number of time steps, defaults to 100
#' @param pr_12 The probability of transitioning from state 1 to 2, defaults to 0.1
#' @param pr_21 The probability of transitioning from state 1 to 2, defaults to 0.1
#' @param run Whether to run this for the odd or even broodline, defaults to "odd"
#' @param deterministic_model Whether the deterministic escapement rule is used, defaults to TRUE
#' @param rec_std Recruitment variability (lognormal sd), defaults to 0.1
#' @param rec_acf Temporal autocorrelation of recruitment. Values of 1 = random walk, values of 0 = white noise. Defaults to 0.7
#' @param escapement_rule Can be "pre", "post", or "both" (default). If "pre" or "post", a single
#' escapement rule is used
#' @param discount_rate Optional, can also be done after the fact, but defaults to 0.1
#' @param seed Seed for random number generation, defaults to 123
#' @return data frame of simulations
#'
#' @export
#'
# bring up with group on next call: order of harvest v recruitment
# add variabiltiy by regime?
sim <- function(sims = 1000, # number of simulations
                time_steps = 100,
                pr_12 = 0.1,# probability transitioning 1:2
                pr_21 = 0.1,# probability transitioning 2:1
                run = "odd",
                deterministic_model = TRUE,
                rec_std = 0.1, # recruitment standard deviation in log space
                rec_acf = 0.7, # recruitment autocorrelation in time
                escapement_rule = "both",
                discount_rate = 0.1,
                seed = 123
) {

  ricker_pars <- data.frame("regime" = c("pre","post","pre","post"),
                            "regime_id" = c(1,2,1,2),
                            "run" = c("odd","odd","even","even"),
                            "a" = c(1.89763548,1.28512935,0.95639419,2.83733629),
                            "b" = c(-0.16520772,-0.04008856,-0.0937247,-0.47663675),
                            "S0" = c(3.962855, 3.962855, 3.675333,3.675333), # escapement in 2018-19
                            "S_star"=c(6.14274,14.8476,6.17917,2.59796), # escapement rule
                            "real_price" = c(2099586.443,2099586.443,2132653.372,2132653.372),
                            "cst_param_calib"=c(2132653.372,2132653.372,9435607.205,9435607.205))

  if(deterministic_model == FALSE) {
    ricker_pars$S_star = c(6.59548, 12.9706, 5.63686, 2.74081)
  }
  ricker_pars = ricker_pars[which(ricker_pars$run == run),]

  # add fixed escapement rules here
  if(escapement_rule == "pre") ricker_pars$S_star[2] = ricker_pars$S_star[1]
  if(escapement_rule == "post") ricker_pars$S_star[1] = ricker_pars$S_star[2]

  # transition matrix
  m = matrix(0, 2, 2)
  m[1,] = c(1-pr_12, pr_12)
  m[2,] = c(pr_21, 1-pr_21)

  # loop over iterations
  for(s in 1:sims) {

    set.seed(seed+s^2) # initial seed

    rec_dev = rnorm(1,0,sd=rec_std)    # recruitment deviations
    for(t in 2:time_steps) {
      rec_dev[t] = rnorm(1, mean = rec_acf * rec_dev[t-1], sd = rec_std)
    }

    # initial conditions in year 1, largely built off 2019
    x = sample(1:2,size=1)
    spawners = ricker_pars$S0[x[1]]
    rec = spawners * exp(ricker_pars$a[x[1]] + spawners*ricker_pars$b[x[1]])# recruitment first year, S0 * exp(a + bS0)
    harvest = max(rec - ricker_pars$S_star[x[1]], 0)
    net_benefits = 0 # needs to be update

    for(t in 2:time_steps) {
      x[t] = sample(1:2, size=1, prob = m[x[t-1],]) # simulate regime
      spawners[t] <- rec[t-1] - harvest[t-1] # talk to Dave about this!

      rec[t] <- spawners[t] * exp(ricker_pars$a[x[t]] + spawners[t]*ricker_pars$b[x[t]]) * exp(rec_dev[t] - 0.5*rec_std^2)
      harvest[t] <- max(rec[t] - ricker_pars$S_star[x[t]], 0)

      net_benefits[t] <- rec[t]*ricker_pars$real_price[x[t]] - ricker_pars$cst_param_calib[x[t]] * log(rec[t]) -
        (spawners[t]*ricker_pars$real_price[x[t]] - ricker_pars$cst_param_calib[x[t]] * log(spawners[t]))
      # add discount factor?
      # add discount factor * net_ben
    }

    # add discount
    discount = 1/((1+discount_rate)^(seq(1,time_steps)-1))

    df = data.frame(t = 1:time_steps,
                    regime = x,
                    spawners = spawners,
                    rec = rec,
                    rec_dev = rec_dev,
                    harvest = harvest,
                    net_benefits = net_benefits,
                    sim = s,
                    discount = discount)
    if(s==1) {
      all_df = df
    } else {
      all_df = rbind(df,all_df)
    }

  }
  return(all_df)

}



