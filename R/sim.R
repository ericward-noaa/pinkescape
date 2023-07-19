#' sim is the primary function for simulating management strategies to a regime switching model
#' @param sims The number of simulations, defaults to 1000
#' @param time_steps The number of time steps, defaults to 100
#' @param ricker_pars a dataframe of custom parameters for the Ricker model
#' @param pr_12 The probability of transitioning from state 1 to 2, defaults to 0.1
#' @param pr_21 The probability of transitioning from state 1 to 2, defaults to 0.1
#' @param run Whether to run this for the odd or even broodline, defaults to "odd"
#' @param deterministic_model Whether the deterministic escapement rule is used, defaults to TRUE
#' @param rec_std Recruitment variability (lognormal sd), defaults to 0.1
#' @param rec_acf Temporal autocorrelation of recruitment. Values of 1 = random walk, values of 0 = white noise. Defaults to 0.7
#' @param escapement_rule Can be "pre", "post", or "both" (default). If "pre" or "post", a single
#' escapement rule is used
#' @param harvest_CV The coefficient of variation for harvest. Variability is lognormal -- and this parameter controls the amount of variation.
#' @param discount_rate Optional, can also be done after the fact, but defaults to 0.1
#' @param time_lag Optional, represents a delay in how fast response is to the actual regime. Defaults to
#' 0 but could be a positive integer (e.g. 10) to represent delays in the decision making process
#' @param price_linear_change The price change over the entire time series. This is linear in log space to keep prices positive. Defaults to 0
#' @param price_acf The autocorrelation of prices. Defaults to 0.7, based on time series of pink salmon prices in Prince William Sound, 1984 - 2021
#' @param price_cv The coefficient of variation, or standard deviation in log space of prices. Defaults to 0 (realistic values based on time series of pink salmon prices in Prince William Sound, 1984 - 2021 = 0.1)
#' @param msy_scenario Scenario for maximum sustainable yield (MSY). Defaults to "equilibrium" where values are constant regardless of spawner abundance. Can
#'also be "msy", where values are updated each year
#' @param alpha The objective function to solve for with the MSY derivative. Defaults to 0
#' @param seed Seed for random number generation, defaults to 123
#' @return data frame of simulations
#'
#' @importFrom stats rnorm optimize
#' @export
#'
# bring up with group on next call: order of harvest v recruitment
# add variabiltiy by regime?
sim <- function(sims = 1000, # number of simulations
                time_steps = 100,
                ricker_pars = NULL,
                pr_12 = 0.1,# probability transitioning 1:2
                pr_21 = 0.1,# probability transitioning 2:1
                run = "odd",
                deterministic_model = TRUE,
                rec_std = 0.1, # recruitment standard deviation in log space
                rec_acf = 0.7, # recruitment autocorrelation in time
                escapement_rule = "both",
                harvest_CV = 0,
                discount_rate = 0.1,
                price_linear_change = 0,
                price_acf = 0.7, # default based on PWS pink salmon 1984 - 2021
                price_cv = 0, # default based on PWS pink salmon 1984 - 2021
                time_lag = 0,
                msy_scenario = c("equilibrium","msy")[1],
                alpha = 0,
                seed = 123) {

  if(is.null(ricker_pars)) {
    ricker_pars <- get_ricker()
  }
  if(deterministic_model == FALSE) {
    ricker_pars$S_star = ricker_pars$S_star_stochastic
  }
  ricker_pars = ricker_pars[which(ricker_pars$run == run),]

  # add fixed escapement rules here
  if(escapement_rule == "pre") ricker_pars$S_star[2] = ricker_pars$S_star[1]
  if(escapement_rule == "post") ricker_pars$S_star[1] = ricker_pars$S_star[2]

  # add extra time steps so first 'time lag' ones can be thrown out
  if(time_lag > 0) time_steps <- time_steps + time_lag
  time_steps <- time_steps + 1 # padded to remove first time step

  # transition matrix
  m = matrix(0, 2, 2)
  m[1,] = c(1-pr_12, pr_12)
  m[2,] = c(pr_21, 1-pr_21)

  price_feedback = FALSE
  if(price_linear_change != 0) price_feedback = TRUE
  if(price_cv != 0) price_feedback = TRUE

  # loop over iterations
  for(s in 1:sims) {

    set.seed(seed+s^2) # initial seed

    rec_dev = rnorm(1,0,sd=rec_std)    # recruitment deviations
    for(t in 2:time_steps) {
      rec_dev[t] = rnorm(1, mean = rec_acf * rec_dev[t-1], sd = rec_std)
    }

    # price change
    price1 <- ricker_pars$real_price[1] # starting price in regime 1
    prices = log(ricker_pars$real_price[1])
    price_dev1 = rnorm(1,0,price_cv)
    for(t in 2:time_steps) {
      price_dev1[t] = rnorm(1, price_acf*price_dev1[t-1], price_cv) - (price_cv*price_cv)/2 # autocorrelated price shocks
      prices[t] <- prices[t-1] + price_linear_change + price_dev1[t]
    }
    prices = exp(prices)

    # initial conditions in year 1, largely built off 2019
    x = sample(1:2,size=1)
    spawners = ricker_pars$S0[x[1]]
    rec = spawners * exp(ricker_pars$a[x[1]] + spawners*ricker_pars$b[x[1]])# recruitment first year, S0 * exp(a + bS0)

    if(price_feedback == TRUE) {
      # solve for S_star depending on regime and whether this is deterministic or stochastic
      if(deterministic_model == TRUE) {
        #delta = discount_rate
        #calib = ricker_pars$cst_param_calib[x[1]]
        #p = prices[1, x[1]]
        #a = ricker_pars$a[x[1]]
        #b = ricker_pars$b[x[1]]
        func = function(S) {
          bS = ricker_pars$b[x[1]]*S
          abS = ricker_pars$a[x[1]] + bS
          numer <- prices[1] - ricker_pars$cst_param_calib[x[1]]/(S*exp(abS))
          denom <- prices[1] - ricker_pars$cst_param_calib[x[1]]/S
          tot <- (numer/denom) * (1 + bS) * exp(abS)
          obj <- (tot - (1+discount_rate))^2
          return(obj)
        }
        o <- optimize(func, lower = 0.001, upper = 20, maximum = FALSE)
        harvest = 0
        optimal_escapement = o$minimum
        if(1 > time_lag) harvest = max(rec - o$minimum, 0) # use numerical solution
      } else {
        func = function(S) {
          bS <- eab <- tot <- rep(0,2)
          bS[1] <- ricker_pars$b[1]*S
          bS[2] <- ricker_pars$b[2]*S
          eab[1] <- exp(ricker_pars$a[1] + bS[1])
          eab[2] <- exp(ricker_pars$a[2] + bS[2])
          c = ricker_pars$cst_param_calib[x[1]]
          p = prices[1]
          tot[1] <- (pr_12 * (p - c / (S * eab[2])) * (1 + bS[2]) * eab[2] + (1 - pr_12) * (p - c / (S * eab[1])) * (1 + bS[1]) * eab[1]) / (p - c/S)
          tot[2] <- (pr_21 * (p - c / (S * eab[1])) * (1 + bS[1]) * eab[1] + (1 - pr_21) * (p - c / (S * eab[2])) * (1 + bS[2]) * eab[2]) / (p - c/S)
          if(x[1] == 1) {
            obj <- (tot[1] - (1+discount_rate))^2 # in state 1
          } else {
            obj <- (tot[2] - (1+discount_rate))^2 # in state 2
          }
          return(obj)
        }
        o <- optimize(func, lower = 0.001, upper = 20, maximum = FALSE)
        harvest = 0
        optimal_escapement = o$minimum
        if(1 > time_lag) harvest = max(rec - o$minimum, 0) # use numerical solution
      }
    } else {
      if(msy_scenario == "msy") {
        # same for both deterministic / stochastic
        harvest = 0
        Smsy = -(1 - lambert_W0(exp(1 - ricker_pars$a[x[1]]))) / ricker_pars$b[x[1]] # https://peerj.com/articles/1623/
        optimal_escapement = Smsy
        if(1 > time_lag) harvest = max(rec - Smsy, 0)
      }
      if(msy_scenario == "equilibrium") {
        harvest = max(rec - ricker_pars$S_star[x[1]], 0)
        optimal_escapement = ricker_pars$S_star[x[1]]
      }
    }

    net_benefits = 0 # needs to be updated

    for(t in 2:time_steps) {
      x[t] = sample(1:2, size=1, prob = m[x[t-1],]) # simulate regime
      spawners[t] <- max(rec[t-1] - harvest[t-1], 0)

      rec[t] <- spawners[t] * exp(ricker_pars$a[x[t]] + spawners[t]*ricker_pars$b[x[t]]) * exp(rec_dev[t] - 0.5*rec_std^2)

      if(price_feedback == TRUE) {
        # solve for S_star depending on regime and whether this is deterministic or stochastic
        # if model is constant, and prices are determistic -- might want to build in expected prices
        # also -- want to include optimization below in loop for robustness check
        if(deterministic_model == TRUE) {
          #delta = discount_rate
          #calib = ricker_pars$cst_param_calib[x[1]]
          #p = prices[1, x[1]]
          #a = ricker_pars$a[x[1]]
          #b = ricker_pars$b[x[1]]
          if(t > time_lag) {
            func = function(S) {
              numer <- prices[t] - ricker_pars$cst_param_calib[x[t - time_lag]]/(S*exp(ricker_pars$a[x[t - time_lag]] + ricker_pars$b[x[t - time_lag]]*S))
              denom <- prices[t] - ricker_pars$cst_param_calib[x[t - time_lag]]/S
              tot <- (numer/denom) * (1 + ricker_pars$b[x[t - time_lag]]*S) * exp(ricker_pars$a[x[t - time_lag]] + ricker_pars$b[x[t - time_lag]]*S)
              obj <- (tot - (1+discount_rate))^2
              return(obj)
            }
            o <- optimize(func, lower = 0.001, upper = 20, maximum = FALSE)
          }
          harvest[t] = 0
          optimal_escapement[t] = o$minimum
          if(t > time_lag) harvest[t] = max(rec[t] - o$minimum, 0) # use numerical solution
        } else {
          if(t > time_lag) {
            func = function(S) {
              bS <- ricker_pars$b[x[t - time_lag]]*S
              eab <- exp(ricker_pars$a[x[t - time_lag]] + bS[x[t - time_lag]])
              c = ricker_pars$cst_param_calib[x[1]] # not time or state varying
              p = prices[t] # time but not state varying
              tot <- (pr_12 * (p - c / (S * eab)) * (1 + bS) * eab + (1 - pr_12) * (p - c / (S * eab)) * (1 + bS) * eab) / (p - c/S)
              obj <- (tot - (1+discount_rate))^2 # in state 1
              return(obj)
            }
            o <- optimize(func, lower = 0.001, upper = 20, maximum = FALSE)
          }
          harvest[t] = 0
          optimal_escapement[t] = o$minimum
          if(t > time_lag) harvest[t] = max(rec[t] - o$minimum, 0) # use numerical solution
        }
      } else {
        if(msy_scenario == "msy") {
          harvest[t] = 0
          if(t > time_lag) {
            Smsy = -(1 - lambert_W0(exp(1 - ricker_pars$a[x[t - time_lag]]))) / ricker_pars$b[x[t - time_lag]] # https://peerj.com/articles/1623/
            if(1 > time_lag) harvest = max(rec - Smsy, 0)
            optimal_escapement[t] = Smsy
            if(t > time_lag) harvest[t] = max(rec[t] - Smsy, 0)
          }
        }
        if(msy_scenario == "equilibrium") {
          # add optional time lag, defaults to 0
          harvest[t] <- 0
          if(t > time_lag) {
            harvest[t] <- max(rec[t] - ricker_pars$S_star[x[t - time_lag]], 0)
            optimal_escapement[t] = ricker_pars$S_star[x[t - time_lag]]
          }
        }
      }

      # add in harvest variability
      if(harvest_CV > 0) harvest[t] <- harvest[t] * exp(rnorm(1,mean=0,sd=harvest_CV) - harvest_CV*harvest_CV/2.0)

      # add optional time lag, defaults to 0
      net_benefits[t] <- 0
      net_benefits[t] <- rec[t]*prices[t] - ricker_pars$cst_param_calib[x[t]] * log(rec[t]) -
        (spawners[t]*prices[t] - ricker_pars$cst_param_calib[x[t]] * log(spawners[t]))
      # escapement rules take prices into account. what if prices are lower than expected?
      # ties to marina's work: smaller fish. instead of changing escapement rule, change price
      # add in 5% and 20% declines in prices

      if(harvest[t]==0) net_benefits[t] <- 0 # per DF 5/20/22
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
                    optimal_escapement = optimal_escapement,
                    sim = s,
                    price_per_million = prices,
                    discount = discount)
    if(s==1) {
      all_df = df
    } else {
      all_df = rbind(df,all_df)
    }

  }

  # filter out first 'time lag' ones can be thrown out
  if(time_lag > 0) {
    all_df$t <- all_df$t - time_lag
    all_df <- all_df[which(all_df$t > 0),]
  }
  all_df <- all_df[which(all_df$t > 1),]
  all_df$t <- all_df$t - 1

  # calculate net benefits - millions
  all_df$discount_netben <- all_df$net_benefits*all_df$discount/1000000
  return(all_df)

}



