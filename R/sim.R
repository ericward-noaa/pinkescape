#' sim is the primary function for simulating management strategies to a regime switching model
#'
#' @param sims The number of simulations, defaults to 1000
#' @param time_steps The number of time steps, defaults to 100
#' @param ricker_pars a dataframe of custom parameters for the Ricker model
#' @param pr_12 The probability of transitioning from state 1 to 2, defaults to 0.1
#' @param pr_21 The probability of transitioning from state 2 to 1, defaults to 0.1
#' @param run Whether to run this for the odd or even broodline, defaults to "odd"
#' @param deterministic_model Whether the deterministic escapement rule is used, defaults to TRUE
#' @param rec_std Recruitment variability (lognormal sd), defaults to 0.1
#' @param rec_acf Temporal autocorrelation of recruitment. Values of 1 = random walk, values of 0 = white noise. Defaults to 0.7
#' @param escapement_rule Can be "pre", "post", or "both" (default). If "pre" or "post", a single
#' escapement rule is used
#' @param harvest_CV The coefficient of variation for harvest. Variability is lognormal -- and this parameter controls the amount of variation.
#' @param discount_rate Optional, can also be done after the fact, but defaults to 0.1
#' @param time_lag Optional, represents a delay in how fast response is to the actual regime. Defaults to
#' 0 but could be a positive integer (e.g. 10) to represent delays in the decision making process.
#' At time t the manager sets escapement using the regime as it was \code{time_lag} steps earlier,
#' i.e. \code{x[t - time_lag]}; during the first \code{time_lag} steps the oldest available
#' observation (the initial regime) is used. See Details for how this differs from earlier versions.
#' @param price_linear_change The price change over the entire time series. This is linear in log space to keep prices positive. Defaults to 0
#' @param price_acf The autocorrelation of prices. Defaults to 0.7, based on time series of pink salmon prices in Prince William Sound, 1984 - 2021
#' @param price_cv The coefficient of variation, or standard deviation in log space of prices. Defaults to 0 (realistic values based on time series of pink salmon prices in Prince William Sound, 1984 - 2021 = 0.1)
#' @param use_price_feedback Controls which escapement rule is used. If \code{NULL} (default), the
#' price-feedback optimizer is used whenever prices vary (\code{price_cv != 0} or
#' \code{price_linear_change != 0}); otherwise \code{msy_scenario} is used. This reproduces the
#' original (implicit) dispatch. Set \code{FALSE} to force the \code{msy_scenario} rule even with
#' dynamic prices (e.g. a clean "declining prices on a fixed MSY rule" comparison), or \code{TRUE}
#' to force the feedback optimizer.
#' @param msy_scenario Scenario for maximum sustainable yield (MSY). Defaults to "equilibrium" where values are constant regardless of spawner abundance. Can
#' also be "msy", where values are updated each year. Only used when price feedback is off (see \code{use_price_feedback}).
#' @param seed Seed for random number generation, defaults to 123
#' @return data frame of simulations
#'
#' @importFrom stats rnorm optimize
#' @importFrom gsl lambert_W0
#' @export
#'
sim <- function(sims = 1000,
                time_steps = 100,
                ricker_pars = NULL,
                pr_12 = 0.1,
                pr_21 = 0.1,
                run = "odd",
                deterministic_model = TRUE,
                rec_std = 0.1,
                rec_acf = 0.7,
                escapement_rule = "both",
                harvest_CV = 0,
                discount_rate = 0.1,
                price_linear_change = 0,
                price_acf = 0.7,
                price_cv = 0,
                time_lag = 0,
                use_price_feedback = NULL,
                msy_scenario = NULL,
                seed = 123) {

  if (is.null(ricker_pars)) {
    ricker_pars <- ricker_defaults()
  }
  if (deterministic_model == FALSE) {
    ricker_pars$S_star <- ricker_pars$S_star_stochastic
  }
  ricker_pars <- ricker_pars[which(ricker_pars$run == run), ]

  if (is.null(msy_scenario)) msy_scenario <- c("equilibrium", "msy")[1]

  # add fixed escapement rules here
  if (escapement_rule == "pre")  ricker_pars$S_star[2] <- ricker_pars$S_star[1]
  if (escapement_rule == "post") ricker_pars$S_star[1] <- ricker_pars$S_star[2]

  # horizon no longer depends on time_lag; only the universal +1
  # (the first, deterministic, step is dropped at the end). This keeps the noise stream and the
  # discount schedule identical across time_lag values so the lag comparison is properly paired.
  time_steps <- time_steps + 1

  # transition matrix
  m <- matrix(0, 2, 2)
  m[1, ] <- c(1 - pr_12, pr_12)
  m[2, ] <- c(pr_21, 1 - pr_21)

  # rule selection is explicit rather than inferred
  if (is.null(use_price_feedback)) {
    use_feedback <- (price_linear_change != 0) || (price_cv != 0)
  } else {
    use_feedback <- isTRUE(use_price_feedback)
  }
  if (use_feedback && !is.null(msy_scenario) && msy_scenario == "msy") {
    message("Price feedback is ON, so escapement uses the price-feedback optimizer; ",
            "msy_scenario = 'msy' is ignored for this run. ",
            "Set use_price_feedback = FALSE to keep the MSY rule under dynamic prices.")
  }

  # regime_idx is the regime the manager responds to (possibly lagged); price_now is prices[t].
  get_escapement <- function(regime_idx, price_now) {
    if (use_feedback) {
      if (deterministic_model) {
        f <- function(S) {
          bS  <- ricker_pars$b[regime_idx] * S
          abS <- ricker_pars$a[regime_idx] + bS
          cc  <- ricker_pars$cst_param_calib[regime_idx]
          numer <- price_now - cc / (S * exp(abS))
          denom <- price_now - cc / S
          tot   <- (numer / denom) * (1 + bS) * exp(abS)
          (tot - (1 + discount_rate))^2
        }
      } else {
        # correct two-regime expectation (mirrors the original *pre-loop* version);
        # the old in-loop objective indexed a scalar by regime id (-> NA) and used identical
        # terms for both regimes.
        f <- function(S) {
          bS  <- ricker_pars$b * S                 # length 2
          eab <- exp(ricker_pars$a + bS)           # length 2
          cc  <- ricker_pars$cst_param_calib[regime_idx]
          p   <- price_now
          tot1 <- (pr_12 * (p - cc / (S * eab[2])) * (1 + bS[2]) * eab[2] +
                     (1 - pr_12) * (p - cc / (S * eab[1])) * (1 + bS[1]) * eab[1]) / (p - cc / S)
          tot2 <- (pr_21 * (p - cc / (S * eab[1])) * (1 + bS[1]) * eab[1] +
                     (1 - pr_21) * (p - cc / (S * eab[2])) * (1 + bS[2]) * eab[2]) / (p - cc / S)
          tot  <- if (regime_idx == 1) tot1 else tot2
          (tot - (1 + discount_rate))^2
        }
      }
      optimize(f, lower = 0.001, upper = 20, maximum = FALSE)$minimum
    } else if (msy_scenario == "msy") {
      # https://peerj.com/articles/1623/
      -(1 - lambert_W0(exp(1 - ricker_pars$a[regime_idx]))) / ricker_pars$b[regime_idx]
    } else { # "equilibrium"
      ricker_pars$S_star[regime_idx]
    }
  }

  # preallocate and rbind once at the end
  out_list <- vector("list", sims)

  for (s in 1:sims) {

    set.seed(seed + s^2) # initial seed

    # recruitment deviations: AR(1)
    rec_dev <- rnorm(1, 0, sd = rec_std)
    for (t in 2:time_steps) {
      rec_dev[t] <- rnorm(1, mean = rec_acf * rec_dev[t - 1], sd = rec_std)
    }

    # exact per-step variance of rec_dev for a mean-preserving lognormal correction.
    # No random draws change here - only the deterministic correction term. Converges to the
    # stationary variance rec_std^2 / (1 - rec_acf^2); also correct for rec_acf = 1 (random walk).
    rec_var <- numeric(time_steps)
    rec_var[1] <- rec_std^2
    if (time_steps >= 2) {
      for (t in 2:time_steps) rec_var[t] <- rec_acf^2 * rec_var[t - 1] + rec_std^2
    }

    # price change (log-space random walk with AR(1) increments)
    prices <- log(ricker_pars$real_price[1])
    price_dev1 <- rnorm(1, 0, price_cv)
    for (t in 2:time_steps) {
      # autocorrelated price shocks; -price_cv^2/2 keeps E[price] level when price_acf = 0
      price_dev1[t] <- rnorm(1, price_acf * price_dev1[t - 1], price_cv) - (price_cv * price_cv) / 2
      prices[t] <- prices[t - 1] + price_linear_change + price_dev1[t]
    }
    prices <- exp(prices)

    # initial conditions in year 1 (this row is dropped at the end, but its harvest feeds year 2)
    x <- sample(1:2, size = 1)
    spawners <- ricker_pars$S0[x[1]]
    rec <- spawners * exp(ricker_pars$a[x[1]] + spawners * ricker_pars$b[x[1]])

    # initial escapement/harvest via the shared helper (manager acts on the initial regime)
    optimal_escapement <- get_escapement(x[1], prices[1])
    harvest <- max(rec - optimal_escapement, 0)
    net_benefits <- 0

    for (t in 2:time_steps) {
      x[t] <- sample(1:2, size = 1, prob = m[x[t - 1], ]) # simulate true regime
      spawners[t] <- max(rec[t - 1] - harvest[t - 1], 0)

      # recruitment responds to the TRUE current regime x[t]
      rec[t] <- spawners[t] *
        exp(ricker_pars$a[x[t]] + spawners[t] * ricker_pars$b[x[t]]) *
        exp(rec_dev[t] - 0.5 * rec_var[t])              # exact bias correction

      # manager responds to the regime as it was time_lag steps ago; during the first
      # time_lag steps the oldest available observation (the initial regime) is used. The whole
      # rule (feedback / msy / equilibrium) now runs every step via the shared helper
      mgmt_idx <- x[max(t - time_lag, 1)]
      optimal_escapement[t] <- get_escapement(mgmt_idx, prices[t])
      harvest[t] <- max(rec[t] - optimal_escapement[t], 0)

      # harvest variability (lognormal); only consumes RNG when harvest_CV > 0
      if (harvest_CV > 0) {
        harvest[t] <- harvest[t] * exp(rnorm(1, mean = 0, sd = harvest_CV) - harvest_CV * harvest_CV / 2.0)
        cap <- rec[t] - optimal_escapement[t]
        if (!is.na(cap) && harvest[t] > cap) harvest[t] <- cap   # NA-safe
        if (harvest[t] < 0) harvest[t] <- 0
      }

      # net benefits; zeroed in no-harvest years per DF 5/20/22
      cc_t <- ricker_pars$cst_param_calib[x[t]]
      net_benefits[t] <- rec[t] * prices[t] - cc_t * log(rec[t]) -
        (spawners[t] * prices[t] - cc_t * log(spawners[t]))
      if (harvest[t] == 0) net_benefits[t] <- 0
    }

    # discount factor (computed on the original time index, before trimming, as before)
    discount <- 1 / ((1 + discount_rate)^(seq_len(time_steps) - 1))

    out_list[[s]] <- data.frame(
      t = 1:time_steps,
      regime = x,
      spawners = spawners,
      rec = rec,
      rec_dev = rec_dev,
      harvest = harvest,
      net_benefits = net_benefits,
      optimal_escapement = optimal_escapement,
      sim = s,
      price_per_million = prices,
      discount = discount
    )
  }

  all_df <- do.call(rbind, out_list)

  # drop only the first (deterministic) step; the time_lag burn-in trim is gone because
  # the horizon is no longer padded. discount stays paired with its original-t value, so reported
  # t = 1 carries discount = 1/(1+r), identically for every time_lag.
  all_df <- all_df[all_df$t > 1, ]
  all_df$t <- all_df$t - 1

  # calculate net benefits - millions
  all_df$discount_netben <- all_df$net_benefits * all_df$discount / 1000000
  return(all_df)
}
