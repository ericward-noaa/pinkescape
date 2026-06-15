#' escapement_targets computes optimal escapement targets for the MSY, DT, and ST rules
#'
#' Solves the Ricker management problem for the maximum-sustainable-yield (MSY), deterministic
#' (DT), and stochastic (ST) escapement rules and returns one target per regime. Targets are found
#' by sign-change root-finding on the Euler condition rather than by minimizing the squared
#' objective, which avoids the wrong-root / boundary failures that \code{stats::optimize()} is prone
#' to when the Euler equation has a pole at \eqn{S = c/p} or more than one root.
#'
#' @param ricker_pars A data frame of Ricker parameters (as from \code{ricker_defaults()}). The rows
#'   for the requested \code{run} must supply \code{a}, \code{b}, \code{real_price} and
#'   \code{cst_param_calib}. If \code{price} or \code{cost} are supplied they override the values in
#'   \code{ricker_pars}. Defaults to \code{ricker_defaults()}.
#' @param run Which broodline to use, "odd" (default) or "even".
#' @param price Optional unit price p (per million fish). If \code{NULL} (default) the value in
#'   \code{ricker_pars$real_price} is used. A single value, or a length-2 vector for regime-specific
#'   prices.
#' @param cost Optional cost parameter c. If \code{NULL} (default) the value in
#'   \code{ricker_pars$cst_param_calib} is used. A single value, or length-2 for regime-specific cost.
#' @param discount_rate Discount rate delta. Defaults to 0.1.
#' @param pr_12 Probability of transitioning from regime 1 to 2 (used only by ST). Defaults to 0.118.
#' @param pr_21 Probability of transitioning from regime 2 to 1 (used only by ST). Defaults to 0.202.
#' @param lower,upper Search bounds for escapement. Defaults to 0.001 and 20.
#'
#' @return A data frame with one row per regime and columns: \code{run}, \code{regime},
#'   \code{regime_id}, \code{price}, \code{cost}, \code{discount_rate}, \code{pr_12}, \code{pr_21},
#'   \code{MSY}, \code{DT}, \code{ST}, \code{pole} (= c/p), \code{DT_n_roots}, \code{ST_n_roots}, and
#'   \code{flag}. \code{flag} is "" when the solution is clean, or a comma-separated list of
#'   "multi-root" (the Euler equation had more than one root in range, so the economically sensible
#'   one below the pole was selected) and "near-pole"/"near-bound" (the chosen target sits within a
#'   small tolerance of the pole or a search bound -- treat with care).
#'
#' @details
#' Let \eqn{R_j(S) = S e^{a_j + b_j S}} be next-year recruitment in regime j, with marginal
#' recruitment \eqn{R_j'(S) = e^{a_j + b_j S}(1 + b_j S)}. The optimal escapement equates the
#' marginal value of harvesting a fish now with the (discounted, possibly regime-uncertain) marginal
#' value of the recruits it would produce:
#'
#' MSY (depends only on a, b):
#'   \deqn{S^{MSY} = -(1 - W_0(e^{1-a})) / b}
#'
#' DT, assuming the current regime i persists:
#'   \deqn{(1+\delta)(p - c/S) = (p - c/R_i(S)) R_i'(S)}
#'
#' ST, taking the expectation over the Markov transition out of regime i:
#'   \deqn{(1+\delta)(p - c/S) = \sum_j P(i \to j) (p - c/R_j(S)) R_j'(S)}
#'
#' DT and MSY do not depend on the transition probabilities; ST does. DT and ST depend on price and
#' cost, so any sweep over p or c (e.g. a re-calibration of the profit margin) changes them and
#' should be regenerated with this function rather than read from the stored \code{S_star} /
#' \code{S_star_stochastic} columns, which are fixed at the baseline calibration.
#'
#' @importFrom stats uniroot
#' @importFrom gsl lambert_W0
#' @importFrom stats uniroot
#' @export
#'
escapement_targets <- function(ricker_pars = NULL,
                               run = "odd",
                               price = NULL,
                               cost = NULL,
                               discount_rate = 0.1,
                               pr_12 = 0.118,
                               pr_21 = 0.202,
                               lower = 0.001,
                               upper = 20) {

  if (is.null(ricker_pars)) ricker_pars <- ricker_defaults()
  rp <- ricker_pars[which(ricker_pars$run == run), ]
  if (nrow(rp) != 2) stop("Expected exactly 2 regimes for run = '", run, "'.")

  a <- rp$a
  b <- rp$b
  p <- if (is.null(price)) rp$real_price      else rep_len(price, 2)
  c <- if (is.null(cost))  rp$cst_param_calib else rep_len(cost, 2)

  # Euler residual g(S) = tot(S) - (1 + delta); root of g is the target.
  # DT: single regime i. ST: expectation over transition from regime i.
  g_dt <- function(S, i) {
    bS  <- b[i] * S
    eab <- exp(a[i] + bS)
    (p[i] - c[i] / (S * eab)) / (p[i] - c[i] / S) * (1 + bS) * eab - (1 + discount_rate)
  }
  g_st <- function(S, i) {
    bS1 <- b[1] * S; bS2 <- b[2] * S
    e1  <- exp(a[1] + bS1); e2 <- exp(a[2] + bS2)
    den <- p[i] - c[i] / S
    t1  <- (pr_12 * (p[i] - c[i] / (S * e2)) * (1 + bS2) * e2 +
              (1 - pr_12) * (p[i] - c[i] / (S * e1)) * (1 + bS1) * e1) / den
    t2  <- (pr_21 * (p[i] - c[i] / (S * e1)) * (1 + bS1) * e1 +
              (1 - pr_21) * (p[i] - c[i] / (S * e2)) * (1 + bS2) * e2) / den
    (if (i == 1) t1 else t2) - (1 + discount_rate)
  }

  # find all sign-change roots of g on (lower, upper), splitting around the pole at S = c/p
  find_roots <- function(g, i) {
    pole  <- c[i] / p[i]
    segs  <- list(c(lower, pole - 1e-6), c(pole + 1e-6, upper))
    roots <- numeric(0)
    for (seg in segs) {
      if (seg[2] <= seg[1]) next
      xs <- seq(seg[1], seg[2], length.out = 5000)
      ys <- vapply(xs, function(x) g(x, i), numeric(1))
      ok <- is.finite(ys)
      for (k in seq_len(length(xs) - 1)) {
        if (ok[k] && ok[k + 1] && ys[k] * ys[k + 1] < 0) {
          r <- tryCatch(uniroot(function(x) g(x, i), c(xs[k], xs[k + 1]))$root,
                        error = function(e) NA_real_)
          if (is.finite(r)) roots <- c(roots, r)
        }
      }
    }
    sort(roots)
  }

  # pick the economically sensible root: smallest root below the pole if any, else smallest root
  pick <- function(roots, pole) {
    below <- roots[roots < pole]
    if (length(below)) min(below) else if (length(roots)) min(roots) else NA_real_
  }

  out <- vector("list", 2)
  for (i in 1:2) {
    pole    <- c[i] / p[i]
    msy     <- -(1 - lambert_W0(exp(1 - a[i]))) / b[i]
    dt_r    <- find_roots(g_dt, i); dt <- pick(dt_r, pole)
    st_r    <- find_roots(g_st, i); st <- pick(st_r, pole)

    flags <- character(0)
    if (length(dt_r) > 1 || length(st_r) > 1) flags <- c(flags, "multi-root")
    tol <- 0.3
    if ((is.finite(dt) && abs(dt - pole) < tol) || (is.finite(st) && abs(st - pole) < tol))
      flags <- c(flags, "near-pole")
    bnd <- 1e-3
    if (any(abs(c(dt, st) - lower) < bnd | abs(c(dt, st) - upper) < bnd, na.rm = TRUE))
      flags <- c(flags, "near-bound")

    out[[i]] <- data.frame(
      run = run, regime = rp$regime[i], regime_id = rp$regime_id[i],
      price = p[i], cost = c[i], discount_rate = discount_rate,
      pr_12 = pr_12, pr_21 = pr_21,
      MSY = msy, DT = dt, ST = st, pole = pole,
      DT_n_roots = length(dt_r), ST_n_roots = length(st_r),
      flag = paste(flags, collapse = ","),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
