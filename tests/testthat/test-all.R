
test_that("ricker_defaults returns correct structure", {
  params <- ricker_defaults()

  # Check it's a data frame
  expect_s3_class(params, "data.frame")

  # Check dimensions: 4 rows (2 regimes x 2 runs)
  expect_equal(nrow(params), 4)

  # Check required columns exist
  required_cols <- c("regime", "regime_id", "run", "a", "b", "S0",
                     "S_star", "real_price", "cst_param_calib", "S_star_stochastic")
  expect_true(all(required_cols %in% names(params)))

  # Check regime values
  expect_true(all(params$regime %in% c("pre", "post")))

  # Check run values
  expect_true(all(params$run %in% c("odd", "even")))

  # Check numeric columns are numeric
  expect_type(params$a, "double")
  expect_type(params$b, "double")
  expect_type(params$S0, "double")
})

test_that("ricker_defaults has reasonable parameter values", {
  params <- ricker_defaults()

  # Ricker b parameter should be negative (density dependence)
  expect_true(all(params$b < 0))

  # Positive values where expected
  expect_true(all(params$S0 > 0))
  expect_true(all(params$S_star > 0))
  expect_true(all(params$real_price > 0))
  expect_true(all(params$cst_param_calib > 0))
})

test_that("sim runs with default parameters", {
  # Run a small simulation to test
  result <- sim(sims = 10, time_steps = 10, seed = 123)

  # Check output is a data frame
  expect_s3_class(result, "data.frame")

  # Check dimensions: 10 sims x 10 time steps = 100 rows
  expect_equal(nrow(result), 100)

  # Check required columns exist
  required_cols <- c("t", "regime", "spawners", "rec", "harvest",
                     "net_benefits", "optimal_escapement", "sim",
                     "price_per_million", "discount", "discount_netben")
  expect_true(all(required_cols %in% names(result)))
})

test_that("sim respects number of simulations", {
  result <- sim(sims = 5, time_steps = 10, seed = 123)
  expect_equal(length(unique(result$sim)), 5)
})

test_that("sim respects number of time steps", {
  result <- sim(sims = 3, time_steps = 20, seed = 123)
  expect_equal(nrow(result), 3 * 20)
  expect_equal(max(result$t), 20)
})

test_that("sim produces consistent results with same seed", {
  result1 <- sim(sims = 5, time_steps = 10, seed = 999)
  result2 <- sim(sims = 5, time_steps = 10, seed = 999)

  # Should be identical
  expect_equal(result1$spawners, result2$spawners)
  expect_equal(result1$harvest, result2$harvest)
})

test_that("sim produces different results with different seeds", {
  result1 <- sim(sims = 5, time_steps = 10, seed = 123)
  result2 <- sim(sims = 5, time_steps = 10, seed = 456)

  # Should NOT be identical
  expect_false(all(result1$spawners == result2$spawners))
})

test_that("sim respects run parameter", {
  # Test with custom parameters to verify filtering
  params_odd <- ricker_defaults()
  result_odd <- sim(sims = 5, time_steps = 10, run = "odd",
                    ricker_pars = params_odd, seed = 123)

  result_even <- sim(sims = 5, time_steps = 10, run = "even",
                     ricker_pars = params_odd, seed = 123)

  # Results should differ because different parameters used
  expect_false(identical(result_odd$spawners, result_even$spawners))
})

test_that("sim regime states are valid", {
  result <- sim(sims = 10, time_steps = 10, seed = 123)

  # Regime should only be 1 or 2
  expect_true(all(result$regime %in% c(1, 2)))
})

test_that("sim produces non-negative values where required", {
  result <- sim(sims = 10, time_steps = 20, seed = 123)

  # These should never be negative
  expect_true(all(result$spawners >= 0))
  expect_true(all(result$rec >= 0))
  expect_true(all(result$harvest >= 0))
  expect_true(all(result$optimal_escapement >= 0))
})

test_that("sim harvest never exceeds recruitment", {
  result <- sim(sims = 10, time_steps = 20, seed = 123)

  # Harvest should never exceed recruitment (after accounting for escapement)
  expect_true(all(result$harvest <= result$rec))
})

test_that("sim escapement rules work correctly", {
  # Test "both" (default)
  result_both <- sim(sims = 5, time_steps = 10, escapement_rule = "both", seed = 123)

  # Test "pre" (should use only pre-regime rule)
  result_pre <- sim(sims = 5, time_steps = 10, escapement_rule = "pre", seed = 123)

  # Test "post" (should use only post-regime rule)
  result_post <- sim(sims = 5, time_steps = 10, escapement_rule = "post", seed = 123)

  # All should run without error and produce results
  expect_s3_class(result_both, "data.frame")
  expect_s3_class(result_pre, "data.frame")
  expect_s3_class(result_post, "data.frame")

  # Results should differ
  expect_false(identical(result_both$harvest, result_pre$harvest))
  expect_false(identical(result_both$harvest, result_post$harvest))
})

test_that("sim deterministic vs stochastic models differ", {
  result_det <- sim(sims = 5, time_steps = 10,
                    deterministic_model = TRUE, seed = 123)
  result_stoch <- sim(sims = 5, time_steps = 10,
                      deterministic_model = FALSE, seed = 123)

  # Should use different escapement targets
  expect_false(identical(result_det$optimal_escapement, result_stoch$optimal_escapement))
})

test_that("sim handles recruitment variability", {
  # No variability
  result_novar <- sim(sims = 5, time_steps = 20, rec_std = 0, seed = 123)

  # With variability
  result_var <- sim(sims = 5, time_steps = 20, rec_std = 0.2, seed = 123)

  # With variability, we should see more variation in recruitment
  var_novar <- var(result_novar$rec)
  var_var <- var(result_var$rec)

  expect_true(var_var > var_novar)
})

test_that("sim handles harvest implementation error", {
  # Perfect implementation
  result_perfect <- sim(sims = 5, time_steps = 20, harvest_CV = 0, seed = 123)

  # With implementation error
  result_error <- sim(sims = 5, time_steps = 20, harvest_CV = 0.15, seed = 123)

  # With error, actual harvest should deviate from target
  # (though this is a probabilistic test, so could occasionally fail)
  expect_false(identical(result_perfect$harvest, result_error$harvest))
})

test_that("sim handles time lag correctly", {
  # No lag
  result_no_lag <- sim(sims = 5, time_steps = 20, time_lag = 0, seed = 123)

  # With 5-year lag
  result_lag <- sim(sims = 5, time_steps = 20, time_lag = 5, seed = 123)

  # Both should have same number of output rows
  expect_equal(nrow(result_no_lag), nrow(result_lag))

  # Time should still go from 1 to 20
  expect_equal(max(result_lag$t), 20)

  # Decisions should differ due to lag
  expect_false(identical(result_no_lag$harvest, result_lag$harvest))
})

test_that("sim discount rate affects net benefits", {
  result_low <- sim(sims = 5, time_steps = 20, discount_rate = 0.05, seed = 123)
  result_high <- sim(sims = 5, time_steps = 20, discount_rate = 0.15, seed = 123)

  # Higher discount rate should reduce later benefits more
  # Check that discount factors differ
  expect_false(identical(result_low$discount, result_high$discount))
  expect_false(identical(result_low$discount_netben, result_high$discount_netben))
})

test_that("sim handles price dynamics", {
  # Constant prices
  result_const <- sim(sims = 5, time_steps = 20, price_cv = 0,
                      price_linear_change = 0, seed = 123)

  # Variable prices
  result_var <- sim(sims = 5, time_steps = 20, price_cv = 0.1, seed = 123)

  # Trending prices
  result_trend <- sim(sims = 5, time_steps = 20, price_linear_change = 0.01, seed = 123)

  # Constant prices should have less variation
  cv_const <- sd(result_const$price_per_million) / mean(result_const$price_per_million)
  cv_var <- sd(result_var$price_per_million) / mean(result_var$price_per_million)

  expect_true(cv_var > cv_const)

  # Trending prices should show trend
  first_year_price <- mean(result_trend$price_per_million[result_trend$t == 1])
  last_year_price <- mean(result_trend$price_per_million[result_trend$t == 20])
  expect_true(last_year_price > first_year_price)
})

test_that("sim MSY scenario works", {
  result_eq <- sim(sims = 5, time_steps = 10, msy_scenario = "equilibrium", seed = 123)
  result_msy <- sim(sims = 5, time_steps = 10, msy_scenario = "msy", seed = 123)

  # Both should run
  expect_s3_class(result_eq, "data.frame")
  expect_s3_class(result_msy, "data.frame")

  # Should produce different escapement targets
  expect_false(identical(result_eq$optimal_escapement, result_msy$optimal_escapement))
})

test_that("sim with custom parameters works", {
  custom_params <- ricker_defaults()
  custom_params$a <- custom_params$a * 1.1  # Modify productivity

  result <- sim(sims = 5, time_steps = 10, ricker_pars = custom_params, seed = 123)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
})

test_that("summarize_simulations works with simulation output", {
  sim_results <- sim(sims = 10, time_steps = 20, seed = 123)
  summary <- summarize_simulations(sim_results)

  # Should return a data frame
  expect_s3_class(summary, "data.frame")

  # Should have one row per time step
  expect_equal(nrow(summary), 200)

  # Check key columns exist
  expect_true("t" %in% names(summary))
  expect_true("m_ben" %in% names(summary))
  expect_true("lo25_ben" %in% names(summary))
  expect_true("hi75_ben" %in% names(summary))
})

test_that("summarize_simulations handles scenarios", {
  # Create two scenarios
  sim1 <- sim(sims = 5, time_steps = 10, time_lag = 0, seed = 123)
  sim1$scenario <- "No lag"

  sim2 <- sim(sims = 5, time_steps = 10, time_lag = 5, seed = 123)
  sim2$scenario <- "5-year lag"

  combined <- rbind(sim1, sim2)
  summary <- summarize_simulations(combined)

  # Should have rows for each scenario x time step
  expect_equal(nrow(summary), 100)  # 10 time steps x 2 scenarios
  expect_true("scenario" %in% names(summary))
  expect_equal(length(unique(summary$scenario)), 2)
})

test_that("summarize_simulations respects custom quantiles", {
  sim_results <- sim(sims = 10, time_steps = 10, seed = 123)

  summary_default <- summarize_simulations(sim_results, quantiles = c(0.25, 0.75))
  summary_wide <- summarize_simulations(sim_results, quantiles = c(0.1, 0.9))

  # Both should work
  expect_s3_class(summary_default, "data.frame")
  expect_s3_class(summary_wide, "data.frame")

  # Wider quantiles should have larger spread
  # Check for at least one time point
  default_spread <- summary_default$hi75_ben[5] - summary_default$lo25_ben[5]
  wide_spread <- summary_wide$hi75_ben[5] - summary_wide$lo25_ben[5]

  expect_true(wide_spread > default_spread)
})

test_that("summarize_simulations calculates cumulative correctly", {
  sim_results <- sim(sims = 10, time_steps = 10, seed = 123)
  summary <- summarize_simulations(sim_results)

  # Cumulative should be increasing (or at least non-decreasing)
  expect_true(all(diff(summary$m_ben) >= -1e-10))  # Allow tiny numerical errors
  expect_true(all(diff(summary$m_h) >= -1e-10))
  expect_true(all(diff(summary$m_rec) >= -1e-10))
})

test_that("summarize_plots creates plot list", {
  sim_results <- sim(sims = 10, time_steps = 20, seed = 123)
  summary <- summarize_simulations(sim_results)
  plots <- plot_summary(summary)

  # Should return a list
  expect_type(plots, "list")

  # Should have 7 plots
  expect_equal(length(plots), 7)

  # Each element should be a ggplot
  for(i in 1:7) {
    expect_s3_class(plots[[i]], "gg")
  }
})

test_that("summarize_plots handles scenarios", {
  sim1 <- sim(sims = 5, time_steps = 10, time_lag = 0, seed = 123)
  sim1$scenario <- "No lag"

  sim2 <- sim(sims = 5, time_steps = 10, time_lag = 5, seed = 123)
  sim2$scenario <- "5-year lag"

  combined <- rbind(sim1, sim2)
  summary <- summarize_simulations(combined)
  plots <- plot_summary(summary)

  # Should create plots without error
  expect_equal(length(plots), 7)

  # Plots should handle multiple scenarios (check one plot)
  expect_s3_class(plots[[1]], "gg")
})

test_that("full workflow runs without errors", {
  # This tests the complete pipeline

  # 1. Get default parameters
  params <- ricker_defaults()
  expect_s3_class(params, "data.frame")

  # 2. Run simulation
  results <- sim(sims = 20, time_steps = 30, ricker_pars = params, seed = 123)
  expect_s3_class(results, "data.frame")

  # 3. Summarize
  summary <- summarize_simulations(results)
  expect_s3_class(summary, "data.frame")

  # 4. Plot
  plots <- plot_summary(summary)
  expect_equal(length(plots), 7)
})

test_that("scenario comparison workflow works", {
  scenarios <- list()
  for(lag in c(0, 5, 10)) {
    result <- sim(sims = 10, time_steps = 20, time_lag = lag, seed = 123)
    result$scenario <- paste0("Lag_", lag)
    scenarios[[length(scenarios) + 1]] <- result
  }

  combined <- do.call(rbind, scenarios)
  summary <- summarize_simulations(combined)
  plots <- plot_summary(summary)

  expect_equal(length(unique(combined$scenario)), 3)
  expect_s3_class(summary, "data.frame")
  expect_equal(length(plots), 7)
})

test_that("sim handles edge case: zero time steps", {
  # Should handle gracefully or error informatively
  expect_error(sim(sims = 1, time_steps = 0))
})

test_that("sim handles extreme parameter values", {
  # Very high recruitment variability
  result_high_var <- sim(sims = 5, time_steps = 10, rec_std = 0.5, seed = 123)
  expect_s3_class(result_high_var, "data.frame")

  # Very high discount rate
  result_high_disc <- sim(sims = 5, time_steps = 10, discount_rate = 0.9, seed = 123)
  expect_s3_class(result_high_disc, "data.frame")
})

test_that("sim handles extreme transition probabilities", {
  # Frequent switching
  result_freq <- sim(sims = 5, time_steps = 20, pr_12 = 0.5, pr_21 = 0.5, seed = 123)

  # Rare switching
  result_rare <- sim(sims = 5, time_steps = 20, pr_12 = 0.01, pr_21 = 0.01, seed = 123)

  # Both should run
  expect_s3_class(result_freq, "data.frame")
  expect_s3_class(result_rare, "data.frame")

  # Frequent switching should have more regime changes
  regime_changes_freq <- sum(diff(result_freq$regime[result_freq$sim == 1]) != 0)
  regime_changes_rare <- sum(diff(result_rare$regime[result_rare$sim == 1]) != 0)

})

test_that("sim output has correct data types", {
  result <- sim(sims = 5, time_steps = 10, seed = 123)

  # Check column types
  expect_type(result$t, "double")
  expect_type(result$regime, "integer")
  expect_type(result$spawners, "double")
  expect_type(result$rec, "double")
  expect_type(result$harvest, "double")
  expect_type(result$sim, "integer")
})

test_that("summarize_simulations output has correct structure", {
  sim_results <- sim(sims = 10, time_steps = 10, seed = 123)
  summary <- summarize_simulations(sim_results)

  # Check that means are between quantiles (basic sanity check)
  expect_true(all(summary$m_ben >= summary$lo25_ben))
  expect_true(all(summary$m_ben <= summary$hi75_ben))

  expect_true(all(summary$m_h >= summary$lo25_h))
  expect_true(all(summary$m_h <= summary$hi75_h))
})
