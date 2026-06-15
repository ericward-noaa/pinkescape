#' summarize_simulations returns summaries of benefits, harvest, and recruitment
#'
#' @param df A dataframe. If 'scenario' is included as a variable, it is used for grouping
#' @param quantiles a 2 element vector containing the quantiles to be reported (along with mean and median). Defaults to c(0.25, 0.75)
#' @return data frame of summarized simulations
#'
#' @details
#' Two changes from the earlier version, in addition to the `.data` pronoun used throughout to
#' silence the "no visible binding for global variable" check NOTE:
#'   * Escapement is computed per simulation / per time step in `mutate()` (`esc = rec - harvest`)
#'     and then summarized like the other quantities. Previously `ann_esc = rec - harvest` was
#'     computed inside the grouped `summarise()`, which returns a vector rather than one value per
#'     group and errors on dplyr >= 1.1.
#'   * `arrange()` before the cumulative sums makes `cumsum()` order-safe, and `.groups = "drop"`
#'     returns an ungrouped frame and silences the grouped-output message.
#'
#' @export
#' @import dplyr
#' @importFrom stats median quantile
#' @importFrom rlang .data
#'
summarize_simulations = function(df, quantiles = c(0.25, 0.75)) {
  if (!"scenario" %in% names(df)) df$scenario <- "all"

  # calculate the cumulative sum (and escapement) over time for each simulation / scenario
  summary <- df |>
    dplyr::arrange(.data$scenario, .data$sim, .data$t) |>
    dplyr::group_by(.data$sim, .data$scenario) |>
    dplyr::mutate(cum_ben  = cumsum(.data$discount_netben),
                  cum_harv = cumsum(.data$harvest),
                  cum_rec  = cumsum(.data$rec),
                  esc      = .data$rec - .data$harvest) |>
    dplyr::group_by(.data$t, .data$scenario) |> # group by scenario and get quartiles at each time step
    dplyr::summarise(
      lo25_ben = quantile(.data$cum_ben, quantiles[1]),
      hi75_ben = quantile(.data$cum_ben, quantiles[2]),
      m_ben    = mean(.data$cum_ben),
      med_ben  = median(.data$cum_ben),
      ann_ben_lo25 = quantile(.data$discount_netben, quantiles[1]),
      ann_ben_hi75 = quantile(.data$discount_netben, quantiles[2]),
      ann_ben_50   = quantile(.data$discount_netben, 0.5),
      ann_ben_mean = mean(.data$discount_netben),
      lo25_h = quantile(.data$cum_harv, quantiles[1]),
      hi75_h = quantile(.data$cum_harv, quantiles[2]),
      m_h    = mean(.data$cum_harv),
      med_h  = median(.data$cum_harv),
      ann_harvest_lo25 = quantile(.data$harvest, quantiles[1]),
      ann_harvest_hi75 = quantile(.data$harvest, quantiles[2]),
      ann_harvest_50   = quantile(.data$harvest, 0.5),
      ann_harvest_mean = mean(.data$harvest),
      lo25_rec = quantile(.data$cum_rec, quantiles[1]),
      hi75_rec = quantile(.data$cum_rec, quantiles[2]),
      m_rec    = mean(.data$cum_rec),
      med_rec  = median(.data$cum_rec),
      ann_rec_lo25 = quantile(.data$rec, quantiles[1]),
      ann_rec_hi75 = quantile(.data$rec, quantiles[2]),
      ann_rec_50   = quantile(.data$rec, 0.5),
      ann_rec_mean = mean(.data$rec),
      ann_esc_lo25 = quantile(.data$esc, quantiles[1]),
      ann_esc_hi75 = quantile(.data$esc, quantiles[2]),
      ann_esc_50   = quantile(.data$esc, 0.5),
      ann_esc_mean = mean(.data$esc),
      .groups = "drop"
    )
  return(summary)
}
