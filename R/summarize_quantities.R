#' summarize_quantities returns summaries of benefits, harvest, and recruitment
#' @param df A dataframe. If 'scenario' is included as a variable, it is used for grouping
#' @param quantiles a 2 element vector containing the quantiles to be reported (along with mean and median). Defaults to c(0.25, 0.75)
#' @return data frame of default parameters for simulations
#'
#' @export
#' @import dplyr
#' @importFrom stats median quantile
#'
summarize_quantities = function(df, quantiles = c(0.25, 0.75)) {

  if("scenario" %in% names(df) == FALSE) df$scenario <- "all"

  # calculate the cumulative sum over time for each simulation / scenario
  summary <- dplyr::group_by(df, sim, scenario) %>%
    dplyr::mutate(cum_ben = cumsum(discount_netben),
                  cum_harv = cumsum(harvest),
                  cum_rec = cumsum(rec)) %>%
    dplyr::group_by(t, scenario) %>% # now group by scenario and calculate the quartiles at each time step
    dplyr::summarise(lo25_ben = quantile(cum_ben, quantiles[1]),
                     hi75_ben = quantile(cum_ben, quantiles[2]),
                     m_ben = mean(cum_ben),
                     med_ben = median(cum_ben),
                     ann_ben_lo25 = quantile(discount_netben, quantiles[1]),
                     ann_ben_hi75 = quantile(discount_netben, quantiles[2]),
                     ann_ben_50 = quantile(discount_netben, 0.5),
                     ann_ben_mean = mean(discount_netben),
                     lo25_h = quantile(cum_harv, quantiles[1]),
                     hi75_h = quantile(cum_harv, quantiles[2]),
                     m_h = mean(cum_harv),
                     med_h = median(cum_harv),
                     ann_harvest_lo25 = quantile(harvest, quantiles[1]),
                     ann_harvest_hi75 = quantile(harvest, quantiles[2]),
                     ann_harvest_50 = quantile(harvest, 0.5),
                     ann_harvest_mean = mean(harvest),
                     lo25_rec = quantile(cum_rec, quantiles[1]),
                     hi75_rec = quantile(cum_rec, quantiles[2]),
                     m_rec = mean(cum_rec),
                     med_rec = median(cum_rec),
                     ann_rec_lo25 = quantile(rec, quantiles[1]),
                     ann_rec_hi75 = quantile(rec, quantiles[2]),
                     ann_rec_50 = quantile(rec, 0.5),
                     ann_rec_mean = mean(rec),
                     ann_esc= rec-harvest,
                     ann_esc_lo25 = quantile(ann_esc, quantiles[1]),
                     ann_esc_hi75 = quantile(ann_esc, quantiles[2]),
                     ann_esc_50 = quantile(ann_esc, 0.5),
                     ann_esc_mean=mean(ann_esc)
    )
  return(summary)
}


# Jan: calculate metric of stabiltiy (CV or 1/CV)
# Jan: compare all scenarios relative to fixed pre
# Toby: include non-linear relationship between price and participation (harvest)
# Dave: compare short v long management response
