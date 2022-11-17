#' summarize_plots returns basic plots of quantities of interest over time
#' @param summary_df A dataframe generated from `summarize_quantities()`
#' @return A list of plots
#'
#' @export
#' @import ggplot2
#'
summarize_plots = function(summary_df) {
  plot_list = list()

  plot_list[[1]] <- ggplot(summary_df, aes(t, m_ben, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=lo25_ben, ymax=hi75_ben), alpha=0.5, col=NA) +
    geom_line(aes(t,m_ben)) +
    geom_line(aes(t,med_ben), linetype="dashed") +
    ylab("Discounted net benefits, $M") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag")) +
    coord_cartesian(xlim=c(0,50))

  plot_list[[2]] <- ggplot(summary_df, aes(t, m_h, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=lo25_h, ymax=hi75_h), alpha=0.5, col=NA) +
    geom_line(aes(t,m_h)) +
    geom_line(aes(t,med_h), linetype="dashed") +
    ylab("Cumulative harvest") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag"))

  plot_list[[3]] <- ggplot(summary_df, aes(t, m_rec, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=lo25_rec, ymax=hi75_rec), alpha=0.5, col=NA) +
    geom_line(aes(t,m_rec)) +
    geom_line(aes(t,med_rec), linetype="dashed") +
    ylab("Cumulative recruitment") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag"))

  plot_list[[4]] <- ggplot(summary_df, aes(t, ann_ben_mean, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=ann_ben_lo25, ymax=ann_ben_hi75), alpha=0.5, col=NA) +
    geom_line(aes(t,ann_ben_mean)) +
    geom_line(aes(t,ann_ben_50), linetype="dashed") +
    ylab("Annual net benefits") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag")) +
    coord_cartesian(xlim=c(0,50))

  plot_list[[5]] <- ggplot(summary_df, aes(t, ann_harvest_mean, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=ann_harvest_lo25, ymax=ann_harvest_hi75), alpha=0.5, col=NA) +
    geom_line(aes(t,ann_harvest_mean)) +
    geom_line(aes(t,ann_harvest_50), linetype="dashed") +
    ylab("Annual harvest") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag")) +
    coord_cartesian(xlim=c(0,50))

  plot_list[[6]] <- ggplot(summary_df, aes(t, ann_rec_mean, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=ann_rec_lo25, ymax=ann_rec_hi75), alpha=0.5, col=NA) +
    geom_line(aes(t,ann_rec_mean)) +
    geom_line(aes(t,ann_rec_50), linetype="dashed") +
    ylab("Annual recruitment") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Time lag"),
           col=guide_legend(title="Time lag")) +
    coord_cartesian(xlim=c(0,50))

  plot_list[[7]] <- ggplot(summary_df, aes(t, ann_esc_mean, col = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin=ann_esc_lo25, ymax=ann_esc_hi75), alpha=0.5, col=NA) +
    geom_line(aes(t,ann_esc_mean))+
    ylab("Annual escapement") +
    xlab("Time") +
    scale_color_viridis_d(end=0.4) +
    scale_fill_viridis_d(end=0.4) +
    guides(fill=guide_legend(title="Recruitment sd"),
           col=guide_legend(title="Recruitment sd")) +
    coord_cartesian(xlim=c(0,50))
  return(plot_list)
}
