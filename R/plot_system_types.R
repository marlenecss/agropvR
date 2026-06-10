#' PLOT SYSTEM TYPES
#' Plot index comparison between agrovoltaic system types
#'
#' @param results data.frame returned by load_site_results()
#' @param sites_sf the sites sf object included in the package (sites)
#' @return ggplot object
#' @export
plot_system_types <- function(results, sites_sf = agropvR::sites) {

  system_info <- data.frame(
    site_id    = seq_len(nrow(sites_sf)),
    system_typ = sites_sf$system_typ
  )

  df <- merge(results, system_info, by = "site_id")

  # reshape to long — site values only (comparing system type impact on crops)
  long <- tidyr::pivot_longer(
    df,
    cols      = c(ndvi_site_mean, ndmi_site_mean, bsi_site_mean),
    names_to  = "index",
    values_to = "mean"
  )

  sd_long <- tidyr::pivot_longer(
    df,
    cols      = c(ndvi_site_sd, ndmi_site_sd, bsi_site_sd),
    names_to  = "index_sd",
    values_to = "sd"
  )

  long$sd    <- sd_long$sd
  long$index <- toupper(gsub("_site_mean", "", long$index))

  # summarise by system type and index
  agg <- do.call(rbind, lapply(
    split(long, list(long$system_typ, long$index)),
    function(g) {
      data.frame(
        system_typ = unique(g$system_typ),
        index      = unique(g$index),
        mean       = base::mean(g$mean, na.rm = TRUE),
        sd         = stats::sd(g$mean,   na.rm = TRUE)
      )
    }
  ))

  ggplot2::ggplot(agg, ggplot2::aes(x = system_typ, y = mean, fill = system_typ)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
      width = 0.2
    ) +
    ggplot2::facet_wrap(~ index, ncol = 3, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = c("overhead" = "#1D9E75", "interspace" = "#378ADD")
    ) +
    ggplot2::labs(
      title   = "Index values by agrovoltaic system type",
      x       = NULL,
      y       = "Mean index value",
      fill    = "System type",
      caption = "Error bars = SD across sites. Sentinel-2 L2A."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "top",
      strip.text       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
