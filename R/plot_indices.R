#' PLOT INDICES
#' Plot index time series comparing agrovoltaic site vs reference field
#'
#' Produces a line plot of NDVI, NDMI, and BSI across months,
#' with mean lines and min/max shaded bands for site and reference field.
#'
#' @param results data.frame returned by load_site_results()
#' @param site_id site id, used only for the plot title
#' @return ggplot object
#' @export
plot_indices <- function(results, site_id = NULL) {

  # convert month column to date for proper time axis
  results$date <- as.Date(paste0(results$month, "-15"))

  # reshape to long format for faceting
  long <- tidyr::pivot_longer(
    results,
    cols = c(
      ndvi_site_mean, ndvi_ref_mean,
      ndmi_site_mean, ndmi_ref_mean,
      bsi_site_mean,  bsi_ref_mean,
      ndvi_site_sd,   ndvi_ref_sd,
      ndmi_site_sd,   ndmi_ref_sd,
      bsi_site_sd,    bsi_ref_sd
    ),
    names_to = c("index", "type", "stat"),
    names_pattern = "(.*)_(site|ref)_(mean|sd)"
  )

  # split mean and sd, then calculate min/max as mean +/- sd
  means <- long[long$stat == "mean", ]
  sds   <- long[long$stat == "sd",   ]

  means$sd  <- sds$value
  means$ymin <- means$value - means$sd
  means$ymax <- means$value + means$sd
  means$index <- toupper(means$index)
  means$type  <- ifelse(means$type == "site", "Agrovoltaic site", "Reference field")

  title <- if (!is.null(site_id)) paste("Site", site_id, "- vegetation indices over time") else
    "Vegetation indices over time"

  ggplot2::ggplot(means, ggplot2::aes(x = date, y = value, colour = type, fill = type)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      alpha = 0.15, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_wrap(~ index, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::scale_colour_manual(values = c("Agrovoltaic site" = "#1D9E75", "Reference field" = "#378ADD")) +
    ggplot2::scale_fill_manual(  values = c("Agrovoltaic site" = "#1D9E75", "Reference field" = "#378ADD")) +
    ggplot2::labs(
      title   = title,
      x       = NULL,
      y       = "Index value",
      colour  = NULL,
      fill    = NULL,
      caption = "Shaded band = mean \u00b1 1 SD. Source: Sentinel-2 L2A."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position   = "top",
      strip.text        = ggplot2::element_text(face = "bold"),
      panel.grid.minor  = ggplot2::element_blank()
    )
}
