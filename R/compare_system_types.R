#' COMPARE SYSTEM TYPES
#' Compare index statistics between agrovoltaic system types
#'
#' @param results data.frame returned by load_site_results()
#' @param sites_sf the sites sf object included in the package (sites)
#' @return a summary data.frame with mean and stdev per index per system type
#' @export
compare_system_types <- function(results, sites_sf = agropvR::sites) {

  # join system type from sites to results
  system_info <- data.frame(
    site_id    = seq_len(nrow(sites_sf)),
    system_typ = sites_sf$system_typ
  )

  df <- merge(results, system_info, by = "site_id")

  # summarise per system type
  summary <- do.call(rbind, lapply(
    split(df, df$system_typ),
    function(g) {
      data.frame(
        system_typ     = unique(g$system_typ),

        ndvi_site_mean = base::mean(g$ndvi_site_mean, na.rm = TRUE),
        ndvi_site_sd   = state::sd(g$ndvi_site_mean,   na.rm = TRUE),
        ndvi_ref_mean  = base::mean(g$ndvi_ref_mean,  na.rm = TRUE),
        ndvi_ref_sd    = stats::sd(g$ndvi_ref_mean,    na.rm = TRUE),

        ndmi_site_mean = mean(g$ndmi_site_mean, na.rm = TRUE),
        ndmi_site_sd   = sd(g$ndmi_site_mean,   na.rm = TRUE),
        ndmi_ref_mean  = mean(g$ndmi_ref_mean,  na.rm = TRUE),
        ndmi_ref_sd    = sd(g$ndmi_ref_mean,    na.rm = TRUE),

        bsi_site_mean  = mean(g$bsi_site_mean,  na.rm = TRUE),
        bsi_site_sd    = sd(g$bsi_site_mean,    na.rm = TRUE),
        bsi_ref_mean   = mean(g$bsi_ref_mean,   na.rm = TRUE),
        bsi_ref_sd     = sd(g$bsi_ref_mean,     na.rm = TRUE)
      )
    }
  ))

  rownames(summary) <- NULL
  summary
}
