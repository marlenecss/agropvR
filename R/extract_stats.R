#' EXTRACT STATS
#' Extract mean and stdev of indices for site and reference field
#'
#' @param indices spatRaster returned by calc_indices()
#' @param site_sf polygon of the agrovoltaic facility
#' @param field_sf polygon of the reference agricultural field
#' @param month character string "YYYY-MM" label for the result row
#' @return one-row data.frame with mean/stdev for each index × location
#' @export
extract_stats <- function(indices, site_sf, field_sf, month) {
  # ensure CRS matches raster (UTM 32N)
  crs_str  <- terra::crs(indices, describe = TRUE)$code
  site_sf  <- sf::st_transform(site_sf,  as.integer(crs_str))
  field_sf <- sf::st_transform(field_sf, as.integer(crs_str))

  stats_for <- function(poly) {
    exactextractr::exact_extract(indices, poly, c("mean", "stdev"))
  }

  s <- stats_for(site_sf)
  r <- stats_for(field_sf)

  data.frame(
    month          = month,
    ndvi_site_mean = s$mean.NDVI,   ndvi_site_sd = s$stdev.NDVI,
    ndvi_ref_mean  = r$mean.NDVI,   ndvi_ref_sd  = r$stdev.NDVI,
    ndmi_site_mean = s$mean.NDMI,   ndmi_site_sd = s$stdev.NDMI,
    ndmi_ref_mean  = r$mean.NDMI,   ndmi_ref_sd  = r$stdev.NDMI,
    bsi_site_mean  = s$mean.BSI,    bsi_site_sd  = s$stdev.BSI,
    bsi_ref_mean   = r$mean.BSI,    bsi_ref_sd   = r$stdev.BSI
  )
}
