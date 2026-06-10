#' COMPARE SITE
#' Run the full environmental impact analysis for one site and one month
#'
#' Called internally by collect_site_data(). Can also be used standalone.
#'
#' @param site_sf polygon of the agrovoltaic facility
#' @param field_sf polygon of the paired reference field
#' @param buffer_sf buffered polygon around field_sf, used as imagery crop extent
#' @param month character string "YYYY-MM", e.g. "2024-07"
#' @return data.frame with mean and stdev for NDVI, NDMI, and BSI
#' @export
compare_site <- function(site_sf, field_sf, buffer_sf, month) {

  # find best scene
  scene <- search_sentinel(site_sf, month)
  if (is.null(scene)) return(NULL)

  # load and crop bands using the buffered field extent
  bands <- load_bands(scene, buffer_sf)

  # calculate indices
  indices <- calc_indices(bands)

  # extract statistics for site and reference field
  extract_stats(indices, site_sf, field_sf, month)

}
