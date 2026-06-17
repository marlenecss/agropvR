#' MASK PANELS
#' Mask out PV panel pixels from a raster
#'
#' @param r SpatRaster to mask (e.g. band or index raster)
#' @param site_id Integer site identifier matching the panels dataset
#' @param panels_sf The panels sf object included in the package (panels)
#' @return SpatRaster with panel pixels set to NA
#' @export
mask_panels <- function(r, site_id, panels_sf = agropvR::panels) {

  site_panels <- panels_sf[panels_sf$site_id == site_id, ]

  if (nrow(site_panels) == 0) {
    message("No panel polygons found for site ", site_id, " - returning unmasked raster")
    return(r)
  }

  panel_vect <- terra::vect(sf::st_transform(site_panels, terra::crs(r)))
  terra::mask(r, panel_vect, inverse = TRUE)
}
