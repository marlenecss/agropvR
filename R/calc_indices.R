#' CALCULATE INDICES
#' Calculate NDVI, NDMI, and BSI from Sentinel-2 bands
#'
#' @param bands named list returned by load_bands()
#' @return a SpatRaster with three layers: NDVI, NDMI, BSI
#' @export
calc_indices <- function(bands) {
  ndvi <- (bands$nir  - bands$red)  / (bands$nir  + bands$red)
  ndmi <- (bands$nir  - bands$swir) / (bands$nir  + bands$swir)
  bsi  <- ((bands$swir + bands$red) - (bands$nir + bands$blue)) /
    ((bands$swir + bands$red) + (bands$nir + bands$blue))

  result <- c(ndvi, ndmi, bsi)
  names(result) <- c("NDVI", "NDMI", "BSI")
  result
}
