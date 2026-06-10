#' LOAD BANDS
#' Load and crop Sentinel-2 bands for a site
#'
#' @param scene STAC feature returned by search_sentinel()
#' @param buffer_sf polygon used to define the crop extent
#' @return named list with SpatRaster elements: red, nir, swir, blue
#' @export
load_bands <- function(scene, buffer_sf) {
  ext <- terra::ext(terra::vect(buffer_sf))

  red  <- terra::crop(terra::rast(scene$assets$red$href),    ext)
  nir  <- terra::crop(terra::rast(scene$assets$nir$href),    ext)
  blue <- terra::crop(terra::rast(scene$assets$blue$href),   ext)
  swir <- terra::crop(terra::rast(scene$assets$swir16$href), ext)

  # resample SWIR from 20 m to 10 m to match NIR
  swir <- terra::resample(swir, nir, method = "bilinear")

  list(red = red, nir = nir, blue = blue, swir = swir)
}
