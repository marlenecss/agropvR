#' COLLECT SITE DATA
#' Download and cache data for one agrovoltaic site and one month
#'
#' Safe to re-run — skips if that month is already cached.
#'
#' @param site_sf polygon of the agrovoltaic facility
#' @param field_sf polygon of the paired reference field
#' @param site_id label used for cache filenames
#' @param month character string "YYYY-MM", e.g. "2024-07"
#' @param cache_dir path to folder where .rds files are saved (default "data/cache")
#' @return invisible NULL; result is saved to cache_dir as an .rds file
#' @export
collect_site_data <- function(site_sf,
                              field_sf,
                              site_id,
                              month,
                              cache_dir = "cache") {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  buffer_sf <- sf::st_buffer(
    sf::st_transform(field_sf, 25832),
    dist = 300
  )

  label    <- paste0("site_", site_id, "_", month)
  cache_fp <- file.path(cache_dir, paste0(label, ".rds"))
  tif_fp   <- file.path(cache_dir, paste0(label, ".tif"))

  # skip if both files already exist
  if (file.exists(cache_fp) && file.exists(tif_fp)) {
    message("Already cached: ", label)
    return(invisible(NULL))
  }

  message("Processing: ", label)

  # find scene
  scene <- search_sentinel(site_sf, month)

  if (is.null(scene)) {
    message("  FAILED: no suitable scene found")
    return(invisible(NULL))
  }

  # download bands
  bands <- tryCatch(
    load_bands(scene, buffer_sf),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(bands)) return(invisible(NULL))

  # compute indices
  indices <- calc_indices(bands)

  # save raster .tif
  if (!file.exists(tif_fp)) {
    terra::writeRaster(indices, tif_fp, overwrite = TRUE)
    message("  Saved raster: ", tif_fp)
  }

  # save statistics .rds
  if (!file.exists(cache_fp)) {
    result <- tryCatch(
      extract_stats(indices, site_sf, field_sf, month),
      error = function(e) {
        message("  FAILED extracting stats: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result)) {
      result$site_id <- site_id
      result$month   <- month
      saveRDS(result, cache_fp)
      message("  Saved stats: ", cache_fp)
    }
  }

  invisible(NULL)
}
