remask_cache <- function(cache_dir = "cache") {

  tif_files <- list.files(cache_dir, pattern = "\\.tif$", full.names = TRUE)

  for (tif_fp in tif_files) {

    fname   <- basename(tif_fp)
    parts   <- regmatches(fname, regexpr("site_(\\d+)_(\\d{4}-\\d{2})", fname))
    site_id <- as.integer(sub("site_(\\d+)_.*", "\\1", parts))
    month   <- sub("site_\\d+_(.*)", "\\1", parts)

    message("Remasking: site ", site_id, " month ", month)

    indices <- terra::rast(tif_fp)

    # mask out PV panel pixels
    indices_masked <- tryCatch(
      mask_panels(indices, site_id = site_id),
      error = function(e) {
        message("  FAILED masking: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(indices_masked)) next

    # overwrite the tif with masked version
    terra::writeRaster(indices_masked, tif_fp, overwrite = TRUE)

    # recompute and overwrite stats
    site_sf  <- agropvR::sites[site_id, ]
    field_sf <- agropvR::fields[site_id, ]

    result <- tryCatch(
      extract_stats(indices_masked, site_sf, field_sf, month),
      error = function(e) {
        message("  FAILED extracting stats: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result)) {
      result$site_id <- site_id
      result$month   <- month
      rds_fp <- gsub("\\.tif$", ".rds", tif_fp)
      saveRDS(result, rds_fp)
      message("  Updated: ", basename(rds_fp))
    }
  }

  message("Done remasking all cached files.")
}
