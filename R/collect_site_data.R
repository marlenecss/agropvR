#' COLLECT SITE DATA
#' Download and cache all monthly data for one agrovoltaic site
#'
#' Loops April to October automatically. Safe to re-run — skips already cached months.
#'
#' @param site_sf polygon of the agrovoltaic facility
#' @param field_sf polygon of the paired reference field
#' @param site_id label used for cache filenames
#' @param cache_dir path to folder for .rds files (default "data/cache")
#' @param months vector of "YYYY-MM" strings; defaults to Apr–Oct 2024
#' @return invisible NULL; results are saved to cache_dir as .rds files
#' @export
collect_site_data <- function(site_sf,
                              field_sf,
                              site_id,
                              cache_dir = "data/cache",
                              months = c(paste0("2024-0", 4:9), "2024-10")) {

  # create cache folder if it doesn't exist
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # buffer around field (not site) for imagery download
  buffer_sf <- sf::st_buffer(
    sf::st_transform(field_sf, 25832),
    dist = 300
  )

  # loop over months
  for (m in months) {
    label    <- paste0("site_", site_id, "_", m)
    cache_fp <- file.path(cache_dir, paste0(label, ".rds"))

    # skip if already cached
    if (file.exists(cache_fp)) {
      message("Already cached: ", label)
      next
    }

    message("Processing: ", label)

    result <- tryCatch(
      compare_site(
        site_sf   = site_sf,
        field_sf  = field_sf,
        buffer_sf = buffer_sf,
        month     = m
      ),
      error = function(e) {
        message("  FAILED: ", conditionMessage(e))
        NULL
      }
    )

    # save result if successful
    if (!is.null(result)) {
      result$site_id <- site_id
      result$month   <- m
      saveRDS(result, cache_fp)
      message("  Saved: ", cache_fp)
    }
  }

  invisible(NULL)
}
