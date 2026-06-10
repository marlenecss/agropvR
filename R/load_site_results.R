#' LOAD SITE RESULTS
#' Load cached results for a site, optionally filtering by month
#'
#' @param site_id site identifier matching the one used in collect_site_data()
#' @param months optional character vector of "YYYY-MM" to filter; NULL returns all months
#' @param cache_dir path to cache folder (must match the one used in collect_site_data())
#' @return data.frame with one row per month
#' @export
load_site_results <- function(site_id,
                              months = NULL,
                              cache_dir = "data/cache") {

  all_months <- c(paste0("2024-0", 4:9), "2024-10")
  selected   <- if (is.null(months)) all_months else months

  results <- lapply(selected, function(m) {
    fp <- file.path(cache_dir, paste0("site_", site_id, "_", m, ".rds"))

    if (!file.exists(fp)) {
      message("No cached data for site ", site_id, " month ", m,
              " - run collect_site_data() first")
      return(NULL)
    }

    readRDS(fp)
  })

  dplyr::bind_rows(results)
}
