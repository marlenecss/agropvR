#' LOAD SITE RESULTS
#' Load cached results for a site, optionally filtering by month
#'
#' @param site_id Integer site identifier matching the one used in collect_site_data()
#' @param months Optional character vector of "YYYY-MM" to filter; NULL returns all months
#' @param cache_dir Path to cache folder (must match the one used in collect_site_data())
#' @return A data.frame with one row per month
#' @export
load_site_results <- function(site_id,
                              months = NULL,
                              cache_dir = "cache") {

  all_months <- c(paste0("2024-0", 4:9), "2024-10")
  selected   <- if (is.null(months)) all_months else months

  found   <- character(0)
  missing <- character(0)

  results <- lapply(selected, function(m) {
    fp <- file.path(cache_dir, paste0("site_", site_id, "_", m, ".rds"))

    if (!file.exists(fp)) {
      missing <<- c(missing, m)
      return(NULL)
    }

    found <<- c(found, m)
    readRDS(fp)
  })

  if (length(found) > 0) {
    message("Loaded cached data for site ", site_id, ": ",
            paste(found, collapse = ", "))
  }

  if (length(missing) > 0) {
    message("No cached data for site ", site_id, ": ",
            paste(missing, collapse = ", "),
            " - run collect_site_data() if you want this data")
  }

  dplyr::bind_rows(results)
}
