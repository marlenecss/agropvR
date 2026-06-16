#' AVAILABLE SITES
#' List all site and month combinations available in the cache
#'
#' @param cache_dir path to cache folder (default "cache")
#' @return a data.frame with columns site_id and month
#' @export
available_sites <- function(cache_dir = "cache") {

  cache_files <- list.files(cache_dir, pattern = "\\.rds$")

  if (length(cache_files) == 0) {
    message("No cached data found in ", cache_dir,
            " - run download_cache() first")
    return(invisible(NULL))
  }

  result <- do.call(rbind, lapply(cache_files, function(f) {
    parts <- regmatches(f, regexpr("site_(\\d+)_(\\d{4}-\\d{2})", f))
    data.frame(
      site_id = as.integer(sub("site_(\\d+)_.*", "\\1", parts)),
      month   = sub("site_\\d+_(.*)\\.rds", "\\1", parts)
    )
  }))

  result <- result[order(result$site_id, result$month), ]
  rownames(result) <- NULL
  result
}
