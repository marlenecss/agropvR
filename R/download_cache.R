#' DOWNLOAD CACHE
#' Download pre-computed cache from Zenodo
#'
#' Downloads all cached .rds and .tif files from Zenodo.
#' Only needs to be run once. Skips files already present.
#'
#' @param cache_dir local folder to save files (default "cache")
#' @param zenodo_id character vector of Zenodo record IDs
#' @return invisible NULL
#' @export
download_cache <- function(cache_dir = "cache",
                           zenodo_id  = c("20713726", "20713763", "20713833")) {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  for (id in zenodo_id) {

    message("Fetching file list from Zenodo record ", id, "...")

    api_url  <- paste0("https://zenodo.org/api/records/", id)
    response <- httr::GET(api_url)
    content  <- httr::content(response, as = "parsed")

    # try inline files first (older API format)
    files <- content$files

    # fall back to separate files endpoint (newer API format)
    if (is.null(files) || length(files) == 0) {
      files_url <- content$links$files

      if (!is.null(files_url)) {
        files_response <- httr::GET(files_url)
        files_content  <- httr::content(files_response, as = "parsed")
        files <- files_content$entries
      }
    }

    if (is.null(files) || length(files) == 0) {
      message("  No files found for record ", id, " - skipping")
      next
    }

    message("Found ", length(files), " files")

    for (f in files) {
      fname <- f$key
      furl  <- f$links$content
      if (is.null(furl)) furl <- f$links$self

      dest_fp <- file.path(cache_dir, fname)

      if (file.exists(dest_fp)) {
        message("Already downloaded: ", fname)
        next
      }

      message("Downloading: ", fname)
      httr::GET(
        furl,
        httr::write_disk(dest_fp, overwrite = FALSE),
        httr::progress()
      )
    }
  }

  message("Done! Cache saved to: ", cache_dir)
  invisible(NULL)
}
