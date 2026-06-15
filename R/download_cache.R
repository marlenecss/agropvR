#' DOWNLOAD CACHE
#' Download pre-computed cache from Zenodo
#'
#' Downloads all cached .rds and .tif files from Zenodo.
#' Only needs to be run once. Skips files already present.
#'
#' @param cache_dir Local folder to save files (default "data/cache")
#' @param zenodo_id Zenodo record ID (default is the agropvR dataset)
#' @return Invisible NULL
#' @export
download_cache <- function(cache_dir  = "cache",
                           zenodo_id  = "20705323")
  {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # get file list from Zenodo API
  api_url  <- paste0("https://zenodo.org/api/records/", zenodo_id)
  response <- httr::GET(api_url)
  content  <- httr::content(response, as = "parsed")
  files    <- content$files

  message("Found ", length(files), " files on Zenodo")

  for (f in files) {
    fname   <- f$key
    furl    <- f$links$self
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

  message("Done! Cache saved to: ", cache_dir)
  invisible(NULL)
}
