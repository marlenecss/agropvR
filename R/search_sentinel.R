#' SEARCH SENTINEL
#' Find the least cloudy Sentinel-2 scene for a site and month
#'
#' @param max_cloud maximum cloud cover (%)
#' @param bbox_sf object, its bounding box is used for the STAC search
#' @param month character string in "YYYY-MM" format, e.g. "2024-07"
#' @param limit maximum number of scenes to consider (default 20)
#' @return single STAC feature (list) for the best scene
#' @export
search_sentinel <- function(bbox_sf, month, limit = 20, max_cloud = 30) {
  bbox <- sf::st_bbox(sf::st_transform(bbox_sf, 4326))

  last_day <- lubridate::days_in_month(as.Date(paste0(month, "-01")))
  datetime_string <- paste0(month, "-01T00:00:00Z/", month, "-", last_day, "T23:59:59Z")

  sent <- rstac::stac("https://earth-search.aws.element84.com/v1")

  items <- sent |>
    rstac::stac_search(
      collections = "sentinel-2-l2a",
      bbox        = c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]),
      datetime    = datetime_string,
      limit       = limit
    ) |>
    rstac::post_request()

  cloud <- sapply(items$features, function(x) x$properties[["eo:cloud_cover"]])

  # filter out scenes above threshold
  good <- which(cloud <= max_cloud)

  if (length(good) == 0) {
    message("No scenes below ", max_cloud, "% cloud cover for ", month, " - skipping")
    return(NULL)
  }

  best <- good[which.min(cloud[good])]
  scene_date <- items$features[[best]]$properties$datetime

  message("Selected scene from ", substr(scene_date, 1, 10), "with",
          round(cloud[best], 1), "% cloud cover")
  items$features[[best]]
}
