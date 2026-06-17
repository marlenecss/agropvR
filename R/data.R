#' DATA
#' Agrovoltaic facility polygons
#'
#' Digitised polygons of 25 agrovoltaic facilities in Bavaria and
#' Baden-Wuerttemberg, Germany.
#'
#' @format an sf object with 25 rows and a geometry column (CRS: EPSG 25832)
#' @source https://agrivoltaicsmap.ise.fraunhofer.de/
"sites"

#' Reference agricultural field polygons
#'
#' Paired reference fields for each of the 25 agrovoltaic sites.
#'
#' @format an sf object with 25 rows and a geometry column (CRS: EPSG 25832)
#' @source https://agrivoltaicsmap.ise.fraunhofer.de/
"fields"

#' Photovoltaic panel footprints
#'
#' Digitised polygons of individual photovoltaic panel arrays within
#' each agrovoltaic site. Multiple polygons per site, linked via site_id.
#'
#' @format An sf object with 655 rows and a geometry column (CRS: EPSG 25832)
#' @source Digitised in QGIS using satellite basemap imagery
"panels"
