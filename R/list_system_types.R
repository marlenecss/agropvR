#' LIST SYSTEM TYPES
#' Show which sites are overhead vs interspace systems
#'
#' @param sites_sf the sites sf object included in the package (sites)
#' @return a data.frame with site_id and system_typ
#' @export
list_system_types <- function(sites_sf = agropvR::sites) {
  data.frame(
    site_id    = seq_len(nrow(sites_sf)),
    system_typ = sites_sf$system_typ
  )
}
