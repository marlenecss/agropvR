#' MAP INDICES
#' Map NDVI, NDMI, and BSI for one agrovoltaic site and month
#'
#' Downloads Sentinel-2 imagery and plots three index maps side by side,
#' with site and reference field outlines overlaid.
#'
#' @param site_id site identifier
#' @param month character string "YYYY-MM", e.g. "2024-07"
#' @param sites_sf the sites sf object included in the package (sites)
#' @param fields_sf the fields sf object included in the package (fields)
#' @return patchwork object with three maps side by side
#' @export
#'
map_indices <- function(site_id,
                        month,
                        sites_sf  = agropvR::sites,
                        fields_sf = agropvR::fields) {

  message("Downloading Sentinel-2 bands for site ", site_id,
          " month ", month, " - this may take a few minutes...")

  site_sf  <- sites_sf[site_id, ]
  field_sf <- fields_sf[site_id, ]

  # build buffer around field
  buffer_sf <- sf::st_buffer(
    sf::st_transform(field_sf, 25832),
    dist = 300
  )

  # download bands and compute indices
  scene <- search_sentinel(site_sf, month)

  if (is.null(scene)) {
    stop("No suitable scene found for site ", site_id, " month ", month,
         " - try a different month or raise the cloud cover threshold.")
  }

  bands   <- load_bands(scene, buffer_sf)
  indices <- calc_indices(bands)

  # reproject polygons to match raster CRS
  crs_rast    <- terra::crs(indices)
  site_plot   <- sf::st_transform(site_sf,   crs_rast)
  field_plot  <- sf::st_transform(field_sf,  crs_rast)
  buffer_plot <- sf::st_transform(buffer_sf, crs_rast)

  # crop indices to buffer
  indices <- terra::crop(indices, terra::vect(buffer_plot))

  # convert each layer to data.frame for ggplot
  make_df <- function(layer_name) {
    r  <- indices[[layer_name]]
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "value")
    df$index <- layer_name
    df
  }

  raster_df <- do.call(rbind, lapply(c("NDVI", "NDMI", "BSI"), make_df))

  site_df  <- sf::st_as_sf(site_plot)
  field_df <- sf::st_as_sf(field_plot)

  # format title month e.g. "July 2024"
  month_label <- format(as.Date(paste0(month, "-01")), "%B %Y")

  # dummy data for outline legend
  legend_df <- data.frame(
    label    = c("Agrovoltaic site", "Reference field"),
    colour   = c("red", "#3399FF")
  )

  make_map <- function(index_name, palette, legend_title) {
    df <- raster_df[raster_df$index == index_name, ]

    ggplot2::ggplot() +
      ggplot2::geom_raster(
        data    = df,
        mapping = ggplot2::aes(x = x, y = y, fill = value)
      ) +
      ggplot2::geom_sf(
        data        = site_df,
        fill        = NA,
        colour      = "red",
        linewidth   = 0.8,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_sf(
        data        = field_df,
        fill        = NA,
        colour      = "#3399FF",
        linewidth   = 0.8,
        inherit.aes = FALSE
      ) +
      # dummy lines for outline legend
      ggplot2::geom_line(
        data        = data.frame(x = NA_real_, y = NA_real_, label = "Agrovoltaic site"),
        mapping     = ggplot2::aes(x = x, y = y, colour = label)
      ) +
      ggplot2::geom_line(
        data        = data.frame(x = NA_real_, y = NA_real_, label = "Reference field"),
        mapping     = ggplot2::aes(x = x, y = y, colour = label)
      ) +
      ggplot2::scale_colour_manual(
        name   = NULL,
        values = c("Agrovoltaic site" = "red", "Reference field" = "#3399FF")
      ) +
      ggplot2::scale_fill_distiller(
        palette   = palette,
        direction = 1,
        na.value  = "transparent",
        name      = legend_title,
        guide     = ggplot2::guide_colorbar(
          barwidth  = 8,
          barheight = 0.6,
          title.position = "top",
          title.hjust    = 0.5
        )
      ) +
      ggplot2::labs(title = index_name) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text        = ggplot2::element_text(size = 7),
        strip.text       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position  = "bottom",
        legend.text      = ggplot2::element_text(size = 9),
        legend.title     = ggplot2::element_text(size = 9),
        legend.key.width = ggplot2::unit(1.5, "cm")
      ) +
      ggplot2::coord_sf()
  }

  p_ndvi <- make_map("NDVI", palette = "Greens",  legend_title = "NDVI")
  p_ndmi <- make_map("NDMI", palette = "Blues",   legend_title = "NDMI")
  p_bsi  <- make_map("BSI",  palette = "YlOrBr",  legend_title = "BSI")

  # location map — southern germany with site marked
  germany  <- rnaturalearth::ne_countries(
    scale       = 10,
    country     = "Germany",
    returnclass = "sf"
  )

  site_wgs <- sf::st_transform(site_sf, 4326)
  site_coords <- as.data.frame(sf::st_coordinates(sf::st_centroid(site_wgs)))
  colnames(site_coords) <- c("lon", "lat")

  p_loc <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data   = germany,
      fill   = "grey90",
      colour = "grey60",
      linewidth = 0.3
    ) +
    ggplot2::geom_point(
      data    = site_coords,
      mapping = ggplot2::aes(x = lon, y = lat),
      colour  = "red",
      size    = 3
    ) +
    ggplot2::coord_sf(xlim = c(8.5, 13.5), ylim = c(47.2, 50.5)) +
    ggplot2::labs(title = "Location") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank()
    )

  # combine all four panels
  (p_ndvi + p_ndmi + p_bsi + p_loc) +
    patchwork::plot_annotation(
      title   = paste0("Indices Site ", site_id, ", ", month_label),
      theme   = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
}
