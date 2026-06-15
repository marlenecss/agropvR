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
#' @param cache_dir path to cache folder (default "data/cache")
#' @return patchwork object with three maps side by side
#' @export
#'
map_indices <- function(site_id,
                        month,
                        cache_dir = "cache",
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

  # check for cached raster first
  label  <- paste0("site_", site_id, "_", month)
  tif_fp <- file.path(cache_dir, paste0(label, ".tif"))

  if (file.exists(tif_fp)) {
    message("Loading cached raster for site ", site_id, " month ", month)
    indices <- terra::rast(tif_fp)
  } else {
    message("Downloading Sentinel-2 bands for site ", site_id,
            " month ", month, " - this may take a few minutes...")

    scene <- search_sentinel(site_sf, month)

    if (is.null(scene)) {
      stop("No suitable scene found for site ", site_id, " month ", month,
           " - try a different month or raise the cloud cover threshold.")
    }

    bands   <- load_bands(scene, buffer_sf)
    indices <- calc_indices(bands)
  }

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

  # format title month
  month_label <- format(as.Date(paste0(month, "-01")), "%B %Y")

  # --- admin boundaries for BY and BW ---
  germany <- rnaturalearth::ne_states(
    country     = "Germany",
    returnclass = "sf"
  )

  by_bw <- germany[germany$name %in% c("Bayern", "Baden-Wuerttemberg"), ]
  by_bw <- sf::st_transform(by_bw, 4326)

  # site centroid for overview map
  site_wgs    <- sf::st_transform(site_sf, 4326)
  site_coords <- as.data.frame(
    sf::st_coordinates(sf::st_centroid(site_wgs))
  )
  colnames(site_coords) <- c("lon", "lat")

  # --- OSM basemap for overview ---
  bbox_bybw <- sf::st_bbox(by_bw)

  osm_tile <- maptiles::get_tiles(
    by_bw,
    provider = "OpenStreetMap",
    zoom     = 7,
    crop     = TRUE
  )

  osm_df <- as.data.frame(
    terra::as.data.frame(osm_tile, xy = TRUE)
  )
  colnames(osm_df)[1:2] <- c("x", "y")

  # --- index maps (no legend - collected separately) ---
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
      ggplot2::scale_fill_distiller(
        palette   = palette,
        direction = 1,
        na.value  = "transparent",
        name      = legend_title,
        guide     = ggplot2::guide_colorbar(
          barwidth       = 0.6,
          barheight      = 6,
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
        legend.position  = "none"   # hide individual legends
      ) +
      ggplot2::coord_sf()
  }

  p_ndvi <- make_map("NDVI", palette = "Greens",  legend_title = "NDVI")
  p_ndmi <- make_map("NDMI", palette = "Blues",   legend_title = "NDMI")
  p_bsi  <- make_map("BSI",  palette = "YlOrBr",  legend_title = "BSI")

  # --- overview map with OSM basemap and BY/BW borders ---
  p_loc <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data    = osm_df,
      mapping = ggplot2::aes(x = x, y = y, fill = NULL),
      fill    = "grey90"
    ) +
    tidyterra::geom_spatraster_rgb(data = osm_tile) +
    ggplot2::geom_sf(
      data      = by_bw,
      fill      = NA,
      colour    = "grey30",
      linewidth = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data    = site_coords,
      mapping = ggplot2::aes(x = lon, y = lat),
      colour  = "red",
      size    = 3
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox_bybw["xmin"], bbox_bybw["xmax"]),
      ylim = c(bbox_bybw["ymin"], bbox_bybw["ymax"])
    ) +
    ggplot2::labs(title = "Location") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank()
    )

  # --- build combined legend as a separate ggplot ---
  # colour bar data for each index
  legend_ramp <- function(palette, label) {
    df <- data.frame(
      x     = 1,
      y     = seq(0, 1, length.out = 100),
      value = seq(0, 1, length.out = 100)
    )
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(
        palette   = palette,
        direction = 1,
        name      = label,
        guide     = ggplot2::guide_colorbar(
          barwidth       = 0.6,
          barheight      = 5,
          title.position = "top",
          title.hjust    = 0.5
        )
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position  = "right",
        legend.title     = ggplot2::element_text(size = 9),
        legend.text      = ggplot2::element_text(size = 8)
      )
  }

  # point legend for site and field outlines
  p_point_legend <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data    = data.frame(
        x     = c(1, 1),
        y     = c(2, 1),
        label = c(paste("Site", site_id), "Reference field")
      ),
      mapping = ggplot2::aes(x = x, y = y, colour = label),
      size    = 3,
      show.legend = TRUE
    ) +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = c(
        "Reference field"            = "#3399FF",
        setNames("red", paste("Site", site_id))
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.text     = ggplot2::element_text(size = 9)
    )

  # extract legends only using cowplot
  get_legend <- function(p) cowplot::get_legend(p)

  leg_ndvi  <- get_legend(
    make_map("NDVI", "Greens", "NDVI") +
      ggplot2::theme(legend.position = "right")
  )
  leg_ndmi  <- get_legend(
    make_map("NDMI", "Blues",  "NDMI") +
      ggplot2::theme(legend.position = "right")
  )
  leg_bsi   <- get_legend(
    make_map("BSI",  "YlOrBr", "BSI") +
      ggplot2::theme(legend.position = "right")
  )
  leg_pts   <- get_legend(p_point_legend)

  # stack legends vertically
  combined_legend <- cowplot::plot_grid(
    leg_ndvi, leg_ndmi, leg_bsi, leg_pts,
    ncol    = 1,
    align   = "v",
    rel_heights = c(1, 1, 1, 0.6)
  )

  # --- final layout ---
  maps_row <- p_ndvi + p_ndmi + p_bsi

  bottom_row <- patchwork::wrap_plots(p_loc) |
    patchwork::wrap_elements(combined_legend)

  (maps_row / bottom_row) +
    patchwork::plot_annotation(
      title = paste0("Indices Site ", site_id, ", ", month_label),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
}
