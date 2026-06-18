#' MAP INDICES
#' Map NDVI, NDMI, and BSI for one agrovoltaic site and month
#'
#' Loads unmasked index rasters for visual display, alongside a location
#' map and a panel footprint overview. Note: this map intentionally shows
#' the full unmasked scene for visual clarity. Statistics from
#' load_site_results() are based on masked data which excludes PV panel
#' pixels.
#'
#' @param site_id site identifier
#' @param month character string "YYYY-MM", e.g. "2024-07"
#' @param cache_dir path to folder with unmasked cached rasters (default "cache_unmasked")
#' @param sites_sf the sites sf object included in the package (sites)
#' @param fields_sf the fields sf object included in the package (fields)
#' @param panels_sf the panels sf object included in the package (panels)
#' @return patchwork object with three index maps and two overview maps
#' @export
map_indices <- function(site_id,
                        month,
                        cache_dir = "cache_unmasked",
                        sites_sf  = agropvR::sites,
                        fields_sf = agropvR::fields,
                        panels_sf = agropvR::panels) {

  site_sf  <- sf::st_as_sf(sites_sf[site_id, , drop = FALSE])
  field_sf <- sf::st_as_sf(fields_sf[site_id, , drop = FALSE])

  buffer_sf <- sf::st_buffer(
    sf::st_transform(field_sf, 25832),
    dist = 300
  )

  # check for cached unmasked raster
  label  <- paste0("site_", site_id, "_", month)
  tif_fp <- file.path(cache_dir, paste0(label, ".tif"))

  if (file.exists(tif_fp)) {
    message("Loading unmasked cached raster for site ", site_id, " month ", month)
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

  site_panels <- panels_sf[panels_sf$site_id == site_id, ]

  if (nrow(site_panels) == 0) {
    message("No panel polygons found for site ", site_id)
    panels_plot <- NULL
  } else {
    panels_plot <- sf::st_transform(site_panels, crs_rast)
  }

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

  site_df   <- sf::st_as_sf(site_plot)
  field_df  <- sf::st_as_sf(field_plot)
  panels_df <- if (!is.null(panels_plot)) sf::st_as_sf(panels_plot) else NULL

  p_panels <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = site_df, fill = NA, colour = "red", linewidth = 1, inherit.aes = FALSE)

  if (!is.null(panels_df)) {
    p_panels <- p_panels +
      ggplot2::geom_sf(data = panels_df, fill = "grey40", colour = "grey20", alpha = 0.6, inherit.aes = FALSE)
  }

  p_panels <- p_panels +
    ggplot2::labs(title = "Panel footprint", x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 7), panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::coord_sf()
  month_label <- format(as.Date(paste0(month, "-01")), "%B %Y")

  # --- admin boundaries for BY and BW ---
  germany <- tryCatch(
    rnaturalearth::ne_states(country = "Germany", returnclass = "sf"),
    error = function(e) {
      message("High-res boundaries unavailable, using country-level outline instead")
      rnaturalearth::ne_countries(country = "Germany", returnclass = "sf")
    }
  )

  by_bw <- germany[germany$name %in% c(
    "Bayern",
    "Baden-W\u00fcrttemberg",
    "Baden-Wurttemberg",
    "Baden-Wuerttemberg"
  ), ]
  by_bw <- sf::st_transform(by_bw, 4326)

  site_wgs    <- sf::st_transform(site_sf, 4326)
  site_coords <- as.data.frame(
    sf::st_coordinates(sf::st_centroid(site_wgs))
  )
  colnames(site_coords) <- c("lon", "lat")

  bbox_bybw <- sf::st_bbox(by_bw)

  osm_tile <- maptiles::get_tiles(
    by_bw,
    provider = "OpenStreetMap",
    zoom     = 7,
    crop     = TRUE
  )

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
          barwidth       = 8,
          barheight      = 0.5,
          title.position = "top",
          title.hjust    = 0.5,
          direction      = "horizontal"
        )
      ) +
      ggplot2::labs(
        title = index_name,
        x     = "Longitude",
        y     = "Latitude"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text        = ggplot2::element_text(size = 7),
        strip.text       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position  = "none"
      ) +
      ggplot2::coord_sf()
  }

  p_ndvi <- make_map("NDVI", palette = "Greens",  legend_title = "NDVI")
  p_ndmi <- make_map("NDMI", palette = "Blues",   legend_title = "NDMI")
  p_bsi  <- make_map("BSI",  palette = "YlOrBr",  legend_title = "BSI")

  # --- overview map: location in BY/BW ---
  p_loc <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = osm_tile) +
    ggplot2::geom_sf(
      data        = by_bw,
      fill        = NA,
      colour      = "grey20",
      linewidth   = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data    = site_coords,
      mapping = ggplot2::aes(x = lon, y = lat),
      colour  = "red",
      fill    = "red",
      shape   = 8,
      size    = 5,
      stroke  = 2
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox_bybw["xmin"], bbox_bybw["xmax"]),
      ylim = c(bbox_bybw["ymin"], bbox_bybw["ymax"])
    ) +
    ggplot2::labs(
      title = "Location",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank()
    )

  # --- overview map: site outline + panel footprints ---
  p_panels <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data        = site_df,
      fill        = NA,
      colour      = "red",
      linewidth   = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data        = panels_df,
      fill        = "grey40",
      colour      = "grey20",
      alpha       = 0.6,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title = "Panel footprint",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::coord_sf()

  # --- extract legends ---
  get_legend <- function(p) cowplot::get_legend(p)

  leg_ndvi <- get_legend(
    make_map("NDVI", "Greens", "NDVI") +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(2, "cm")
      )
  )
  leg_ndmi <- get_legend(
    make_map("NDMI", "Blues", "NDMI") +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(2, "cm")
      )
  )
  leg_bsi <- get_legend(
    make_map("BSI", "YlOrBr", "BSI") +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.key.width = ggplot2::unit(2, "cm")
      )
  )

  # site and field outline legend
  p_point_legend <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data    = data.frame(
        x     = c(1, 1),
        y     = c(2, 1),
        label = c(paste("Site", site_id), "Reference field")
      ),
      mapping     = ggplot2::aes(x = x, y = y, colour = label),
      size        = 3,
      show.legend = TRUE
    ) +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = c(
        "Reference field" = "#3399FF",
        setNames("red", paste("Site", site_id))
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text     = ggplot2::element_text(size = 9)
    )

  leg_pts <- get_legend(p_point_legend)

  legend_top <- cowplot::plot_grid(
    leg_ndvi, leg_ndmi, leg_bsi,
    nrow  = 1,
    align = "h"
  )

  legend_bottom <- cowplot::plot_grid(
    NULL, leg_pts, NULL,
    nrow       = 1,
    rel_widths = c(1, 1, 1)
  )

  combined_legend <- cowplot::plot_grid(
    legend_top,
    legend_bottom,
    ncol        = 1,
    rel_heights = c(1, 0.4)
  )

  # --- final layout ---
  maps_row <- patchwork::wrap_plots(p_ndvi, p_ndmi, p_bsi, nrow = 1)

  overview_row <- patchwork::wrap_plots(p_loc, p_panels, nrow = 1)

  bottom_row <- patchwork::wrap_plots(overview_row) |
    patchwork::wrap_elements(combined_legend)

  (maps_row / bottom_row) +
    patchwork::plot_annotation(
      title = paste0("Indices Site ", site_id, ", ", month_label),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
}
