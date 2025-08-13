#' @title `ggplot` maps
#' @description This function produces multi-panel plots of [`terra::SpatRaster`]s. It was designed to plot utilisation distributions (UDs) for multiple animals (rows) constructed by different algorithms (columns) in passive acoustic telemetry systems. UDs are expected to live on the disk.
#' @param .mapdt  A [`data.table::data.table`] that defines panel maps with the following columns: 
#' * `row`, `column` : row/column identifiers;
#' * `file_ud`       : a `character` that defines the path to the map, readable by [`terra::rast()`], or `NA`;
#' @param .map,.coast,.poly,.moorings Spatial layers.
#' * `.map` is a [`terra::SpatRaster`]. This is used to handle blank panels (panels where `.mapdt$file_ud` does not exist). 
#' * (optional) `.coast`, `.poly` Simple feature polygons (or similar) that define coastline and/or other relevant boundaries (e.g., a Marine Protected Area). Objects are coerced to simple features for plotting via [`sf::st_as_sf()`]. To plot individual-specific polygons, include `row` and `column` columns in `.poly` (as for `.mapdt`).
#' * (optional) `.moorings` A [`data.table::data.table`] of coordinates (in columns `receiver_x` and `receiver_y`). 
#' @param .path A [`data.table::data.table`] with movement path(s). This must contain the following columns:
#' * `row`, `column` : row/column identifiers, as for `.mapdt`;
#' * `timestep`      : an `integer` vector of time steps for each position along a path;
#' * `x`, `y`        : path coordinates on the `.map`;
#' 
#' Note that only a subset of `row`/`column` combinations in `.mapdt` may be associated with paths. `row`/`column` combinations not in `.mapdt` are dropped with a [`warning`].
#' @param .coast_tolerance,.coast_mask Spatial operations applied to spatial layers before plotting. 
#' * `.coast_tolerance` is a `double`, passed to [`sf::st_simplify()`]'s `dTolerance` argument to simplify the coastline before plotting (for improved speed). 
#' * `.coast_mask` is a logical variable that defines whether or not to mask each UD by `.coast`.
#' @param .map_mask_zero A `logical` variable that defines whether or not to set grid cells with a value of zero to `NA`. Under the default colour scheme, these cells then appear in white. 
#' @param .trans_coast,.trans_poly Numbers that define the transparency of the coastline and the polygon on blank plots. 
#' @param .geom_coast,.geom_poly,.geom_moorings,.geom_path Named `list`s, passed to [`ggplot2::geom_sf()`] or [`ggplot2::geom_path()`], used to customise graphics. 
#' @param .scale_path If `.path` is specified, `.scale_path` is a scale for the colour of the path.  
#' @param .xlim,.ylim,.zlim (optional) Axis limits. 
#' @param .png_args (optional) A named `list` of arguments, passed to [`grDevices::png()`], to write the image to file.  
#' @param .verbose User output control. 
#' @details
#' By default, `.zlim` is defined for each map between the minimum and maximum values of that map, unless specified. The colour scheme is inherited from `getOption(terra.pal)`, if specified, or set to `grDevices::terrain.colors(256L, rev = TRUE)` otherwise. `.coast` is added as a polygon. Set `.coast$col` and `.coast$alpha` to set fill and transparency, respectively. `.trans_coast` sets transparency on blank panels (see below). `.poly` is added as a line; `.poly$col`, `.poly$alpha` set colour and transparency. `.trans_poly` sets transparency on blank panels. `.moorings` is added as points; `.path` is added as paths. All layers can be customised via `geom_*()` `list`s, but note that these `list`s currently override transparency settings on blank panels. Blank panels (without a UD) are produced for any `.mapdt$file_ud` that doesn't exist (or `NA` elements). If `.coast`, `.poly` and/or `.moorings` are specified, these spatial layers are rendered partially transparent on such panels. `.path`(s) are added afterwards, coloured by timestep. Use `.scale_path` to change the colour scaling for paths. Submit an issue request for additional customisation options. 
#' 
#' @example man/examples/example-ggplots.R
#' @author Edward Lavender
#' @export 

ggmaps <- function(.mapdt,
                   .map, 
                   .coast = NULL, 
                   .poly = NULL,
                   .moorings = NULL,
                   .path = NULL,
                   .coast_tolerance = NULL, .coast_mask = FALSE, 
                   .map_mask_zero = FALSE,
                   .trans_coast = 0.075, 
                   .trans_poly = 0.25,
                   .geom_coast = list(),
                   .geom_poly = list(), 
                   .geom_moorings = list(), 
                   .geom_path = list(),
                   .scale_path = scale_colour_gradientn(colours = viridis::magma(100)),
                   .xlim = NULL, .ylim = NULL, .zlim = NULL, 
                   .png_args = NULL, 
                   .verbose = TRUE) {
  
  # Set up
  rlang::check_installed(c("data.table", "dplyr", 
                           "ggplot2", "scales", 
                           "sf", "terra"))
  cats    <- cat_setup(.fun = "ggmaps", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  
  # Validate inputs
  stopifnot(c("row", "column", "file_ud") %in% colnames(.mapdt))
  n_row <- length(unique(.mapdt$row))
  n_col <- length(unique(.mapdt$column))
  
  # Define a blank map (data.frame)
  # * This is used to handle blank panels (for which we couldn't compute the UD)
  blank <- 
    as.data.frame(.map, xy = TRUE, na.rm = TRUE) |> 
    as.data.table()
  colnames(blank) <- c("x", "y", "map_value")
  map_value <- col <- NULL
  blank[, map_value := NA_real_]
  blank[, col := NA_character_]
  
  # (optional) Simplify coastline for improved speed 
  if (!is.null(.coast)) {
    .coast <- sf::st_as_sf(.coast)
    if (!is.null(.coast_tolerance)) {
      .coast <- sf::st_simplify(.coast, preserveTopology = TRUE, dTolerance = .coast_tolerance) 
    }
  }

  # (optional) Prepare .poly
  if (!is.null(.poly)) {
    .poly <- sf::st_as_sf(.poly)
  }
  
  # Build a mapdata data.frame
  # * This includes, for each panel (row/column), the raster coordinates & values
  mapdata <- lapply(split(.mapdt, seq_len(nrow(.mapdt))), function(d) {
    if (!is.na(d$file_ud) && file.exists(d$file_ud)) {
      r <- terra::rast(d$file_ud)
      if (!is.null(.coast) & .coast_mask) {
        r <- terra::mask(r, .coast, inverse = TRUE, touches = FALSE)
      }
      if (.map_mask_zero) {
        r <- terra::classify(r, cbind(0, NA))
      }
      # Get map coordinates via as.data.frame() or terra::spatSample() 
      rdt <- as.data.frame(r, xy = TRUE) 
      rdt <- rdt[!is.na(rdt[, 3]), ]
      setDT(rdt)
      colnames(rdt) <- c("x", "y", "map_value")
      # Define colours
      # * facet_wrap() forces the same .zlim across all plots
      # * For individual colours, between min and max, we have to:
      # - Build the col column here
      # - Use scale_fill_identity()
      cols <- getOption("terra.pal")
      if (is.null(cols)) {
        cols <- grDevices::terrain.colors(256L, rev = TRUE)
      }
      if (is.null(.zlim)) {
        .zlim <- range(rdt$map_value)
      }
      ints <- seq(.zlim[1], .zlim[2], length.out = length(cols))
      cols <- data.table(int = ints, 
                         col = cols)
      rdt[, col := cols$col[findInterval(rdt$map_value, cols$int)]]
      stopifnot(all(!is.na(rdt$col)))
    } else {
      rdt <- copy(blank)
    }
    # Link data.tables (e.g., row/column)
    # cbind(d, rdt)
    rdt[, c("row", "column") := list(d$row, d$column)]
  }) 
  if (length(mapdata) == 1L) {
    mapdata <- mapdata[[1]]
  } else {
    mapdata <- rbindlist(mapdata)
  }
  key <- row <- column <- NULL
  mapdata[, key := paste(row, column)]
  
  # Extract colours
  # * TO DO Extract colours from geom_*() elements via get_geom_parameter()
  # * It would be nice to extract fill, colour and alpha from geom_ elements
  #   and then carry that forward
  # * At the moment colours can be specified there, 
  #   but the transparency on blank panels is not carried forward
  
  # Update .coast with gg mapping
  # * We duplicate .coast for each raster
  # * We use a more transparent colour for panels with convergence failures
  # * We use a blank panel otherwise (for NA factor levels)
  if (!is.null(.coast)) {
    if (!rlang::has_name(.coast, "col")) {
      .coast$col <- "dimgrey"
    }
    if (!rlang::has_name(.coast, "alpha")) {
      .coast$alpha <- 0.3
    }
    .coast <- 
      lapply(unique(mapdata$key), function(key) {
        .coast$key <- key
        .coast
      }) |> 
      dplyr::bind_rows() |> 
      mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
             column = mapdata$column[match(.data$key, mapdata$key)], 
             blank = is.na(mapdata$col[match(.data$key, mapdata$key)]), 
             alpha = if_else(blank, .trans_coast, .data$alpha),
             col = scales::alpha(.data$col, .data$alpha),
             key = NULL, blank = NULL, alpha = NULL)
  }

  # Update .poly with gg mapping
  if (!is.null(.poly)) {
    if (!rlang::has_name(.poly, "col")) {
      .poly$col <- "black"
    }
    if (!rlang::has_name(.poly, "alpha")) {
      .poly$alpha <- 0.5
    }
    if (rlang::has_name(.poly, "row") & rlang::has_name(.poly, "column")) {
      .poly$key <- paste(.poly$row, .poly$column)
    } else {
      .poly <- 
        lapply(unique(mapdata$key), function(key) {
          .poly$key <- key
          .poly
        }) |> 
        dplyr::bind_rows() |> 
        mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
               column = mapdata$column[match(.data$key, mapdata$key)])
    }
    .poly <- 
      .poly |> 
      mutate(blank = is.na(mapdata$col[match(.data$key, mapdata$key)]), 
             alpha = ifelse(blank, .trans_poly, .data$alpha),
             col = scales::alpha(.data$col, .data$alpha),
             key = NULL)
  }

  # Update .moorings with gg mapping
  # * This is necessary so that we only add receivers to non-blank panels
  if (!is.null(.moorings)) {
    .moorings <- 
      lapply(unique(mapdata$key[!is.na(mapdata$col)]), function(key) {
        m <- copy(.moorings)
        m[, key := key]
      }) |> 
      dplyr::bind_rows() |> 
      mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
             column = mapdata$column[match(.data$key, mapdata$key)]) |>
      filter(.data$key %in% mapdata$key) |> 
      as.data.table()
  }

  # Process .path
  if (!is.null(.path)) {
    # .path must contain row/column plus timestep and x and y coordinates
    .path <- as.data.table(.path)
    check_names(.path, c("row", "column", "timestep", "x", "y"))
    # .path shouldn't contain row/columns not in .mapdt
    n0 <- nrow(.path)
    .path <- .path[.path$row %in% .mapdt$row & .path$column %in% .mapdt$column, ]
    if (n0 != nrow(.path)) {
      warn(".path rows/columns not in mapdt are dropped.")
    }
  }
  
  # Define map limits
  if (is.null(.xlim)) {
    .xlim <- terra::ext(.map)[1:2]
    # .xlim <- terra::ext(.poly)[1:2]
  }
  if (is.null(.ylim)) {
    .ylim <- terra::ext(.map)[3:4]
    # .ylim <- terra::ext(.poly)[3:4]
  }
  
  # Base ggplot 
  p <- 
    mapdata |> 
    ggplot() + 
    theme_bw() + 
    geom_raster(aes(x = .data$x, y = .data$y, fill = .data$col)) +
    scale_fill_identity() 
  
  # (optional) Add .coast
  if (!is.null(.coast)) {
    p <- p + do.call(geom_sf,
                     list_args(default = list(data = .coast, 
                                              mapping = aes(fill = I(.data$col)), 
                                              linewidth = 0.15), 
                               user = .geom_coast))
  }
  
  # (optional) Add polygon (e.g., mpa)
  if (!is.null(.poly)) {
    p <- p + do.call(geom_sf,
                     list_args(default = list(data = .poly, 
                                              mapping = aes(color = I(.data$col)), 
                                              fill = NA, linewidth = 0.25), 
                               user = .geom_poly))
  }
  
  # (optional) Add .moorings
  if (!is.null(.moorings)) {
    p <- p + do.call(geom_point,
                     list_args(default = list(data = .moorings, 
                                              mapping = aes(.data$receiver_x, .data$receiver_y), 
                                              shape = 4, size = 0.2, stroke = 0.2), 
                               user = .geom_moorings))
  }
  
  # Add .path(s)
  if (!is.null(.path)) {
    p <- p + do.call(geom_path,
                     list_args(default = list(data = .path, 
                                              mapping = aes(.data$x, .data$y, colour = .data$timestep)), 
                               user = .geom_path))
    if (!is.null(.scale_path)) {
      p <- p + .scale_path
    }
  }
  
  # Tidy ggplot
  p <- 
    p + 
    coord_sf(xlim = .xlim, ylim = .ylim, expand = FALSE) + 
    xlab("") + ylab("") 
  
  # Implement faceting 
  if (nrow(.mapdt) > 1L) {
    p <- 
      p +  
      facet_grid(row ~ column, drop = FALSE) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            panel.grid = element_blank(), 
            # strip.text = element_blank(), 
            legend.position = "none")
  }
  
  # Return ggplot 
  if (!is.null(.png_args)) {
    do.call(grDevices::png, .png_args)
    print(p)
    grDevices::dev.off()
    return(nothing())
    
  } else {
    
    return(p)
  }
  
}
