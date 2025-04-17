#' @title `ggplot` maps
#' @description This function produces multi-panel plots of [`terra::SpatRaster`]s. It was designed to plot utilisation distributions (UDs) for multiple animals (rows) constructed by different algorithms (columns) in passive acoustic telemetry systems. UDs are expected to live on the disk.
#' @param .mapdt  A [`data.table`] that defines plot maps with the following columns: 
#' * `row`, `column` : row/column identifiers;
#' * `file_ud`       : the path to the map, readable by [`terra::rast()`];
#' @param .map,.coast,.poly,.moorings Spatial layers.
#' * `.map` is a [`terra::SpatRaster`]. This is used to handle blank panels (panels where `.mapdt$file_ud` does not exist). 
#' * (optional) `.coast`, `.poly` Simple feature polygon, or similar, that define coastline and/or other relevant boundaries (e.g., a Marine Protected Area). Objects are coerced to simple features for plotting via [`sf::st_as_sf()`].
#' * (optional) `.moorings` A [`data.table`] of coordinates (in columns `receiver_x` and `receiver_y`). 
#' @param .coast_tolerance,.coast_mask Spatial operations applied to spatial layers before plotting. 
#' * `.coast_tolerance` is a `double`, passed to [`sf::st_simplify()`]'s `dtolerance` argument to simplify the coastline polygon before plotting (for improved speed). 
#' * `.coast_mask` is a logical variable that defines whether or not to mask each UD by `.coast`.
#' @param .xlim,.ylim,.zlim (optional) Axis limits. 
#' @param .png_args (optional) A named `list` of arguments, passed to [`grDevices::png()`], to write the image to file.  
#' @param .verbose User output control. 
#' @example man/examples/example-ggplots.R
#' @author Edward Lavender
#' @export 

ggmaps <- function(.mapdt,
                   .map, 
                   .coast = NULL, 
                   .poly = NULL,
                   .moorings = NULL,
                   .coast_tolerance = NULL, .coast_mask = FALSE, 
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
    if (file.exists(d$file_ud)) {
      r <- terra::rast(d$file_ud)
      if (!is.null(.coast) & .coast_mask) {
        r <- terra::mask(r, .coast, inverse = TRUE, touches = FALSE)
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
  
  # Update .coast with gg mapping
  # * We duplicate .coast for each raster
  # * We use a more transparent colour for panels with convergence failures
  # * We use a blank panel otherwise (for NA factor levels)
  if (!is.null(.coast)) {
    .coast <- 
      lapply(unique(mapdata$key), function(key) {
        .coast$key <- key
        .coast
      }) |> 
      dplyr::bind_rows() |> 
      mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
             column = mapdata$column[match(.data$key, mapdata$key)], 
             col = mapdata$col[match(.data$key, mapdata$key)], 
             alp = ifelse(is.na(.data$col), 0.075, 0.3),
             col = scales::alpha("dimgrey", .data$alp),
             key = NULL)
  }

  # Update .poly with gg mapping
  if (!is.null(.poly)) {
    .poly <- 
      lapply(unique(mapdata$key), function(key) {
        .poly$key <- key
        .poly
      }) |> 
      dplyr::bind_rows() |> 
      mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
             column = mapdata$column[match(.data$key, mapdata$key)],
             blank = is.na(mapdata$col[match(.data$key, mapdata$key)]), 
             # col = ifelse(blank, NA, col),
             alp = ifelse(blank, 0.25, 0.5),
             col = scales::alpha(.data$col, .data$alp),
             key = NULL)
  }

  # Update .moorings with gg mapping
  # * This is necessary so that we only add receivers to non-blank panels
  if (!is.null(.moorings)) {
    .moorings <- 
      lapply(unique(mapdata$key), function(key) {
        m <- copy(.moorings)
        m[, key := key]
      }) |> 
      dplyr::bind_rows() |> 
      mutate(row = mapdata$row[match(.data$key, mapdata$key)], 
             column = mapdata$column[match(.data$key, mapdata$key)]) |>
      filter(.data$key %in% mapdata$key) |> 
      as.data.table()
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
    p <- p + geom_sf(data = .coast, aes(fill = I(.data$col)), 
                     linewidth = 0.15) 
  }
  
  # (optional) Add polygon (e.g., mpa)
  if (!is.null(.poly)) {
    p <- p + geom_sf(data = .poly, aes(color = I(.data$col)), 
                     fill = NA, linewidth = 0.25)
  }
  
  # (optional) Add .moorings
  if (!is.null(.moorings)) {
    p <- p + geom_point(data = .moorings, aes(.data$receiver_x, .data$receiver_y), 
                        shape = 4, size = 0.2, stroke = 0.2) 
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
      facet_wrap(row ~ column, drop = FALSE) + 
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
