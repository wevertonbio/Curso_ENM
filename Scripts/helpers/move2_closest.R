move_2closest_cell <- function(data, longitude_column, latitude_column,
                               raster_layer, move_limit_distance,
                               verbose = TRUE) {

  # detecting potential errors
  if (missing(data)) {
    stop("Argument 'data' must be defined.")
  }
  if (missing(longitude_column)) {
    stop("Argument 'longitude_column' must be defined.")
  }
  if (missing(latitude_column)) {
    stop("Argument 'latitude_column' must be defined.")
  }
  if (missing(raster_layer)) {
    stop("Argument 'raster_layer' must be defined.")
  }
  if (missing(move_limit_distance)) {
    stop("Argument 'move_limit_distance' must be defined.")
  }
  if (class(raster_layer)[1] != "SpatRaster") {
    stop("'raster_layer' must be of class 'SpatRaster'")
  }

  # preparing data
  message("Convertind data.frame to spatial object...")
  xy <- data[, c(longitude_column, latitude_column)]

  message("Identifying records to move...")
  vals <- terra::extract(raster_layer, xy)

  tomove <- which(is.na(vals[[2]]))

  # finding pixels to move in
  if (length(tomove) > 0) {
    xyout <- data[tomove, ]

    ## buffer from out
    limdist <- move_limit_distance * 1000

    xyvec <- terra::vect(xyout, geom = c(longitude_column, latitude_column),
                         crs = "+proj=longlat")
    xyvec <- terra::buffer(xyvec, width = limdist)

    ## relevant pixels to move
    raster_layer <- terra::crop(raster_layer, xyvec, mask = TRUE)

    xyras <-  as.data.frame(raster_layer, xy = TRUE)[, 1:2]
    dists <- terra::distance(as.matrix(xyout[, c(longitude_column,
                                                 latitude_column)]),
                             as.matrix(xyras), lonlat = TRUE)

    condition <- rep("Correct", nrow(data))
    distss <- rep(0, nrow(data)) # not distance for the ones farther

    # running process
    if (verbose == TRUE) {
      message("Moving occurrences to closest pixels...")
    }

    no <- nrow(xyout)

    for (i in 1:no) {
      mindis <- min(dists[i, ])

      if (mindis <= limdist) {
        xyin <- xyras[dists[i, ] == mindis, ]

        data[tomove[i], longitude_column] <- xyin[1, 1]
        data[tomove[i], latitude_column] <- xyin[1, 2]
        condition[tomove[i]] <- "Moved"
        distss[tomove[i]] <- mindis / 1000
      } else {
        condition[tomove[i]] <- "Not_moved"
        distss[tomove[i]] <- mindis / 1000
      }
    }
    data <- data.frame(data, condition = condition, distance_km = distss,
                       initial_lon = xy[, 1], initial_lat = xy[, 2])

    data$distance_km <- ifelse(data$distance_km > move_limit_distance, NA,
                               data$distance_km)
  } else {
    if (verbose == TRUE) {
      message("All points are inside valid raster boundaries.")
    }
  }

  return(data)
}
