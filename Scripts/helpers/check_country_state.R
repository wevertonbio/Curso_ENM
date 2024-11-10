check_country_state <- function(data, long = "decimalLongitude",
                          lat = "decimalLatitude",
                          country,
                          new_column,
                          distance = 50,
                          map,
                          column_map = "admin"){
  
  #Get unique countries
  countries <- unique(data[[country]])
  
  #Get shapefile with countries
  country_shp <- map[, column_map]
  #Intersect with available countries to test
  countries <- intersect(countries, country_shp[[column_map]][[column_map]])
  
  #Convert to spatial object
  data <- as.data.frame(data)
  #Get unique coordinates
  unique_xy <- data[,c(long, lat, country)] %>% distinct()
  
  dc <- terra::vect(unique_xy,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")
  
  #Looping with pbsapply
  message("Testing countries/states...")
  test_country <- pbapply::pbsapply(countries, function(i){
    country_i <- country_shp[country_shp[[column_map]] == i]
    country_i <- terra::aggregate(terra::buffer(country_i, width = distance*1000))
    data_i <- which(unique_xy[[country]] == i)
    xy_i <- dc[data_i]
    xy_test <- as.logical(relate(country_i, xy_i, "contains"))
    names(xy_test) <- data_i
    return(xy_test)
  })
  names(test_country) <- NULL
  test_country <- unlist(test_country)
  #Updata data of unique coordinates
  unique_xy[[new_column]] <- NA
  if(is.null(names(test_country))){
    unique_xy[, new_column] <- test_country[[1]]
  } else {
  unique_xy[as.numeric(names(test_country)), new_column] <- test_country
  }
  
  #Merge data again
  data <- left_join(data, unique_xy, by = c(long, lat, {{country}}))
  return(data)
}
