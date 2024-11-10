#Extract discrepancies between countries
fix_coordinates_by_map <- function(data,
                           long = "decimalLongitude",
                           lat = "decimalLatitude",
                           country = "country_suggested",
                           correct_country = "correct_country",
                           distance = 50,
                           world_map,
                           column_map = "admin"){
  #Subset data
  data_correct <- data %>% filter(correct_country | is.na(correct_country)) %>% as.data.frame()
  data_incorrect <- data %>% filter(!correct_country) %>% as.data.frame()
  #Create columns to identify problems
  data_correct$transposed_country <- "correct"
  data_incorrect$transposed_country <- "incorrect"
  
  #Get shapefile with countries
  country_shp <- world_map[, column_map]
  #Get all countries
  all_countries <- unique(country_shp[[column_map]])[[column_map]]
  
  ####1 - Inverted signal of longitude####
  d1 <- data_incorrect[,c(long,lat, country)]
  countries_1 <- base::intersect(unique(d1[,country]), all_countries)
  
  d1[, long] <- - d1[,long]
  d1 <- terra::vect(d1,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")
  message("Task 1 of 7: testing if longitude is inverted")
  test_1 <- pbapply::pbsapply(countries_1, function(i){
    country_i <- country_shp[country_shp[[column_map]] == i]
    country_i <- terra::buffer(country_i, width = distance*1000)
    data_i <- which(d1[[country]] == i)
    xy_i <- d1[data_i]
    xy_test <- terra::is.related(xy_i, country_i, "intersects")
    names(xy_test) <- data_i
    return(xy_test)
  })
  names(test_1) <- NULL
  test_1 <- unlist(test_1)
  message(sum(test_1), " coordinates with longitude inverted")
  #Updata data
  data_incorrect[as.numeric(names(test_1)), correct_country] <- test_1
  #Update coordinates
  data_incorrect[,long][
    data_incorrect[,correct_country] == TRUE] <- - data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE]
  #Update transposed information
  data_incorrect[,"transposed_country"][
    data_incorrect[,correct_country] == TRUE] <- "inverted_long"
  
  #Update correct and incorrect data
  data_correct <- bind_rows(data_correct,
                            data_incorrect %>% filter(correct_country))
  data_incorrect <- data_incorrect %>% filter(!correct_country)
  
  ####2 - Inverted signal of latitude ####
  if(nrow(data_incorrect) > 0) {
    d2 <- data_incorrect[,c(long,lat, country)]
    countries_2 <- intersect(unique(d2[,country]), all_countries)
    d2[, lat] <- - d2[,lat]
    d2 <- terra::vect(d2,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    message("Task 2 of 7: testing if latitude is inverted")
    test_2 <- pbapply::pbsapply(countries_2, function(i){
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d2[[country]] == i)
      xy_i <- d2[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_2) <- NULL
    test_2 <- unlist(test_2)
    message(sum(test_2), " coordinates with latitude inverted")
    #Updata data
    data_incorrect[as.numeric(names(test_2)), correct_country] <- test_2
    #Update coordinates
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- - data_incorrect[,lat][
        data_incorrect[,correct_country] == TRUE]
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "inverted_lat"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  ####3 - Inverted signal of longitude and latitude ####
  if(nrow(data_incorrect) > 0) {
    d3 <- data_incorrect[,c(long,lat, country)]
    countries_3 <- intersect(unique(d3[,country]), all_countries)
    d3[, long] <- - d3[,long]
    d3[, lat] <- - d3[,lat]
    d3 <- terra::vect(d3,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    message("Task 3 of 7: testing if longitude and latitude are inverted")
    test_3 <- pbapply::pbsapply(countries_3, function(i){
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d3[[country]] == i)
      xy_i <- d3[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_3) <- NULL
    test_3 <- unlist(test_3)
    message(sum(test_3), " coordinates with longitude and latitude inverted")
    #Updata data
    data_incorrect[as.numeric(names(test_3)), correct_country] <- test_3
    #Update coordinates
    data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE] <- - data_incorrect[,long][
        data_incorrect[,correct_country] == TRUE]
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- - data_incorrect[,lat][
        data_incorrect[,correct_country] == TRUE]
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "inverted_long_lat"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  ####4 - Swap longitude and latitude ####
  if(nrow(data_incorrect) > 0) {
    d4 <- data_incorrect[,c(long,lat, country)]
    countries_4 <- intersect(unique(d4[,country]), all_countries)
    # d4[, long] <- - d4[,long]
    # d4[, lat] <- - d4[,lat]
    d4 <- terra::vect(d4,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 4 of 7: testing if longitude and latitude are swapped")
    test_4 <- pbapply::pbsapply(countries_4, function(i){
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d4[[country]] == i)
      xy_i <- d4[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_4) <- NULL
    test_4 <- unlist(test_4)
    message(sum(test_4), " coordinates with longitude and latitude swapped")
    #Updata data
    data_incorrect[as.numeric(names(test_4)), correct_country] <- test_4
    #Update coordinates
    correct_x <- data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE]
    correct_y <- data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE]
    data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE] <- correct_x
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- correct_y
    
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "Swaped_long_lat"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  ####5 - Swap longitude and latitude, with longitude inverted ####
  if(nrow(data_incorrect) > 0) {
    d5 <- data_incorrect[,c(long,lat, country)]
    countries_5 <- intersect(unique(d5[,country]), all_countries)
    d5[, long] <- - d5[,long]
    # d5[, lat] <- - d5[,lat]
    d5 <- terra::vect(d5,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 5 of 7: testing if longitude and latitude are swapped -
            with longitude inverted")
    test_5 <- pbapply::pbsapply(countries_5, function(i){
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d5[[country]] == i)
      xy_i <- d5[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_5) <- NULL
    test_5 <- unlist(test_5)
    message(sum(test_5), " coordinates with longitude and latitude swapped
            and longitude inverted")
    #Updata data
    data_incorrect[as.numeric(names(test_5)), correct_country] <- test_5
    #Update coordinates
    correct_x <- data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE]
    correct_y <- - data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE]
    data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE] <- correct_x
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- correct_y
    
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "Swaped_and_inverted_long"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  ####6 - Swap longitude and latitude, with latitude inverted ####
  if(nrow(data_incorrect) > 0) {
    d6 <- data_incorrect[,c(long,lat, country)]
    countries_6 <- intersect(unique(d6[,country]), all_countries)
    #d6[, long] <- - d6[,long]
    d6[, lat] <- - d6[,lat]
    d6 <- terra::vect(d6,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 6 of 7: testing if longitude and latitude are swapped -
            with latitude inverted")
    test_6 <- pbapply::pbsapply(countries_6, function(i){
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d6[[country]] == i)
      xy_i <- d6[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_6) <- NULL
    test_6 <- unlist(test_6)
    message(sum(test_6), " coordinates with longitude and latitude swapped
            and latitude inverted")
    #Updata data
    data_incorrect[as.numeric(names(test_6)), correct_country] <- test_6
    #Update coordinates
    correct_x <- - data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE]
    correct_y <- data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE]
    data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE] <- correct_x
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- correct_y
    
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "Swaped_and_inverted_lat"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  ####7 - Swap longitude and latitude, with latitude and longitude inverted ####
  if(nrow(data_incorrect) > 0) {
    d7 <- data_incorrect[,c(long,lat, country)]
    countries_7 <- intersect(unique(d7[,country]), all_countries)
    d7[, long] <- - d7[,long]
    d7[, lat] <- - d7[,lat]
    d7 <- terra::vect(d7,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 7 of 7: testing if longitude and latitude are swapped -
            with longitude latitude inverted")
    test_7 <- pbapply::pbsapply(countries_7, function(i){
      #print(i)
      country_i <- country_shp[country_shp[[column_map]] == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      data_i <- which(d7[[country]] == i)
      xy_i <- d7[data_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- data_i
      return(xy_test)
    })
    names(test_7) <- NULL
    test_7 <- unlist(test_7)
    message(sum(test_6), " coordinates with longitude and latitude swapped
            and inverted")
    #Updata data
    data_incorrect[as.numeric(names(test_7)), correct_country] <- test_7
    #Update coordinates
    correct_x <- - data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE]
    correct_y <- - data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE]
    data_incorrect[,long][
      data_incorrect[,correct_country] == TRUE] <- correct_x
    data_incorrect[,lat][
      data_incorrect[,correct_country] == TRUE] <- correct_y
    
    #Update transposed information
    data_incorrect[,"transposed_country"][
      data_incorrect[,correct_country] == TRUE] <- "Swaped_and_inverted_long_lat"
    
    #Update correct and incorrect data
    data_correct <- bind_rows(data_correct,
                              data_incorrect %>% filter(correct_country))
    data_incorrect <- data_incorrect %>% filter(!correct_country)
  }
  
  #Final dataset
  if(nrow(data_incorrect) == 0) {
    data <- data_correct
  } else {
    data <- bind_rows(data_correct, data_incorrect)
  }
  
  return(data)
}