standardize_country_state <- function(data,
                                country_state,
                                new_column,
                                obs_column,
                                x, y,
                                map,
                                map_column, postal_column,
                                use_raster = FALSE, r = NULL,
                                verbose = TRUE){
  
  #Remove accents
  map[[map_column]] <- sapply(map[[map_column]], accentless)
    
  #Import data and extract infor
  countries_names <- map[[map_column]] %>% unique() %>% pull()
  postal_codes <- map[,c(map_column, postal_column)] %>% as.data.frame() %>%
    filter({ postal_column } != "-99") %>% distinct()
  
  #Create column with new country or state
  data[[new_column]] <- stringr::str_to_title(data[[country_state]])
  
  #Remove accents
  data[[new_column]] <- accentless(data[[new_column]])
  
  #Postal code to upper case
  data[[new_column]][
    stringr::str_count(data[[new_column]]) == 2] <- toupper(data[[new_column]][
      stringr::str_count(data[[new_column]]) == 2])
  #Remove ? and !
  data[[new_column]] <- gsub("\\?", "", data[[new_column]])
  
  #Get countries correct
  c_in <- unique(intersect(data[[new_column]], countries_names))
  
  if(length(c_in) > 0){
  #Create column
  data[[obs_column]] <- NA
  data[[obs_column]][data[[new_column]] %in% c_in] <- "Perfect match"
  
  if(verbose)
    message(sum(data[[obs_column]]== "Perfect match", na.rm = TRUE), 
            " ocurrences with perfect match") } else {
  if(verbose)
   message("0 ocurrences with perfect match")
            }
  
  #Try to fix postal codes
  colnames(postal_codes)[2] <- new_column
  data <- left_join(data, postal_codes, by = new_column)
  data[[new_column]][!is.na(data[[map_column]])] <- data[[map_column]][!is.na(data[[map_column]])]
  #Create column
  data[[obs_column]][!is.na(data[[map_column]])] <- "Fixed by postal code"
  
  if(verbose)
  message(length(data[[map_column]][!is.na(data[[map_column]])]), " occurences fixed with postal code")
  data[[map_column]] <- NULL
  
  #Fix countries not in map and not a postal code
  c_out <- setdiff(data[[new_column]], countries_names)
  c_out <- c_out[stringr::str_count(c_out) > 2]
  c_out <- na.omit(c_out[c_out != ""])
  #Check for similar words
  similar_c <- pbapply::pblapply(c_out, function(i){
     c_agrep <- agrep(i, countries_names, value = TRUE, max.distance = 0.1)
    #Get distance
    if(length(c_agrep) > 0){
      #Create dataframe
      d <- data.frame(new_column = i,
                      new_new_column = c_agrep,
                      Distance = as.numeric(adist(i, c_agrep))) %>% 
        filter(Distance == min(Distance))
      } else {d <- NULL}
    
  })
  similar_c <- data.table(rbindlist(similar_c))
  
  #Rename
  if(nrow(similar_c) > 0) {
  colnames(similar_c)[1] <- country_state

  #Replace countries
  data <- left_join(data, similar_c, by = country_state)
  data$Distance <- NULL
  data[[new_column]][!is.na(data$new_new_column)] <- data$new_new_column[!is.na(data$new_new_column)]
  #Create column
  data[[obs_column]][!is.na(data$new_new_column)] <- "Fixed by fuzzy match"
  
  if(verbose)
      message(length(data$new_new_column[!is.na(data$new_new_column)]), " occurrences fixed by fuzzy matching")
  data$new_new_column <- NULL } else {
    if(verbose)
      message("0 occurrences fixed by fuzzy matching")
  }
  
  #Extract countries from NA
  d_without_c <- which(is.na(data[[obs_column]]))
  crs(map) <- "+init=epsg:4326"
  
  #If use raster, rasterize w
  if(use_raster){
    w_r <- terra::rasterize(map, r, touches = TRUE, field = map_column)
    c_extracted <- as.character(terra::extract(w_r,
                                               data[d_without_c, c(x, y)])[[map_column]])
    
  } else {
  c_extracted <- terra::extract(map, data[d_without_c, c(x, y)])[[map_column]]}
  
  #Update
  data[[new_column]][d_without_c] <- c_extracted
  #Create column
  data[[obs_column]][d_without_c] <- "Extracted from XY"
  if(verbose)
    message(length(data[[obs_column]][d_without_c]), " occurrences extracted from XY")
  #Species absent in countries/states
  data[[new_column]][is.na(data[[new_column]])] <- "Not found"
  
  #Arrange columns
  data <- data %>% dplyr::relocate({{ new_column }}, {{ obs_column }},
                                   .after = {{ country_state }})
  return(data)
}


#Remover acentos
accentless <- function( s ) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
    s);
}


