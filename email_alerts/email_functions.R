require(tidyverse)
require(rinat)
require(lubridate)
require(leaflet)
require(bslib)
require(fresh)
require(png)
require(grid)
require(purrr)
require(readxl)
require(rebird)
require(downloader)
require(sf)
require(hashids)
require(shinyalert)


## List of functions
# inat_recent()
# ebird_recent()
# combine_citsci_data()
# filter_nps()
# te_species()
# watchlist_species()
# new_species()
# make_leaflet()
# leaflet_summary()




#' Function returns summaries and a data frame of recent iNaturalist observations 
#'
#' This function takes a recent time span and returns all iNaturalist records from
#' inside a desired location during that time span. Additionally, this 
#' function produces four data frames with summary statistics from the iNaturalist data.
#'
#' @inheritParams None
#' @return A data frame of recent iNaturalist observations.
#' @param timespan: The recent time span of interest. Options 
#' are "week", "threedays", or "yesterday" as inputs for the "timespan" parameter.
#' @seealso None
#' @export

inat_recent <- function(timespan) {
  
  # Stop summarise output message
  options(dplyr.summarise.inform = FALSE)
  
  
  # Get the past week's dates and format
  date.filter <- format(Sys.Date()-1:7, "%Y-%m-%d") %>% 
    as_tibble() %>% 
    rename(date = value) %>% 
    mutate(year = as.numeric(year(date)),
           month = as.numeric(month(date)),
           day = as.numeric(day(date)))
  
  
  # List the month and year for get_inat_obs sub-function
  year <- date.filter$year
  month <- date.filter$month
  
  ym <- paste(year, month, sep = "-") %>% 
    unique() %>% 
    as.list()
  
  
  # This is the function that starts the download of inat data inside park boundary
  get_inat_data <- function(dates) {
    
    obs_year <- dates %>% 
      unlist() %>%
      str_remove(., "\\-\\d*$")
    
    obs_month <- dates %>% 
      unlist() %>% 
      str_remove(., "^\\d*\\-")
    
    get_inat_obs(place_id = 17,
                 geo = TRUE,
                 year = obs_year, 
                 month = obs_month, 
                 maxresults = 10000) %>% 
      filter(iconic_taxon_name == "Aves") %>% 
      as_tibble() %>% 
      select(scientific_name, common_name, iconic_taxon_name, observed_on, place_guess, 
             latitude, longitude, positional_accuracy, user_login, user_id, captive_cultivated, url, image_url, license) %>% 
      mutate(common_name = tolower(common_name)) %>% 
      rename_all( ~ str_replace_all(., "_", "."))
    
  }
  
  
  # Runs if week is called
  if (timespan == "week") {
    
    # Pull the previous week of inat data
    inat_obs <- map_dfr(ym, get_inat_data) %>% 
      filter(observed.on >= date.filter$date[7] & observed.on <= date.filter$date[1])
    
  }
  
  
  # Runs if month is called
  if (timespan == "month") {
    
    # Subset this to three days
    inat_obs <- map_dfr(ym, get_inat_data)
    
  }

  
  
  inat_obs <- inat_obs %>% 
    mutate(dup = duplicated(.),
           observed.on = as.Date(observed.on)) %>% 
    filter(dup == "FALSE") %>% 
    select(-dup)
  
  
  
  return(inat_obs) 
  
}





#' Function to produce an interactive leaflet map widget of iNaturalist observations
#'
#' This function takes a data frame of iNaturalist records (created specifically for the
#' output of the "inat_recent()" function) and produces a leaflet map widget with satellite 
#' imagery base layer, labels of common names that appear when the mouse is hovered over a
#' marker, and a link to the observation on iNaturalist accessible by clicking a marker.
#'
#' @inheritParams None
#' @return A leaflet map widget of recent iNaturalist observations.
#' @param x: Data frame of iNaturalist observations.
#' @seealso None
#' @export

leaflet_summary <- function (x) {
  
  formap <- x %>% 
    mutate(url = paste0("<b><a href='", url, "' target='_blank' rel='noopener noreferrer'", ">View observation here</a></b>")) 
  
  maxLong = max(formap$longitude) + 0.1
  maxLat = max(formap$latitude) + 0.1
  minLong = min(formap$longitude) - 0.1
  minLat = min(formap$latitude) - 0.1
  
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35)) %>% 
    #addProviderTiles(providers$Stamen.TerrainLabels) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels) %>% 
    addMarkers(formap$longitude, formap$latitude, label = formap$common.name,
               labelOptions = labelOptions(textsize = "15px"),
               clusterOptions = markerClusterOptions(),
               popup = formap$url) %>%
    fitBounds(minLong, minLat, maxLong, maxLat)
  
  return(map)
  
}

