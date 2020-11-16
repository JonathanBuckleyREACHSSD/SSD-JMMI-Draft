rm(list = ls())

library(dplyr)
library(leaflet)

setwd("C:/Users/REACH/Dropbox (SSD REACH)/REACH South Sudan upscale/34_WFP/11_WFP_IACWG")
markets <- read.csv('5. Tools/Marketplace mapping/market_maping.csv', stringsAsFactors = FALSE)

market_radius <- function (x) {ifelse(x %in% '1_15',2.5,
                                      ifelse(x %in% '16_50',3,
                                             ifelse(x %in% '51_100',3.5,
                                                    ifelse(x %in% '101_200', 4,
                                                           ifelse(x %in% '201_500', 4.5,
                                                                  ifelse(x %in% 'above_500', 6,
                                                                         ifelse(x %in% 'dont_know', 1, 1)))))))
  
  
}

markets <- markets %>%
 filter(!is.na(lat) & !is.na(lon) & !is.na(size))

market_map <- leaflet(markets) %>%
  addTiles(group = 'OSM') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
  addProviderTiles(providers$OpenTopoMap, group = 'Topographic') %>%
  addCircleMarkers(color = 'red',
    radius = ~market_radius(size),
    popup = ~marketplace,
    stroke = FALSE,
    group = 'Marketplaces') %>%
  addLayersControl(baseGroups = c("OSM", "Satellite", 'Topographic'), 
                   overlayGroups = c("Marketplaces"),
                   options = layersControlOptions(collapsed = FALSE))
             
market_map 