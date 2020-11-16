rm(list = ls())
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(mapview)
library(sf)
library(leaflet.mapboxgl)

setwd("C:/Users/REACH/Dropbox (SSD REACH)/REACH South Sudan upscale/34_WFP/11_WFP_IACWG")
coordinates <- read.csv('8. Dashboard/r_dashboard/app_plot/coordinates.csv')
jmmi <- read.csv('7. JMMI Data/new/1_import/JMMI_data.csv', stringsAsFactors = FALSE)


# filter data 

jmmi <- 

# add coordinates

jmmi_coord <- left_join(coordinates, jmmi_fil, by = 'Location') %>%
  filter(Week == 37)

jmmi_coord_2 <- left_join(coordinates, jmmi_fil_2, by = 'Location') %>%
  filter(Month ==9)

disputed <- st_read('8. Dashboard/r_dashboard/app_plot/Disputed/SSD_Undetermined.shp')
disputed <- st_transform(disputed,"+init=epsg:4326" )

states <- st_read('8. Dashboard/r_dashboard/app_plot/States/SSD_States.shp')
states <- st_transform(states,"+init=epsg:4326" )

counties <- st_read('8. Dashboard/r_dashboard/app_plot/Counties/SSD_counties.shp')
counties <- st_transform(counties,"+init=epsg:4326" )

rivers <- st_read('8. Dashboard/r_dashboard/app_plot/Rivers/SSD_Rivers.shp')
rivers <- st_transform(rivers, "+init=epsg:4326")
rivers_primary <- rivers %>% filter(OBJECTID == c(5, 6))
#st_write(rivers_primary, "rivers_primary.shp")

roads <- st_read('8. Dashboard/r_dashboard/app_plot/Roads/SSD_Roads.shp')
roads <- st_transform(roads, "+init=epsg:4326")
roads_primary <- roads %>% filter(CLASS == "Primary")
#st_write(roads_primary, "roads_primary.shp")

USD_format <- function(x) {paste(sep = " ", round(x), "SSP")}

jmmi_coord <- mutate(jmmi_coord, USD_SSP = USD_format(USD))

jmmi_coord_2 <- mutate(jmmi_coord_2, USD_SSP = USD_format(USD))

options(mapbox.accessToken = "pk.eyJ1Ijoic3Nkam1taWFvIiwiYSI6ImNrZnhqdTlsMTF3MDkycHBkZWNvcnhkMGYifQ.Av8BPr_-jtfGN5T9wzStzg")
m <- leaflet() %>%
        addMapboxGL(style = 'mapbox://styles/ssdjmmiao/ckg0w32ww1m6119qtx6qtrtot') %>%
        setView(lat = 7.7, lng = 30, zoom = 7) %>%
    
  
m


# addPolygons(data = disputed, group = "Disputed", fill = FALSE, stroke = TRUE, dashArray = c(5, 5), color = "#58585A", weight = 1, opacity = 0.7) %>%
addPolygons(data = counties, group = "Counties", fill = FALSE, stroke = TRUE, color = "#BDBDBD", weight = 0.6, opacity = 0.5) %>%
  addPolygons(data = states, group = "States", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1, opacity = 0.7, label = states$admin1Name,
              labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "center", style=list("font-size"="14px", 
                                                                                                        "text-shadow"="10px 10px 10px '#ffffff'",
                                                                                                        "color"="rgb(88, 88, 91)",
                                                                                                        "font-family" = "Arial Narrow",
                                                                                                        "font-weight" = "bold"
              ))) %>%
  addPolylines(data = rivers_primary, group = "Rivers", stroke = TRUE, color = "#94CCDC", weight = 1.3, opacity = 0.7) %>%
  addPolylines(data = roads_primary, group = "Roads", stroke = TRUE, color = "#F69E61", weight = 1.5, opacity = 0.4)

