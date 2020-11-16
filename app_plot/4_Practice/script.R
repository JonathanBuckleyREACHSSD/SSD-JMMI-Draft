rm(list = ls())

library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(sf)
library(dplyr)
library(tidyr)

setwd(setwd('C:/Users/REACH/Dropbox (SSD REACH)/REACH South Sudan upscale/34_WFP/11_WFP_IACWG'))

disputed <- st_read('8. Dashboard/r_dashboard/app_plot/Disputed/SSD_Undetermined.shp')
disputed <- st_transform(disputed,"+init=epsg:4326" )

country <- st_read('8. Dashboard/r_dashboard/app_plot/Country/SSD_Country.shp')
country <- st_transform(country,"+init=epsg:4326" )

states <- st_read("8. Dashboard/r_dashboard/app_plot/States/SSD_States.shp")
states <- st_transform(states,"+init=epsg:4326" )

counties <- st_read("8. Dashboard/r_dashboard/app_plot/Counties/SSD_counties.shp")
counties <- st_transform(counties,"+init=epsg:4326" )

settlements <- st_read("Settlements/SSD_Settlements.shp")
settlements <- st_transform(settlements, "+init=epsg:4326")

rivers <- st_read("8. Dashboard/r_dashboard/app_plot/Rivers/SSD_Rivers.shp")
rivers <- st_transform(rivers, "+init=epsg:4326")
rivers_primary <- rivers %>% filter(OBJECTID == c(5, 6))
#st_write(rivers_primary, "rivers_primary.shp")

lakes <- st_read("8. Dashboard/r_dashboard/app_plot/Lakes/SSD_Lakes.shp")
lakes <- st_transform(lakes, "+init=epsg:4326")

roads <- st_read("8. Dashboard/r_dashboard/app_plot/Roads/SSD_roads.shp")
roads <- st_transform(roads, "+init=epsg:4326")
roads_primary <- roads %>% filter(CLASS == "Primary")
#st_write(roads_primary, "roads_primary.shp")

leaflet() %>%
  #addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = lakes, group = "Lakes", fill = TRUE, stroke = FALSE, fillColor = "#D5EAF1", fillOpacity = 0.75) %>%
  addPolygons(data = counties, group = "Counties", fill = FALSE, stroke = TRUE, color = "#BDBDBD", weight = 0.6, opacity = 0.5) %>%
  addPolygons(data = states, group = "States", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1, opacity = 0.7) %>%
  #addPolygons(data = disputed, group = "Disputed Territory", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1, opacity = 0.7) %>%
  addPolylines(data = rivers_primary, group = "Rivers", stroke = TRUE, color = "#94CCDC", weight = 1.3, opacity = 0.7) %>%
  addPolylines(data = roads_primary, group = "Roads", stroke = TRUE, color = "#F69E61", weight = 1.5, opacity = 0.4) %>%
  # addCircleMarkers(data = settlements, group = "Settlements", radius = 3, stroke = FALSE, fillOpacity = 0.5) %>%
  #addLegend("bottomright", colors = c("#03F", "#03F"), labels = c("States", "Counties")) %>%
  addLayersControl(
    overlayGroups = c("States", "Counties", "Lakes", "Rivers", "Roads"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  setMapWidgetStyle(style = list(background = "transparent"))
 