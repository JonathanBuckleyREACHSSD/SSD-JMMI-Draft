rm(list = ls())
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(mapview)
library(sf)

setwd("C:/Users/REACH/Dropbox (SSD REACH)/REACH South Sudan upscale/34_WFP/11_WFP_IACWG")
coordinates <- read.csv('8. Dashboard/r_dashboard/app_plot/coordinates.csv')
jmmi <- read.csv('7. JMMI Data/new/6_longterm/JMMI_longterm_bylocation.csv', stringsAsFactors = FALSE)
jmmi_2 <- read.csv('12. JMMI COVID Data/new/6_longterm/JMMI_longterm_bylocation.csv', stringsAsFactors = FALSE)

# filter data 

jmmi_fil <- jmmi %>%
  select(Year, Month, Location, USD) %>%
  filter(Year == 2020 & Month == 10) %>%
  arrange(USD)

jmmi_fil_2 <- jmmi %>%
  select(Year, Month, Location, USD) %>%
  filter(Year == 2020 & Month == 9) %>%
  arrange(USD)

# add coordinates

jmmi_coord <- left_join(coordinates, jmmi_fil, by = 'Location') %>%
  filter(Month == 10)

jmmi_coord_2 <- left_join(coordinates, jmmi_fil_2, by = 'Location') %>%
  filter(Month == 9)

func = colorNumeric(c("#A2CD91", "#FFF54C","#ED5758"), domain = jmmi_coord$USD)
func_2 = colorNumeric(c("#A2CD91", "#FFF54C","#ED5758"), domain = jmmi_coord_2$USD)

disputed <- st_read('8. Dashboard/r_dashboard/app_plot/Disputed/SSD_Undetermined.shp')
disputed <- st_transform(disputed,"+init=epsg:4326" )

country <- st_read('8. Dashboard/r_dashboard/app_plot/Country/SSD_Country.shp')
country <- st_transform(country,"+init=epsg:4326" )

states <- st_read('8. Dashboard/r_dashboard/app_plot/States/SSD_States.shp')
states <- st_transform(states,"+init=epsg:4326" )

counties <- st_read('8. Dashboard/r_dashboard/app_plot/Counties/SSD_counties.shp')
counties <- st_transform(counties,"+init=epsg:4326" )

rivers <- st_read('8. Dashboard/r_dashboard/app_plot/Rivers/SSD_Rivers.shp')
rivers <- st_transform(rivers, "+init=epsg:4326")
rivers_primary <- rivers %>% filter(OBJECTID == c(5, 6))
#st_write(rivers_primary, "rivers_primary.shp")

lakes <- st_read('8. Dashboard/r_dashboard/app_plot/Lakes/SSD_Lakes.shp')
lakes <- st_transform(lakes, "+init=epsg:4326")

roads <- st_read('8. Dashboard/r_dashboard/app_plot/Roads/SSD_Roads.shp')
roads <- st_transform(roads, "+init=epsg:4326")
roads_primary <- roads %>% filter(CLASS == "Primary")
#st_write(roads_primary, "roads_primary.shp")

USD_format <- function(x) {paste(sep = " ", round(x), "SSP")}

jmmi_coord <- mutate(jmmi_coord, USD_SSP = USD_format(USD))

jmmi_coord_2 <- mutate(jmmi_coord_2, USD_SSP = USD_format(USD))


m <- leaflet() %>%
        setView(lat = 7.7, lng = 30, zoom = 7) %>%
        addPolygons(data = lakes, group = "Lakes", fill = TRUE, stroke = FALSE, fillColor = "#D5EAF1", fillOpacity = 0.75) %>%
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
        addPolylines(data = roads_primary, group = "Roads", stroke = TRUE, color = "#F69E61", weight = 1.5, opacity = 0.4) %>%
        addCircleMarkers(data = jmmi_coord,
          fillColor = ~func(USD),
            color = 'black',
            fillOpacity = 1,
            popup = ~paste(sep = " ", USD, 'SSP'),
            label = ~Location,
            radius = 7,
            weight = 1,
            stroke = NULL,
            group = 'September Third Week SSP Rate') %>%
        addCircleMarkers(data = jmmi_coord_2,
          fillColor = ~func_2(USD),
            color = 'black',
            fillOpacity = 1,
            popup = ~paste(sep = " ", USD, 'SSP'),
            label = ~Location,
            radius = 7,
            weight = 1,
            stroke = NULL,
            group = 'September SSP Rate') %>%
        addLayersControl(baseGroups = c('September SSP Rate', 'October SSP Rate'),
            options = layersControlOptions(collapsed = FALSE),
            ) %>%
        hideGroup('September First Week Median SSP Rate') %>%
        addLegend("bottomright", pal = func, values = jmmi_coord$USD,
            title = "October SSP Rate",
            labFormat = labelFormat(prefix = "SSP "),
            opacity = 1,
            layerId = 'October SSP Rate',
            group = 'October SSP Rate',
            na.label = 'No USD Available',
            bins = 3)   %>%
        addLegend("bottomright", pal = func_2, values = jmmi_coord_2$USD,
            title = "September SSP Rate",
            labFormat = labelFormat(prefix = "SSP "),
            opacity = 1,
            layerId = 'September SSP Rate',
            group = 'September SSP Rate',
            na.label = 'No USD Available',
            bins = 3) %>%
        setMapWidgetStyle(list(background = 'transparent'))
  
m

mapshot(m, url = 'sspratemap.html')