library(sf) # the package that converts shp files to readable numeric values
library(dplyr) # for tidying and formatting data
library(leaflet) # for creating the map 
library(leaflet.extras) # for some extra formatting options
library(leaflet.providers) # to add extra basemaps beyond the basic OSM


network <- st_read("Network/Cell_Coverage_Hex.shp") # Network connectivity hexagons
network <- st_transform(network, "+init=epsg:4326") # This locates the hexagons according to coordinates in SSD

network_zain  <- network %>%                                   # Filter out the network files for areas in which Zain is the primary provider
 filter(P_001.ph_1 > P_001.ph_3) %>%
 mutate(Network = "Zain")                                      # Add a column so that you can have this as a label as a 'popup' in leaflet
st_write(network_zain, 'network_zain.shp')                     # Rewrite the shapefile with only the zain hexagons (you don't need to to this but it helps to reduce file size)

network_mtn <- network %>%                                     # Filter out the network files for areas in which MTN is the primary provider
 filter(P_001.ph_3 > P_001.ph_1) %>%
 mutate(Network = "MTN")                                       # Add a column so that you can have this as a label as a 'popup' in leaflet
st_write(network_mtn, 'network_mtn.shp')                       # Rewrite the shapefile with only the mtn hexagons

network_both <- network %>%                                                    # Filter out the network files for areas in which both providers are equally functional
  filter((P_001.ph_3 & P_001.ph_1 > 0) & (P_001.ph_3 == P_001.ph_1)) %>%       
  mutate(Network = "Both MTN and Zain")
 st_write(network_both, 'network_both.shp')                    # Add a layer so that you can have this as a label as a 'popup' in leaflet
plot(st_geometry(network_both))                                # Rewrite the shapefile with hexagons with both telcos

network_none <- network %>%                                    # Filter out the network files for areas in which no network is reported
  filter(P_001.ph_3 + P_001.ph_1 == 0) %>%
  mutate(Network = 'No network reported')                      # Add a layer so that you can have this as a label as a 'popup' in leaflet
st_write(network_none, 'network_none.shp')                     # Rewrite the shapefile with hexagons with no telcos
  

plot(st_geometry(network_both))

m <- leaflet() %>%  # call the leaflet function - you can also add specific long and lat points to the leaflet() call but it isn't necessary to run the package (ie. if you had specific settlements with points you were mapping)
  setView(lat = 7.7, lng = 30, zoom = 7) %>% # sets the long and lat which immediately appear when the map is loaded
  addTiles() %>%                             # Adds the default OSM OpenSourceMap - you can go to https://leaflet-extras.github.io/leaflet-providers/preview/ to see other options that can be used as basemaps
  addPolygons(data = network_both, fill = TRUE, stroke = FALSE, fillColor = '#58585A', fillOpacity = .5, popup = ~Network) %>% # add the 'both' data as a polygon with .5 opacity and colour grey, with the 'Network' column as the popup label
  addPolygons(data = network_zain, fill = TRUE, stroke = FALSE, fillColor = '#0067A9', fillOpacity = .5, popup = ~Network) %>% # add the zain data as a polygon with .5 opacity and colour blue, with the 'Network' column as the popup label
  addPolygons(data = network_mtn, fill = TRUE, stroke = FALSE, fillColor = '#F69E61', fillOpacity = .5, popup = ~Network) # add the mtn data as a polygon with .5 opacity and colour grey, with the 'Network' column as the popup label

m # load the map in RStudio 'viewer' and have a look