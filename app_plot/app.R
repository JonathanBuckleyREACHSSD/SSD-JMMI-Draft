### 1 Load Packages #################################################################3  


rm(list=ls())
  
  library(stringr)
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(shinythemes)
  library(leaflet)
  library(leaflet.extras)
  library(leaflet.minicharts)
  library(sf)
  library(rsconnect)
  library(tidytidbits)
  library(highcharter)
  library(kableExtra) 
  
  
  #======================================== Load relevant data =======================================================
 
  # Load JMMI Data, marektplace mapping data, coordinates and itemsets
  
  jmmi <- read.csv('1_Data/JMMI_longterm_bylocation.csv', stringsAsFactors = FALSE) # This contains all the longterm data since August 2019
  
  state_location <- read.csv('2_Itemsets/state_location.csv') # This is an itemset which matches states with locations and counties
  
  itemset <- read.csv('2_Itemsets/itemset.csv') # This is an itemset which matches items by category
  
  markets <- read.csv('1_Data/market_maping.csv', stringsAsFactors = FALSE) # This is a dataset that was made after a participatory mapping exercise held by REACH - 
                                                                     # it contains all known marketplaces by field staff and includes size estimates, etc.
  
  coordinates <- read.csv('1_Data/coordinates.csv')                         # This File contains all the JMMI locations along with their coordinates - coordinates need to be updated when new locations are added
  
  # Load shapefiles for MSSMEB map
  
  disputed <- st_read('3_Shapefiles/Disputed/SSD_Undetermined.shp') # Disputed boundaries
  disputed <- st_transform(disputed,"+init=epsg:4326" ) # the transform adapts it to the right GPS coordinates
  
  country <- st_read('3_Shapefiles/Country/SSD_Country.shp')        # Country borders
  country <- st_transform(country,"+init=epsg:4326" )
  
  states <- st_read("3_Shapefiles/States/SSD_States.shp")           # State borders
  states <- st_transform(states,"+init=epsg:4326" )
  
  counties <- st_read("3_Shapefiles/Counties/SSD_counties.shp")    # County Borders
  counties <- st_transform(counties,"+init=epsg:4326" )
  
  rivers <- st_read("3_Shapefiles/Rivers/rivers_primary.shp")          # Primary Rivers
  rivers <- st_transform(rivers, "+init=epsg:4326")
  #st_write(rivers_primary, "rivers_primary.shp")
  
  lakes <- st_read("3_Shapefiles/Lakes/SSD_Lakes.shp")             # Lakes
  lakes <- st_transform(lakes, "+init=epsg:4326")
  
  roads <- st_read("3_Shapefiles/Roads/roads_primary.shp")             # Primary roads
  roads <- st_transform(roads, "+init=epsg:4326")
  #st_write(roads_primary, "roads_primary.shp")
  
  network_zain <- st_read("3_Shapefiles/Network/network_zain.shp") # Zain network connectivity hexagons (as the primary telco)
  network_mtn <- st_read("3_Shapefiles/Network/network_mtn.shp")   # MTN network connectivity hexagons (as the primary telco)
  network_both <- st_read("3_Shapefiles/Network/network_both.shp") # Hexagons for which areas in which both networks work equally
  

  
 #----------------------------------------- Create Aggregated Dataset ---------------------------------------------------------------
  
  # Add a date to the long term data set 
  
  jmmi$Date <-as.Date(paste(jmmi$Year, jmmi$Month, "01", sep = '/'))
  
  # create dataset and format the names
  
  jmmi_table <- jmmi %>%
      select(Date, State, County, Location, everything(),-c('Year', 'Month', 'X38','Covid.price.index', 'Casual.labor')) %>%
      mutate(Location = gsub("_"," ", Location),
             Location = gsub("([a-z])([A-Z])", "\\1 \\2", Location),     # This line makes spaces between different words that start with capital letters
             Location = gsub("Po C", "PoC", Location),                   # This corrects Po C to PoC (an error from the previous line)
             State = gsub("([a-z])([A-Z])", "\\1 \\2", State),
             County = gsub("([a-z])([A-Z])", "\\1 \\2", County)) %>%
      rename('Sorghum Grain (1 kg)' = Sorghum.grain,
             'Maize Grain (1 kg)' = Maize.grain,
             'Wheat Flour (1 kg)' = Wheat.flour,
             'Rice (1 kg)' = Rice,
             'Groundnuts (shelled) (1 kg)' = Groundnuts,
             'Beans (1 kg)' = Beans,
             'Sugar (1 kg)' = Sugar,
             'Salt (1 kg)' = Salt,
             'Cooking Oil (1L)' = Cooking.oil,
             'Water (1L)' = Water,
             'Soap (200 g)' = Soap,
             'Jerrycan (1 pc)' = Jerrycan,
             'Mosquito Net (1 pc)' = Mosquito.net,
             'Exercise Book (1 pc)' = Exercise.book,
             'Blanket (1 pc)' = Blanket,
             'Cooking Pot (1 pc)' = Cooking.pot,
             'Plastic Sheet (1 pc)' = Plastic.sheet,
             'Pole (1 pc)' = Pole,
             'Firewood (1 bundle)' = Firewood,
             'Charcoal (1 kg)' = Charcoal,
             'Goat (1 unit)' = Goat,
             'Chicken (1 unit)' = Chicken,
             'MSSMEB Food Basket' = MSSMEB.food.basket,
             'Food Price Index' = Food.price.index)

# Format the state location setlist so the name matches what is in the JMMI dataset  

  state_location <- state_location %>%
  mutate(Location = gsub("_"," ", Location),
         Location = gsub("([a-z])([A-Z])", "\\1 \\2", Location),
         Location = gsub("Po C", "PoC", Location),
         State = gsub("([a-z])([A-Z])", "\\1 \\2", State),
         County = gsub("([a-z])([A-Z])", "\\1 \\2", County))
  
# Make a long version which is easier to read for plots

jmmi_table_long <- gather(jmmi_table, key = Item, value = Price, 5:ncol(jmmi_table))  
  
# Combine the coordinates with the JMMI Data

jmmi_coord <- left_join(coordinates, jmmi_table, by = 'Location') %>%
  filter(!is.na(MSSMEB))

jmmi_coord_variables <- select_if(jmmi_coord, is.numeric)

jmmi_coord_variables <- jmmi_coord_variables %>%
  select(-c('Lat', 'Lon'))

  
# ========================================== Reference List ================================================================  
  

  dates <- sort(unique(jmmi_table_long$Date))                                           # define list with date range in data
  dates_min  <- as.Date("2019-08-01")                                                   # set minimum date to be displayed
  dates_max  <- max(jmmi_table_long$Date)                                               # maximum date in data
  dates_max2 <- sort(unique(jmmi_table_long$Date), decreasing=T)[2]                     # second-latest date
  
  dates_max_1y <- as.POSIXlt(dates_max)                                                 # most recent month minus 1 year
  dates_max_1y$year <- dates_max_1y$year-1
  dates_max_1y <- as.Date(dates_max_1y)
  
  
  unique_size <- unique(as.character(markets$size))                                                  # Identify market sizes according to the options avilable within the map file
  unique_size <- unique_size[1:7]
  unique_size_order <- c('dont_know', '1_15', '16_50', '51_100', '101_200','201_500', 'above_500')   # Order them so that it appears from smallest to 
  unique_size <- unique_size[order(match(unique_size, unique_size_order))]
  
  unique_type <- unique(as.character(markets$type))                                                  # Identify market type options for the marketplace map
  unique_type <- unique_type[1:2]
  

# ------------------------------------------------Aggregate data to different levels ---------------------------------
  
  jmmi_aggregate_country <- jmmi_table_long %>%
    select(Date, Item, Price) %>%
    group_by(.dots = c('Date', 'Item')) %>%
    summarise_all(funs(median(., na.rm = TRUE)))
  

### Define the MSSMEB Dataframe #####################################
  
  meb <- data.frame(Category = c(rep("Food Items", 4), rep("Non-Food Items (Monthly)", 13), rep("Non-Food Items (one-off)*", 7)),         # define SMEB content table
                     Item = c("Cereals", "Pulses", "Vegetable Oil", "Salt", 
                              "Charcoal", "Milling Cost", "Bar Soap", "Bleach*", "Human Drugs", "Airtime", "Transport", "School Fee", "Exercise Book", "Pencil*", "Pen*", "Rubber*", "Sharpener*",
                              "Blanket", "Mosquito Net", "Kitchen Set", "Jerrycan", "Sanitary Pad", "Underwear", "Kanga**"
                              ),
                     Quantity = c("90 kg", "9 kg", "6 l", "1 kg",
                                  "50 kg", "30 kg", "6 pcs", "1.5 l", "10 USD", "30 min", "3 USD", "3 USD", "12 pcs", "6 pcs", "6 pcs", "3 pcs", "3 pcs",
                                  "2 pcs", "2 pcs", "1 pc", "2 pcs", "4 pcs", "4 pcs", "2 pcs"))
  
  meb_kbl <- meb %>%                                                                                      # make a html (kable) object out of dataframe
    kbl(bootstrap_options = c("hover", "condensed"), escape = F) %>%
    kable_styling(fixed_thead = T, full_width = F) %>%
    column_spec(1, width = "8em", bold = T) %>%
    column_spec(2, width = "10em") %>%
    column_spec(3, width = "8em") %>%
    collapse_rows(columns = 1, valign = "top")
  
    
#----------------------------------------------------UI---------------------------------------------------------------
  
  # Define UI 
  ui <- fluidPage(theme = shinytheme("sandstone"),
    navbarPage('JMMI Dashboard',
               
               ################################################## 2 Plots ################################################
               
               tabPanel("Plot",                                                                               # set panel title
                        icon = icon("chart-line"),                                                            # select icon
                        chooseSliderSkin(skin = "Nice", color = NULL),                                        # set theme for sliders
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            tags$i(h6("Note: Reported values are indicative only.", style="color:#045a8d")),
                            
                            pickerInput("plot_aggregation",
                                        label = "Aggregation level:",
                                        choices = c("Location", "Country"),
                                        selected = "Location",
                                        multiple = FALSE
                            ),
                            
                            conditionalPanel(condition = "input.plot_aggregation == 'Location'",
                                             radioGroupButtons("plot_by_location_item",
                                                               label = "Group by:",
                                                               choices = c("Item", "Location"),
                                                               selected = "Item",
                                                               justified = TRUE
                                             )
                            ),
                            
                            hr(),
                            
                            conditionalPanel(condition = "input.plot_aggregation == 'Location' & input.plot_by_location_item == 'Location'",
                                             pickerInput("select_bylocation_location",
                                                         label = "Location(s):",
                                                         choices = lapply(split(as.character(state_location$Location), as.character(state_location$State)), as.list),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = c('Juba Town'),
                                                         multiple = TRUE
                                             )
                            ),
                            
                            conditionalPanel(condition = "input.plot_aggregation == 'Location' & input.plot_by_location_item == 'Location'",
                                             pickerInput("select_bylocation_item",
                                                         label = "Item:",   
                                                         choices = lapply(split(as.character(itemset$Item), as.character(itemset$Category)), as.list),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = "MSSMEB",
                                                         multiple = FALSE
                                             )
                            ),
                            
                            conditionalPanel(condition = "input.plot_aggregation == 'Location' & input.plot_by_location_item == 'Item'",
                                             pickerInput("select_byitem_location",
                                                         label = "Location:",
                                                         choices = lapply(split(as.character(state_location$Location), as.character(state_location$State)), as.list),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = "Juba Town",
                                                         multiple = FALSE
                                             )
                            ),
                            
                            conditionalPanel(condition = "input.plot_aggregation == 'Country' | (input.plot_aggregation == 'Location' & input.plot_by_location_item == 'Item')",
                                             pickerInput("select_byitem_item",
                                                         label = "Item(s):",   
                                                         choices = lapply(split(as.character(itemset$Item), as.character(itemset$Category)), as.list),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = c("MSSMEB", "MSSMEB Food Basket", "Food Price Index"),
                                                         multiple = TRUE
                                             )
                            ),
                            
                            sliderTextInput("select_date",                                                # set date slider
                                            "Month:",
                                            choices = dates,
                                            selected = c(dates_min, dates_max)
                            ),
                            
                            h6("Select aggregation level, item(s), location(s) and month from drop-down menues to update plot.
                                   Displayed values are median prices - retail prices are first aggregated on site level and then
                                   on district level (and then on governorate/country level)."),
                            
                            absolutePanel(bottom = 20, left = 20, width = 200,                            # define blue info button
                                          fixed=TRUE, draggable = FALSE, height = "auto",
                                          dropdownButton(
                                            h4("MSSMEB contents"),
                                            HTML(meb_kbl),
                                            span(tags$ol(
                                              tags$li("The MSSMEB represents the minimum culturally adjusted group of items required to support a six-person South Sudanese household for one month, as defined by the CWG."),
                                              tags$li("*Only the MSSMEB's key elements (food and monthly non-fooditems, excluding bleach, pencils, pens, rubbers and sharpeners) were incorporated into the calculations in this dashboard"),
                                              tags$li("** Traditional piece of garment"))),
                                            circle = TRUE,
                                            status = "info",
                                            size = "xs",
                                            up = TRUE,
                                            icon = icon("info"), width = "400px",
                                            tooltip = tooltipOptions(title = "Click to find more information about the composition of the MSSMEB.")
                                          )
                            ),
                            
                            width = 3,                                                                    # set bootstrap width of sidebar (out of 12)
                          ),                                                                                # close sidebar panel
                          
                          mainPanel(
                            tags$i(textOutput("plot_text"), style = "color: red"),                        # display error message displayed if there is no data available
                            highchartOutput("graph", width = "100%", height = "600px"),                   # display large chart
                            width = 8                                                                     # set width of main panel (out of 12, as per bootstrap logic)
                          )
                        )),
               
               
              #   tabPanel('Plots',
              #            sidebarLayout(
              #              sidebarPanel(
              #               fluidRow(   
              #    pickerInput(inputId = 'location',label = 'Choose your Location',
              #                choices = lapply(split(as.character(state_location$Location), as.character(state_location$State)), as.list),
              #                selected = 'Juba Town', multiple = TRUE,
              #                options = list(`actions-box` = TRUE, size = 10)
              #                )),
                  
              #               fluidRow(
              #    pickerInput(inputId = 'item',label = 'Choose the item',
              #                choices = lapply(split(as.character(itemset$Item), as.character(itemset$Category)), as.list),
              #                selected = 'MSSMEB', 
              #                options = list(`actions-box` = TRUE, size = 10)
              #                ))),
                  
              #    mainPanel(plotlyOutput('jmmi'))
                  
              #    )),
               
               
               ############################################## 3 Data Explorer ######################################################
                  
                tabPanel('Data Explorer',
                             icon = icon("table"),
                             sidebarLayout(
                             sidebarPanel(
                             fluidRow(
                  pickerInput(inputId = 'location_table',label = 'Choose your Location',
                             choices = lapply(split(as.character(state_location$Location), as.character(state_location$State)), as.list),
                             selected = NULL, 
                             options = list(`actions-box` = TRUE, size = 10)
                             ),
                  
                  pickerInput(inputId = 'item_table', label = 'Choose an Item', 
                             choices = lapply(split(as.character(itemset$Item), as.character(itemset$Category)), as.list), 
                             multiple = TRUE,
                             selected = 'MSSMEB',
                             options = list(`actions-box` = TRUE, size = 10)
                             ),
                  
                  sliderTextInput(inputId = 'date_table',
                             label = 'Choose by Date',
                             choices = dates,
                             selected = c(dates_min, dates_max))                    
                             ),
                  
                  actionButton("table_reset", "Reset filters"),
                  
                  downloadButton("downloadData", "Download as CSV"),
                  
                  width = 3),
                  
                  mainPanel(dataTableOutput('table'),
                            width = 9)
                  
                  )),
               
        ############################################## 4 Marketplace Maps #####################################################
               
#                tabPanel('Mapped Markets',
#                             icon = icon("map"),
#                             sidebarLayout(
#                             sidebarPanel(
#                             fluidRow(
#                pickerInput(inputId = 'size', 
#                            label = 'Choose a location size',
#                            choices = unique_size,
#                            selected = unique_size,             
#                            multiple = TRUE, 
#                            options = list(`actions-box` = TRUE, size = 10)
#                )),
#                            fluidRow(
#                pickerInput(inputId = 'state',
#                            label = 'Choose a State',
#                            choices = unique(as.character(markets$state)),
#                            selected = unique(as.character(markets$state)),
#                            multiple = TRUE,
#                            options = list(`actions-box` = TRUE, size = 10)
#                )),
#                            fluidRow(
#                pickerInput(inputId = 'type',
#                            label = 'Choose a Market Type',
#                            choices = unique_type,
#                            selected = unique_type,
#                            multiple = TRUE,
#                            options = list(`actions-box` = TRUE, size = 10)
#                )),                
#                            width = 3),
                
#                  mainPanel(leafletOutput('marketplaces', height = 1000))
                      
#                )),

tabPanel("Mapped Markets", icon = icon("map"),
         
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
             ),
             
             leafletOutput("marketplaces", width = "100%", height = "100%"),
             
             absolutePanel(
               id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
               width = 300, height = "auto",
               
               pickerInput(inputId = 'size', 
                           label = 'Choose a location size',
                           choices = unique_size,
                           selected = unique_size,             
                           multiple = TRUE, 
                           options = list(`actions-box` = TRUE, size = 10)
               ),
               pickerInput(inputId = 'state',
                           label = 'Choose a State',
                           choices = unique(as.character(markets$state)),
                           selected = unique(as.character(markets$state)),
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE, size = 10)
               ),
               pickerInput(inputId = 'type',
                           label = 'Choose a Market Type',
                           choices = unique_type,
                           selected = unique_type,
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE, size = 10)
                           
                ),                                                                              # close sliderTextInput
             h6(em("Network information comes from the REACH Area of Knowledge data collection as of June. 
                   The hexagons represent which network is preferred in which location, 
                   not all locations in which the network is present")))                       # close absolutePanel
         )                                                                                     # close div for class outer
),                                                                                             # close tab panel
        
        ##################################### Price Maps ##################################################
               
#               tabPanel('Price Maps',
#                           icon = icon("map"),
#                           sidebarLayout(
#                           sidebarPanel(
#                           fluidRow(
#               selectInput(inputId = 'date_2',
#                           label = 'Choose by Date',
#                           choices = unique(jmmi_table$Date),
#                           selected = 'Oct 2020')
#                           ),
#                           fluidRow(
#               selectInput(inputId = 'map_item_selected',
#                           label = 'Choose an item',
#                           choices = names(jmmi_coord_variables),
#                           selected = 'MSSMEB'
#                           ))
#               ),
               
#                   mainPanel(leafletOutput('prices', height = 1000))        
                           
#               ))
               
               
#               ))

tabPanel("Price Maps", icon = icon("map"),
         
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
             ),
             
             leafletOutput("prices", width = "100%", height = "100%"),
             
             absolutePanel(
               id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
               width = 300, height = "auto",
               
               selectInput(inputId = 'date_2',
                           label = 'Choose by Date',
                           choices = unique(jmmi_table$Date),
                           selected = dates_max
               ),
               selectInput(inputId = 'map_item_selected',
                           label = 'Choose an item',
                           choices = names(jmmi_coord_variables),
                           selected = 'MSSMEB'
               )
               )                                                                       # close absolutePanel
         )                                                                                     # close div for class outer
)                                                                                              # Close Tab Panel
)                                                                                              # Close Navbarpage
)                                                                                              # Close Fluidpage  
                               
# ==================================================== Server =========================================================
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
  ######################################################## Plots ###############################################################
    
        #make the plot
    
    plot_location_select <- reactive({
      if (input$plot_by_location_item == "Item") {input$select_byitem_location} else {input$select_bylocation_location}
    })
    
    plot_item_select <- reactive({
      if (input$plot_aggregation == 'Country' | (input$plot_aggregation == 'Location' & input$plot_by_location_item == "Item")) {input$select_byitem_item} else if (input$plot_aggregation == 'Location' & input$plot_by_location_item == 'Location') {input$select_bylocation_item}
    })
    
    plot_datasetInput <- reactive({jmmi_table_long %>%
        filter(
          is.null(plot_item_select()) | Item %in% plot_item_select(),
          Date >= input$select_date[1] & Date <= input$select_date[2]
        ) %>%
        execute_if(input$plot_aggregation == 'Location', filter(is.null(plot_location_select()) | Location %in% plot_location_select())) %>%
        execute_if(input$plot_aggregation == 'Location', select(-State,-County)) %>%
        execute_if(input$plot_aggregation == 'Country', select(-State, -County, -Location)) %>%
        execute_if(input$plot_aggregation == 'Country', group_by_(.dots = c("Date", "Item"))) %>%
        execute_if(input$plot_aggregation == 'Country', summarise_all(funs(median(., na.rm = TRUE))))
    })
    
    output$plot_text <- renderText({
      if (nrow(plot_datasetInput()) == 0)
      {"There is no data for this selection. Change the time frame or select another indicator."}
      else ("")
    })
    
    output$graph <- renderHighchart({
      
      if (input$plot_aggregation == "Country" | (input$plot_aggregation == "Location" & input$plot_by_location_item == "Item")) {
        graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Item)) %>%
          hc_yAxis(min = 0, title = list(text = "Price (in SSP)")) %>%
          hc_xAxis(title = "") %>%
          hc_exporting(
            enabled = TRUE,
            filename = paste0("SSD-JMMI-plot_export-", Sys.Date()),
            buttons = list(
              contextButton = list(
                menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")
              )),
            sourceWidth = 1000,
            sourceHeight = 600
          ) 
        
      } else if (input$plot_aggregation == "Location"){
        graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Location)) %>%
          hc_yAxis(min = 0, title = list(text = "Price (in SSP)")) %>%
          hc_xAxis(title = "") %>%
          hc_exporting(
            enabled = TRUE,
            filename = paste0("SSD-JMMI-plot_export-", Sys.Date()),
            buttons = list(
              contextButton = list(
                menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")
              )),
            sourceWidth = 1000,
            sourceHeight = 600
          ) 
        
      }    
    })
    
    
                     
   # jmmi_reactive <- reactive({ggplot(data = subset(jmmi_table_long, Location %in% input$location & Item %in% input$item),aes(x = Date, y= Price, group=Location)) +
   #                                    geom_line(aes(color = Location)) +
   #                                    geom_point(aes(color = Location)) +
   #                                    theme_bw() +
   #                                    theme(text = element_text(family = 'Arial Narrow', face = 'bold', size = 10))
   #                                     })
    
   # output$jmmi <- renderPlotly({jmmi_reactive()})
    
   ######################################################### 3.1 Data Explorer #################################################
    
    conditional <- function(condition, success) {
      ifelse(condition, success, TRUE)
    }
    
    reactive_table <-reactive({jmmi_table_long <- jmmi_table_long %>%
      select(Date, State, County, Location, Item, Price) %>%
      filter(conditional(Location != "", Location %in% input$location_table)) %>%
      filter(Date >= input$date_table[1] & Date <= input$date_table[2]) %>%
      filter(conditional(Item != "", Item %in% input$item_table))})
    
    output$table <- renderDataTable(reactive_table())
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("SSD-JMMI-data-download-", Sys.Date(),".csv", sep = "")
      },
      content = function(file) {
        write.csv(reactive_table(), file, row.names = FALSE)
      }
    )
    
    observe({
      input$table_reset
      updatePickerInput(session, "location_table", selected = "Juba Town")
      updatePickerInput(session, "item_table", selected = "MSSMEB")
      updateSliderTextInput(session, "date_table", selected = c(dates_min, dates_max))
    }) 
    
    
    
  ############################################################ Marketplace Map ##################################################
  
   # function to map out size of marketplaces
  
  
  market_radius <- function (x) {ifelse(x %in% '1_15',2.5,
                                        ifelse(x %in% '16_50',3,
                                               ifelse(x %in% '51_100',3.5,
                                                      ifelse(x %in% '101_200', 4,
                                                             ifelse(x %in% '201_500', 4.5,
                                                                    ifelse(x %in% 'above_500', 6,
                                                                           ifelse(x %in% 'dont_know', 1, 1)))))))
    
    
  }
  
  # filter out the marketplace NA data
  
  markets <- markets %>%
    filter(!is.na(lat) & !is.na(lon) & !is.na(size))
  
  # make the markeplace map using leaflet
  
  market_map <- reactive({leaflet(subset(markets, size %in% input$size & state %in% input$state & type %in% input$type)) %>%
    addTiles(group = 'OSM') %>%
    addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
    addPolygons(data = network_both, group = 'Both MTN and Zain', fill = TRUE, stroke = FALSE, fillColor = '#58585A', fillOpacity = .5, popup = ~Network) %>%
    addPolygons(data = network_zain, group = 'Zain', fill = TRUE, stroke = FALSE, fillColor = '#0067A9', fillOpacity = .5, popup = ~Network) %>%
    addPolygons(data = network_mtn, group = 'MTN', fill = TRUE, stroke = FALSE, fillColor = '#F69E61', fillOpacity = .5, popup = ~Network) %>%
    addCircleMarkers(color = 'red',
                     radius = ~market_radius(size),
                     popup = ~marketplace,
                     stroke = FALSE,
                     group = 'Marketplaces') %>%
    addLayersControl(baseGroups = c("OSM", "Satellite"), 
                     overlayGroups = c("Marketplaces", "Both MTN and Zain", "Zain", "MTN"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("Both MTN and Zain", "Zain", "MTN")) %>%  
    setView(lng = 30.2036, lat = 7.1376, zoom = 6.5)
    })
  
  # render the map
  
  output$marketplaces <- renderLeaflet({market_map()}) 
  
  
  # Make the MSSMEB map - first get the data and make the inputs reactive
  # Prepare the reactive elements for the price map
  
  map_time_select <- reactive({input$date_2})
  map_item_select <- reactive({input$map_item_selected})
  
  map_data_initial <- jmmi_coord %>%
    dplyr::select(Date, State, Location, MSSMEB, Lon, Lat) %>%
    filter(Date == dates_max)
  
  map_data_selected <- reactive({
    jmmi_coord %>%
      select(State, County, Location, input$map_item_selected, Lon, Lat, Date) %>%
      filter(Date == input$date_2)
  })
  
  map_data_selected_2 <- reactive({
    map_data_selected() %>%
      dplyr::select(input$map_item_selected)
  })
  
  # Render the map
  
  
  output$prices <- renderLeaflet({
    
    # create the color palette, which is used for the map circles
    pal <- colorNumeric(palette = c("#A2CD91", "#FFF54C","#ED5758"),
                        domain = map_data_initial$MSSMEB
    )
    
    map_data_initial %>% leaflet() %>%
      setView(lat = 7.7, lng = 30, zoom = 7) %>%
      addPolygons(data = lakes, group = "Lakes", fill = TRUE, stroke = FALSE, fillColor = "#D5EAF1", fillOpacity = 0.75) %>%
     # addPolygons(data = disputed, group = "Disputed", fill = FALSE, stroke = TRUE, dashArray = c(5, 5), color = "#58585A", weight = 1, opacity = 0.7) %>%
      addPolygons(data = counties, group = "Counties", fill = FALSE, stroke = TRUE, color = "#BDBDBD", weight = 0.6, opacity = 0.5) %>%
      addPolygons(data = states, group = "States", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1, opacity = 0.7) %>%
      addPolylines(data = rivers, group = "Rivers", stroke = TRUE, color = "#94CCDC", weight = 1.3, opacity = 0.7) %>%
      addPolylines(data = roads, group = "Roads", stroke = TRUE, color = "#F69E61", weight = 1.5, opacity = 0.4) %>%
      addCircleMarkers(lng = ~Lon, lat = ~Lat,
                       radius = 11,
                       fillColor = ~pal(map_data_initial$MSSMEB),
                       fillOpacity = 1,
                       stroke = TRUE,
                       weight = 1,
                       color = "black",
                       popup = paste("SSP", as.factor(map_data_initial$MSSMEB)),
                       popupOptions(closeButton = TRUE),
                       label= ~Location
      ) %>%
      addLegend("bottomright", pal = pal, values = map_data_initial$MSSMEB,
                title = "Price:",
                labFormat = labelFormat(prefix = "SSP "),
                opacity = 1
      ) %>%
      setMapWidgetStyle(style = list(background = "transparent"))
  })
  
  # Add the reactive elements for the new map
  
    observe({
    
    map_data_selected_update <- map_data_selected()
    
    pal_2 <- colorNumeric(palette = c("#A2CD91", "#FFF54C","#ED5758"),
                          domain = map_data_selected_2())
  
    leafletProxy('prices', data = map_data_selected_update) %>%
    clearMarkers() %>%
    clearControls %>%
    addCircleMarkers(color = 'black',
                     fillColor = ~pal_2(map_data_selected_update[[input$map_item_selected]]),
                     fillOpacity = 1,
                     popup = ~paste(round(map_data_selected_update[[input$map_item_selected]]), "SSP", sep = " "),
                     label = ~Location,
                     radius = 11,
                     weight = 1) %>%
      addLegend("bottomright", pal = pal_2, values = map_data_selected_update[[input$map_item_selected]],
                title = "Price:",
                labFormat = labelFormat(prefix = "SSP "),
                opacity = 1
      )
       })
 
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

