# export/save option (image) for leaflet map
# change country and adm0 inputs so they take up less space - multi-select dropdowns - select all and deselect all buttons- https://stackoverflow.com/questions/50616346/r-shiny-pickerinput-select-all-text
# UI - sidebar layout?

# make some title "Map of "indicator" using "file" results" - not sure how to put in country in title


library(tidyverse)
library(sf)
library(raster)
library(shiny)
library(shinyBS)
library(leaflet)
library(leaflet.extras2)
library(shinyWidgets)




ui <- fluidPage(
  tags$head(
    tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
  ),

  fluidRow(
    column(width = 2, offset=0,
  titlePanel("Cadre Harmonise Mapper"),
  #select the date/period - how to make this read from only certain path - like the www folder?
  selectInput(inputId = "file", label = "Select the reference period you would like to map", choices = c("March 2021 - projected","March 2021 - current","November 2020 - current")),
  #select indicator to map- cant get this one to work
  selectInput(inputId = "indicator", label = "Select the indicator you would like to map", choices = c("Final phase","Food Consumption phase","Livelihoods phase","Nutrition phase","Mortality phase")),
  #select country to map
  pickerInput(inputId="country", label="Select Countries to map", choices="", selected="", multiple = TRUE, options=list(`actions-box`=TRUE, `none-selected-text`="Select Country")), #style="background-color:white;

  pickerInput(inputId="adm1", label="Select 1st level adminstrative areas to map", choices="", selected="", multiple = TRUE, options=list(`actions-box`=TRUE, `none-selected-text`="Select Adm1")), #style="background-color:white;

  pickerInput(inputId="adm2", label="Select 2nd level adminstrative areas to map", choices="", selected="", multiple = TRUE, options=list(`actions-box`=TRUE, `none-selected-text`="Select Adm2")), #style="background-color:white;


  actionButton("printbutton", "Print Map"),
    ), #end of column1
  column(width=10, offset = 0,
  textOutput("title"),
  leafletOutput("map", height="100vh")

  ) #nd of column 2
  ) #end of fluidRow
)





server <- function(input, output, session) {

  output$title <- renderText ({
    paste("This is a map of", input$indicator, "using: ", input$file, " results")
  })

  GISfile <- reactiveValues()


  observe({
    if(input$file=="March 2021 - projected"){
    gisfilz <- read_sf("www/wca_CHIPC_mar2021_projected.gpkg")
    }
    if(input$file=="November 2020 - current"){
      gisfilz <- read_sf("www/wca_CHIPC_nov2020_current.gpkg")
    }
    if(input$file=="March 2021 - current"){
      gisfilz <- read_sf("www/wca_CHIPC_mar2021_current.gpkg")
    }
    sfdf <- as.data.frame(gisfilz)

    #updateSelectInput(session = session, "country",choices = unique(sfdf$adm0_name),selected = unique(sfdf$adm0_name))

    updatePickerInput(session = session, "country",choices = unique(sort(sfdf$adm0_name)),selected = unique(sfdf$adm0_name))

    print(head(sfdf))
    GISfile$df <- gisfilz
    })

  #country level boundaries for the background
  wca_shp0 <- read_sf("www/wca_shp0all.gpkg") %>% mutate(admin0Name = case_when(
    admin0Name == "Côte d'Ivoire" ~ "Cote d'Ivoire", admin0Name == "Guinea Bissau" ~ "Guinea-Bissau", TRUE ~ as.character(admin0Name)
  ))

  observe({
   filt_bounds <-wca_shp0[wca_shp0$admin0Name %in% input$country,]
   output$map <- renderLeaflet({
     leaflet() %>% addPolylines(data=filt_bounds, color="#000000", weight=2) %>% setView(lat= 13, lng=7, zoom=6) %>%
       addMapPane("MapData", zIndex=390)


  })
  })


  #what to map
  #reassign
  pal <-  colorFactor(palette = c("#c6ffc7", "#ffe718", "#e88400","#e02d00","#FFFFFF"),
                      levels = c("1", "2", "3", "4",NA))

  observe({
    CH <- GISfile$df

    filtcountry <-CH[CH$adm0_name %in% input$country,]

    #filtcountry2 <-filtcountry[filtcountry$adm1_name %in% input$adm1,]

    #need to make this apply as a filter
    updatePickerInput(session = session, "adm1",choices = unique(sort(filtcountry$adm1_name)),selected = unique(filtcountry$adm1_name))
    #need to make this apply as a filter
    updatePickerInput(session = session, "adm2",choices = unique(sort(filtcountry$adm2_name)),selected = unique(filtcountry$adm2_name))

    if(input$indicator=="Final phase"){
    leafletProxy("map")  %>% flyToBounds(extent(filtcountry)[1],extent(filtcountry)[3],extent(filtcountry)[2],extent(filtcountry)[4]) %>% clearGroup("shapes") %>% clearControls() %>%
        addPolygons(data = filtcountry, group = "shapes", color = "#808080", weight = 1, fillOpacity = 0.75, options=pathOptions(pane="MapData"),
                                                        fillColor = pal(filtcountry$phase_class),
                                                        popup = paste("Country:", filtcountry$adm0_name, "<br>",
                                                        "Admin1:", filtcountry$adm1_name, "<br>",
                                                        "Admin2:", filtcountry$adm2_name, "<br>",
                                                        "Phase:", filtcountry$phase_class, "<br>"),
                                                         #options=pathOptions(pane="MapData"),
                                                        highlight = highlightOptions(weight = 3, color = "black", bringToFront = FALSE)) %>%
                                          addLegend("bottomleft", pal = pal, values = filtcountry$phase_class,
                                                   title = "Final Phase",
                                         opacity = 4)
    }
    else if(input$indicator=="Food Consumption phase"){
      leafletProxy("map") %>%  clearGroup("shapes") %>% clearControls() %>%
        addPolygons(data = filtcountry,  group = "shapes", weight = 0.25, fillOpacity = 0.75, options=pathOptions(pane="MapData"),
                                          color = pal(filtcountry$foodconsumption_phase),
                                          popup = paste("Country:", filtcountry$adm0_name, "<br>",
                                                        "Admin1:", filtcountry$adm1_name, "<br>",
                                                        "Admin2:", filtcountry$adm2_name, "<br>",
                                                        "Food Consumption Phase:", filtcountry$foodconsumption_phase, "<br>"),
                                                          #options=pathOptions(pane="MapData"),
                                          highlight = highlightOptions(weight = 3, color = "black", bringToFront = FALSE)) %>%
        addLegend("bottomleft", pal = pal, values = filtcountry$foodconsumption_phase,
                  title = "Phase",
                  opacity = 4)
    }
    else if(input$indicator=="Livelihoods phase"){
      leafletProxy("map") %>%  clearGroup("shapes") %>% clearControls() %>%
        addPolygons(data = filtcountry,  group = "shapes", weight = 0.25, fillOpacity = 0.75, options=pathOptions(pane="MapData"),
                    color = pal(filtcountry$livelihoods_phase),
                    popup = paste("Country:", filtcountry$adm0_name, "<br>",
                                  "Admin1:", filtcountry$adm1_name, "<br>",
                                  "Admin2:", filtcountry$adm2_name, "<br>",
                                  "Livelihoods Phase:", filtcountry$livelihoods_phase, "<br>"),
                    #options=pathOptions(pane="MapData"),
                    highlight = highlightOptions(weight = 3, color = "black", bringToFront = FALSE)) %>%
        addLegend("bottomleft", pal = pal, values = filtcountry$livelihoods_phase,
                  title = "Phase",
                  opacity = 4)
    }
    else if(input$indicator=="Nutrition phase"){
      leafletProxy("map") %>%  clearGroup("shapes") %>% clearControls() %>%
        addPolygons(data = filtcountry,  group = "shapes", weight = 0.25, fillOpacity = 0.75,
                    color = pal(filtcountry$livelihoods_phase),
                    popup = paste("Country:", filtcountry$adm0_name, "<br>",
                                  "Admin1:", filtcountry$adm1_name, "<br>",
                                  "Admin2:", filtcountry$adm2_name, "<br>",
                                  "Phase:", filtcountry$livelihoods_phase, "<br>"),
                    #options=pathOptions(pane="MapData"),
                    highlight = highlightOptions(weight = 3, color = "black", bringToFront = FALSE)) %>%
        addLegend("bottomleft", pal = pal, values = filtcountry$livelihoods_phase,
                  title = "Nutrition Phase",
                  opacity = 4)
    }

    else if(input$indicator=="Mortality phase"){
      leafletProxy("map") %>%  clearGroup("shapes") %>% clearControls() %>%
        addPolygons(data = CH,  group = "shapes", weight = 0.25, fillOpacity = 0.75, options=pathOptions(pane="MapData"),
                    color = pal(CH$mortality_phase),
                    popup = paste("Country:", CH$adm0_name, "<br>",
                                  "Admin1:", CH$adm1_name, "<br>",
                                  "Admin2:", CH$adm2_name, "<br>",
                                  "Phase:", CH$mortality_phase, "<br>"),
                    #options=pathOptions(pane="MapData"),
                    highlight = highlightOptions(weight = 3, color = "black", bringToFront = FALSE)) %>%
        addLegend("bottomleft", pal = pal, values = CH$mortality_phase,
                  title = "Mortality Phase",
                  opacity = 4)
    }
            })



  observeEvent(input$printbutton, {
    leafletProxy("map") %>%
      addEasyprint(options=easyprintOptions(exportOnly=TRUE, hidden=TRUE, hideControlContainer = TRUE)) %>%
       easyprintMap(sizeModes="CurrentSize", filename="CHexportedMap")  #A4Portrait
  })










   }

shinyApp(ui = ui, server = server)
