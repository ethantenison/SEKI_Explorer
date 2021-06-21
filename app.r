# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #



library(shiny)
library(shinyjs)
library(RColorBrewer)
library(tidyverse)
library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)
library(shinyWidgets)
library(rsconnect)


# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
options(scipen = 999)


edges <- readRDS("./data/edges_4_shiny.rds")
edges <- data.frame(group = c(edges$ego, edges$alter),
                    lat = c(edges$centroid_lat.x, edges$centroid_lat.y),
                    lon = c(edges$centroid_lon.x, edges$centroid_lon.y)
)


nodes <- readRDS("./data/nodes_4_shiny.rds")
nodes$location <- as.factor(nodes$location)
nodes$orgtype <- as.factor(nodes$orgtype)
nodes= st_as_sf(nodes, coords = c("centroid_lon", "centroid_lat"), 
                   crs = 4326, agr = "constant", remove = FALSE)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = "5%",
    right = "5%",
    selectInput(
      "colorvar",
      "Select Color Scheme",
      choices = c("Physical Location",
                  "Organization Type")
    )
    
  )
)



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #



server <- shinyServer(function(input, output, session) {
  
  #Palette Function 
  pal <- reactive({ 
    
    if (input$colorvar %in% "Physical Location"){ 
      
      colorFactor(
        palette = c('red', 'blue'),
        domain = nodes$location
      )
    }
    
    else {
      colorFactor(
        palette = c('red', 'blue', 'green', 'yellow', 'purple','orange'),
        na.color = "#808080",
        domain = nodes$orgtype
      )
    }
  })
  
  
  
  #Basemap that doesn't change 
  output$map <- renderLeaflet({
    leaflet(nodes) |> 
      #addTiles() |> 
      addTiles(urlTemplate = 'https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png',
               attribution = '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy;
               <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors') |>
      setView(-118.60, 36.73, zoom = 8)
    
    
  })
  
  observe({
    
    pal <- pal()
    
    if (input$colorvar %in% "Physical Location"){ 
    leafletProxy("map") |> 
      clearControls() |> 
      clearMarkers() |> 
      addCircleMarkers(
        data = nodes,
        lng =  ~ nodes$centroid_lon,
        lat =  ~ nodes$centroid_lat,
        fillColor = ~pal(nodes$location),
        color = ~pal(nodes$location),
        radius = nodes$value,
        fillOpacity = 0.7,
        label = nodes$id
      ) |> 
        addPolylines(data = edges,
                     lng = ~lon,
                     lat = ~lat,
                     group = ~group,
                     weight = 1,
                     opacity = 0.5) |> 
        addLegend(data = nodes,
                  "bottomright",
                  pal = pal,
                  values = ~nodes$location,
                  title = "")
    } else {
      leafletProxy("map") |> 
        clearControls() |> 
        clearMarkers() |> 
        addCircleMarkers(
          data = nodes,
          lng =  ~ nodes$centroid_lon,
          lat =  ~ nodes$centroid_lat,
          fillColor = ~pal(nodes$orgtype),
          color = ~pal(nodes$orgtype),
          radius = nodes$value,
          fillOpacity = 0.7,
          label = nodes$id
        ) |> 
        addPolylines(data = edges,
                     lng = ~lon,
                     lat = ~lat,
                     group = ~group,
                     weight = 1,
                     opacity = 0.5,
                     color = "red") |> 
        addLegend(data = nodes,
                  "bottomright",
                  pal = pal,
                  values = ~nodes$orgtype,
                  title = "")
      }
    
  })
  
})



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)