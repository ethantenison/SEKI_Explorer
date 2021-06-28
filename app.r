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
library(shinydashboard)
library(shinydashboardPlus)
library(visNetwork)
library(igraph)


# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
options(scipen = 999)

datalist <- readRDS("./data/shiny_spatial.rds")

polygons <- datalist[1] |>
  as.data.frame() |>
  st_as_sf(crs = 4326, agr = "constant")

nodes <- datalist[2] |>
  as.data.frame() |>
  mutate(across(orgtype, factor)) |>
  st_as_sf(crs = 4326, agr = "constant")


edges <- datalist[3] |> 
  as.data.frame()

datalist2 <- readRDS("./data/shiny_network.rds")
nodes2 <- datalist2[1] |>
  as.data.frame()
edges2 <- datalist2[2] |>
  as.data.frame()

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #


ui <- fluidPage(
  titlePanel("SEKI Explorer"),
  fluidRow(column(5,offset = 1,
                  leafletOutput(
                    "map",
                    width = "700px",
                    height = "700px"
                  )),
           column(5, offset = 1, 
                  visNetworkOutput(
                    "visnet",
                    height = "700px"
                  )
                  ))
  # absolutePanel(
  #   top = "5%",
  #   right = "5%",
  #   selectInput(
  #     "colorvar",
  #     "Select Color Scheme",
  #     choices = c("Physical Location",
  #                 "Organization Type")
  #   )
  #   
  # )
)



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #



server <- function(input, output, session) {
  
  #Palette Function 
  pal <- reactive({ 
    
      
      colorFactor(
        palette = c('red', 'green', 'yellow'),
        na.color = "#808080",
        domain = nodes$orgtype
      )

    
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
  
      leafletProxy("map") |> 
        clearControls() |> 
        clearMarkers() |> 
        addPolygons(data = polygons, label = polygons$Name) |> 
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
    
    
  })
  
  
  #Visnetwork
  output$visnet <- renderVisNetwork({
    visNetwork(
      nodes2,
      edges2,
      main = "SEKI PACE Network",
      width = "100%",
      height = "700px"
    ) |>
       visEdges(
         smooth = T,
         arrows = list(
           to = list(enabled = TRUE, scaleFactor = .5),
           width = 3
         ),
         color = list(highlight = "black")
       ) |> 
       visNodes(color = list(
         background = "white",
         border = "black",
         highlight = list(background = "#A9A9A9", border = "black"),
          hover = list(background = "#A9A9A9", border = "black")
       )) |>
       visIgraphLayout(
         smooth = FALSE,
         physics = FALSE,
         layout = "layout_with_fr",
         randomSeed = 27
       ) 
      
  })
  
}



# ------------------------------- #
# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
# ------------------------------- #

shinyApp(ui, server)