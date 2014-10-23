
library(shiny)
library(dplyr)
library(reshape2)
library(RJSONIO)
library(rMaps)

shinyServer(function(input, output) {
  
  loadGeoJSON <- reactive({
    # Loading the administratie boundaries of Belgium
    filename <- 'data/BEL_adm4.geojson'
    geoJSON  <- readChar(filename, file.info(filename)$size)
    geo.df  <- fromJSON(geoJSON)
    
    # Removing unnecessary fields in properties
    geo.df$features <- lapply(features, function(feature) {
      feature$properties <- feature$properties[names(feature$properties) == "NAME_4"]
      names(feature$properties) <- c("Municipality")
      feature
    })
    
    geo.df
  })
  
  output$map <- renderMap({
    # Loading GeoJSON data
    geo.df <- loadGeoJSON()
    
    # Creating the map
    map <- Leaflet$new()
    map$setView(c(50.5, 4.6), zoom = 8)
    map$tileLayer("http://{s}.www.toolserver.org/tiles/bw-mapnik/{z}/{x}/{y}.png")
    
    # Adding GeoJSON to the map
    map$geoJson(geo.df, 
                style = "#! 
                    function(feature) {
                      return {
                        'color': '#cc3333',
                        'fillColor': '#cc3333',
                        'opacity': 0.8,
                        'fillOpacity': 0.8,
                        'weight': 2
                      }; 
                    } !#", 
                onEachFeature = "#!
                      function(feature, layer) {
                        L.polygon(feature.geometry.coordinates).addTo(map);
                      }
                    !#")
    map
  })
})

