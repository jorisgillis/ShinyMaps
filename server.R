
library(shiny)
library(dplyr)
library(reshape2)
library(jsonlite)

shinyServer(function(input, output) {
  
  loadGeoJSON <- reactive({
    # Loading the administratie boundaries of Belgium
    geoJSON <- readLines('data/BEL_adm4.geojson')
    geo.df  <- fromJSON(geoJSON)
    geo.df  <- head(geo.df) # For testing purposes
    geo.df$type <- unbox(geo.df$type)
    geo.df
  })
  
  feature <- reactive({
    feature <- 
      list(
        type = unbox("FeatureCollection"),
        features = 
          list(
            list(
              type = unbox("Feature"),
              id   = unbox("feature1"),
              properties = list(name = "A Rectangle"),
              geometry = list(
                type = unbox("LineString"),
                coordinates = list(list(list(unbox(49.5), unbox(5.1)), 
                                        list(unbox(51.5), unbox(5.1)), 
                                        list(unbox(51.5), unbox(4.1)), 
                                        list(unbox(49.5), unbox(4.1))))
              )
            )
          )
      )
    feature
  })
  
  output$map <- renderMap({
    # Loading GeoJSON data
    geo.df <- loadGeoJSON()
    
    # Creating the map
    map <- Leaflet$new()
    map$setView(c(50.5, 4.6), zoom = 8)
    map$tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
    
    # Adding GeoJSON to the map
    map$geoJson(toJSON(geo.df), 
                style = "#! 
                    function(feature) {
                      return {
                        'color': '#cc3333',
                        'fillColor': '#cc3333',
                        'weight': 5
                      }; 
                    } !#", 
                onEachFeature = "#!
                      function(feature, layer) {
                        console.log(JSON.stringify(feature));
                        L.polygon(feature.geometry.coordinates).addTo(map);
                      }
                    !#")
    map
  })
})
