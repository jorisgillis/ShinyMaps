
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
    geo.df$features <- lapply(geo.df$features, function(feature) {
      feature$properties <- feature$properties[names(feature$properties) == "NAME_4"]
      names(feature$properties) <- c("Municipality")
      feature
    })
    
    geo.df
  })
  
  loadPopulationNumbers <- reactive({
    # Postal codes
    municipalities <- 
      rbind(
        read.csv('data/postalcodes/Antwerpen.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Brussel.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Henegouwen.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Limburg.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Luik.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Luxemburg.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Namen.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Oost-Vlaanderen.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Vlaams-Brabant.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/Waals-Brabant.csv', sep = ';', stringsAsFactors = FALSE),
        read.csv('data/postalcodes/West-Vlaanderen.csv', sep = ';', stringsAsFactors = FALSE)
      )
    municipalities <- municipalities %>% group_by(Postal.Code) %>% 
      summarise(Municipality = max(Municipality)) %>% ungroup()
    
    # Population number
    population <- read.csv('data/population.csv', sep = ';', stringsAsFactors = FALSE)
    population <- population %>% mutate(Municipality = GEMEENTE, Number = TOTAAL.BEVOLKING) %>%
      select(Municipality, Number) %>% filter(!is.na(Number))
    
    # Combine
    postal.population <- left_join(municipalities, population)
    postal.population
  })
  
  
  ## MAP
  output$map <- renderMap({
    # Loading GeoJSON data
    geo.df <- loadGeoJSON()
    
    # Loading population data
    population <- loadPopulationNumbers()
    
    geo.df$features <- lapply(geo.df$features, function(feature) {
      municipality <- feature$properties["Municipality"]
      p <- population %>% filter(Municipality == toupper(municipality))
      feature$properties[["Population"]] <- max(p$Number, 0)
      feature$properties[["Density"]]    <- rnorm(1, mean = max(p$Number, 0) / 2, sd = max(p$Number, 0) / 16) / max(p$Number, 0)
      feature
    })
    
    # Creating the map
    map <- Leaflet$new()
    map$setView(c(50.5, 4.6), zoom = 8)
    map$tileLayer("http://{s}.www.toolserver.org/tiles/bw-mapnik/{z}/{x}/{y}.png")
    
    # Adding GeoJSON to the map
    map$geoJson(geo.df, 
                style = "#! 
                    function(feature) {
                      if(feature.properties.Density < 0.5)
                        return {
                          'color': '#cc3333',
                          'fillColor': '#cc3333',
                          'opacity': 0.8,
                          'fillOpacity': 0.8,
                          'weight': 2
                        }; 
                      else
                        return {
                          'color': '#33cc33',
                          'fillColor': '#33cc33',
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

