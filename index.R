
library(sf)
library(dplyr)
library(lwgeom)
library(purrr)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(rmapshaper)




# LOAD IN THE SHAPEFILE. USE RMAPSHAPER TO REDUCE THE SIZE OF THE SHAPEFILE WITHOUT SACRIFICING TOO MUCH DETAIL

provinces.sf <- rmapshaper::ms_simplify(st_read("C:/file_path/Provinces", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326))

# MAKE THE PROVINCE ID NUMERIC SO WE CAN WORK WITH THEM

provinces.sf$PRUID <- as.numeric(provinces.sf$PRUID)

# MAKE SOME FAKE DATA TO TEST IT ON

PRUID <- provinces.sf$PRUID
some_value <- rnorm(nrow(provinces.sf), mean = 1, sd = 0.3)
prov.test.data <- data.frame(PRUID, some_value)

# JOIN THEM TOGETHER NOW

prov.join <- left_join(prov.test.data, provinces.sf) %>% st_as_sf()



er.sf <- rmapshaper::ms_simplify(st_read("C:/file_path/Economic Regions", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326))

# MAKE THE ERUID NUMERIC SO WE CAN WORK WITH THEM

er.sf$ERUID <- as.numeric(er.sf$ERUID)

# MAKE SOME FAKE DATA TO TEST IT ON

ERUID <- er.sf$ERUID
er.some_value <- rnorm(nrow(er.sf), mean = 1, sd = 0.3)
er.test.data <- data.frame(ERUID, er.some_value)

# JOIN THEM TOGETHER NOW

er.join <- left_join(er.test.data, er.sf) %>% st_as_sf()



#### CENSUS DIVISIONS ####

cd.sf <- rmapshaper::ms_simplify(st_read("I:/ICP RESEARCH/Working Papers/WP 34_Clusters/Research/Weseem/Mapping/Shapefiles/Census Divisions", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326))

# MAKE THE CDUID NUMERIC SO WE CAN WORK WITH THEM

cd.sf$CDUID <- as.numeric(cd.sf$CDUID)

# MAKE SOME FAKE DATA TO TEST IT ON

CDUID <- cd.sf$CDUID
cd.some_value <- rnorm(nrow(cd.sf), mean = 1, sd = 0.3)
cd.test.data <- data.frame(CDUID, cd.some_value)

# JOIN THEM TOGETHER NOW

cd.join <- left_join(cd.test.data, cd.sf) %>% st_as_sf()


#### CENSUS SUBDIVISIONS ####

csd.sf <- rmapshaper::ms_simplify(st_read("C:/file_path/Census Subdivisions", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326))

# MAKE THE CSDUID NUMERIC SO WE CAN WORK WITH THEM
  
csd.sf$CSDUID <- as.numeric(csd.sf$CSDUID)

# MAKE SOME FAKE DATA TO TEST IT ON

CSDUID <- csd.sf$CSDUID
csd.some_value <- rnorm(nrow(csd.sf), mean = 1, sd = 0.3)
csd.test.data <- data.frame(CSDUID, csd.some_value)

# JOIN THEM TOGETHER NOW

csd.join <- left_join(csd.test.data, csd.sf) %>% st_as_sf()


# SET COLOURS

prov.pal <- colorNumeric(palette = "Blues", domain = prov.join$some_value)
er.pal <- colorNumeric(palette = "Blues", domain = er.join$er.some_value)
cd.pal <- colorNumeric(palette = "Blues", domain = cd.join$cd.some_value)
csd.pal <- colorNumeric(palette = "Blues", domain = csd.join$csd.some_value)

# CREATE BASIC LABELS FOR POPUPS

prov.label <- paste("<p>", prov.join$PRNAME, "</p>", 
                    "<p>", "Some value: ", round(prov.join$some_value, digits = 2), "</p>", sep = "")

er.label<- paste("<p>", er.join$ERNAME, "</p>", 
                 "<p>", "Some value: ", round(er.join$er.some_value, digits = 2), "</p>", sep = "")


cd.label <- paste("<p>", cd.join$CDNAME, "</p>", 
                    "<p>", "Some value: ", round(cd.join$cd.some_value, digits = 2), "</p>", sep = "")


csd.label <- paste("<p>", csd.join$CSDNAME, "</p>", 
                  "<p>", "Some value: ", round(csd.join$csd.some_value, digits = 2), "</p>", sep = "")


# MAKE THE LEAFLET BASE, ADDING IN EACH GEOGRAPHIC LEVEL AS A LAYER SELECTED BY THE USER INPUT WITH "ADDLAYERSCONTROL"

all.layers <- leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% setView(lng = -101.73, lat = 61, zoom = 4) %>% 
  addPolygons(data = prov.join, 
              fillColor = ~prov.pal(prov.join$some_value),  
              smoothFactor = .5, 
              fillOpacity = 0.8, 
              weight = 1.5,
              color = "black",
              highlightOptions = highlightOptions(
                color = "white", 
                weight = 2,
                bringToFront = TRUE),
              group = "Provinces",
              label = lapply(prov.label, HTML)) %>%
  addPolygons(data = er.join, 
              fillColor = ~er.pal(er.join$er.some_value), 
              smoothFactor = .5, 
              fillOpacity = 0.8, 
              weight = 1.5,
              color = "black",
              highlightOptions = highlightOptions(
                color = "white", 
                weight = 2,
                bringToFront = TRUE),
              group = "Economic Regions",
              label = lapply(er.label, HTML)) %>%
  addPolygons(data = cd.join, 
              fillColor = ~cd.pal(cd.join$cd.some_value), 
              smoothFactor = 0.5, 
              fillOpacity = 0.8, 
              weight = 1.5,
              color = "black",
              highlightOptions = highlightOptions(
                color = "white", 
                weight = 2,
                bringToFront = TRUE),
              group = "Census Divisions",
              label = lapply(cd.label, HTML)) %>% 
  addPolygons(data = csd.join, 
              fillColor = ~csd.pal(csd.join$csd.some_value), 
              smoothFactor = 0.5, 
              fillOpacity = 0.8, 
              weight = 1.5,
              color = "black",
              highlightOptions = highlightOptions(
                color = "white", 
                weight = 2,
                bringToFront = TRUE),
              group = "Census Subdivisions",
              label = lapply(csd.label, HTML)) %>%
  addLayersControl(baseGroups = c("Provinces", "Economic Regions", "Census Divisions", "Census Subdivisions"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(position = "bottomright", pal = prov.pal, 
    values = prov.join$some_value, title = "Some values", opacity = .6)


# SAVE IT AS AN INTERACTIVE WIDGET

saveWidget(widget = all.layers, file = "C:/your_directory/index.html")
    
