library(tidyverse)
library(ggmap)
library(sf)
library(mapview)
library(leaflet)
library(zipcode)

dat <- read_csv("C:/Users/Cloe Wynn/Documents/BYU/Senior/Winter 2019/Stat 421/Dist_Carrier_Data.csv",
                col_types = list(DestinationZip = col_character(),
                                 OriginZip = col_character()))
set.seed(18)
samp <- dat[sample(nrow(dat), 5000), ]
locations <- samp[, c(10,11,13:19)]
origins_sf <- st_as_sf(locations, coords = c("OriginLong", "OriginLat"), crs = 4326)
destinations_sf <- st_as_sf(locations, coords = c("DestinationLong", "DestinationLat"), crs = 4326)

#mapview(origins_sf)
#mapview(destinations_sf)

row1 <- samp[1, c(10,11,13:19)]

#row1_origin <- st_as_sf(row1, coords = c("OriginLong", "OriginLat"), crs = 4326)
#row1_dest <- st_as_sf(row1, coords = c("DestinationLong", "DestinationLat"), crs = 4326)


pathlist <- list(samp$Actions[1])
zipCodes <- NULL
i <- 1
endlist <- FALSE
while (!endlist) {
  zipCodes[i] <- gsub(".*\\\"(\\d{5})\\\"(.*)","\\1", pathlist[[i]])
  pathlist[[i + 1]] <- gsub(".*(\\\"\\d{5}\\\")(.*)","\\2", pathlist[[i]])
  endlist <- ifelse((grepl(".*(\\\"\\d{5}\\\")(.*)", pathlist[[i + 1]])), 
         FALSE, TRUE)
  i <- i + 1
}


data(zipcode)
path <- zipcode[which(zipcode$zip == zipCodes), ]
row1 <- cbind(row1, path)

Long <- as.vector(as.matrix(row1[,c("OriginLong", "longitude", "DestinationLong")]))
Lat <- as.vector(as.matrix(row1[,c("OriginLat", "latitude", "DestinationLat")]))
row1 <- as.data.frame(cbind(row1, Long, Lat))   

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = row1$Long, lat = row1$Lat)
m <- addPolylines(m, lng = row1$Long, lat = row1$Lat)
m

