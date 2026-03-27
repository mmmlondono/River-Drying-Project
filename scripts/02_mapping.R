####READ ME####
#The purpose of this script is to create a map showing the river gages along the Rio Grande

####libraries####
library(ggmap) # making maps
library(rgdal) # changing projections
library(broom) # pulling out data from shape files
library(sp) # helpful for maps 
library(tidyverse) 
library(spdplyr) # manipulating spatialdataframes
library(devtools)
# make sure to download ggmaps from github
#devtools::install_github("dkahle/ggmap", ref = "tidyup")

# this is my key
register_google(key = "IzaSyACAtHvk5OIB8hqMZVozXxx1yQIBxVbFqU") 
####this is just from the tutorial - don't run####
# just a test that everything is working
santa_fe <- geocode("Santa Fe, New Mexico") 
# printing this way so you can see the decimals
print.data.frame(santa_fe)

# Basic US maps
usmap::plot_usmap()
usmap::plot_usmap(include = c("NM", "AZ", "UT", "CO", "NV"))
usmap::plot_usmap(include = c("NM"))
# wont be correct until you transformedform the point
usmap::plot_usmap() + geom_point(data = santa_fe, aes(x= lon, y = lat), size = 5)

# converts map coordinates to Albers Equal Area projection
usmap_transform(santa_fe)

# now it works
usmap::plot_usmap() + 
  geom_point(data = usmap_transform(santa_fe), 
             aes(x= lon.1, y = lat.1), size = 2)

#####ggmaps######
# create a base map for New Mexico
NM_basemap <- get_map(location=c(left = -111, bottom = 30, right = -101, top = 38), 
                      maptype = 'terrain', 
                      source = 'stamen')
ggmap(NM_basemap)

ggmap(get_googlemap("New Mexico", maptype = "roadmap", zoom = 7))
####add points to a map####
#obtain information available for a particular USGS site (or sites)
siteInfo <- readNWISsite(c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                           "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                           "08354900"))
# find the range of longitude and latitude
box <- siteInfo %>%
  summarise(left = min(dec_long_va), 
            right = max(dec_long_va), 
            bottom = min(dec_lat_va), 
            top = max(dec_lat_va))
# i am going to a little bit so the points aren't on the edge of the screen
buffer <- .1
# create the base map using the coordinates and stamen
gauge_base <- get_map(location=c(left = box$left-buffer, 
                                  bottom = box$bottom-buffer, 
                                  right = box$right + buffer, 
                                  top = box$top + buffer), 
                            provider = "stamen", maptype = 'terrain', source = 'stamen', scale = 10, zoom = 10)

# map it! the sensors are the red points
ggmap(gauge_base) + 
  geom_point(data = siteInfo, aes(x = dec_long_va, y = dec_lat_va), size = 1.5, color = "red")
