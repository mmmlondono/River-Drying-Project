library(ggmap) # making maps
library(rgdal) # changing projections
library(broom) # pulling out data from shape files
library(sp) # helpful for maps 
library(tidyverse) 
library(usmap) #plots us map
library(spdplyr) # manipulating spatialdataframes


# make sure to download ggmaps from github
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

# this is my key, I would suggest you create your own. 
register_google(key = "AIzaSyAEUCxlN7Nvje2a24PW065bblJsCNH3ekg") 

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





##### ggmaps ######
# create a base map for New Mexico
NM_basemap <- get_map(location=c(left = -111, bottom = 30, right = -101, top = 38), 
                      maptype = 'terrain', 
                      source = 'stamen')
ggmap(NM_basemap)

ggmap(get_googlemap("New Mexico", maptype = "roadmap", zoom = 7))


# Create a base map for ABQ
abq <-ggmap(get_googlemap("Albuquerque", zoom = 12, maptype = "satellite"))
abq 


#### Sensor example ####
# showing how to add points to a map # 

# read in sensor data with the coordinates
sensors <- read_csv("data/sensor_locations_est.csv")

# find the range of longitude and latitude
box <- 
  sensors %>%
  summarise(left = min(long), 
            right = max(long), 
            bottom = min(lat), 
            top = max(lat))

# i am going to a little bit so the points aren't on the edge of the screen
buffer <- .5

# create the base map using the coordinates and stamen
sensor_base <- get_map(location=c(left = box$left-buffer, 
                                  bottom = box$bottom-buffer, 
                                  right = box$right + buffer, 
                                  top = box$top + buffer), 
                       maptype = 'terrain', source = 'stamen')

# map it! the sensors are the red points
ggmap(sensor_base) + 
  geom_point(data = sensors, aes(x = long, y = lat), size = 1.5, color = "red")






# just for fun, I plotted where the WTA tennis events are scheduled or occurred in 2021 so far:
wta <- read_csv("data/WTA.csv") %>%
  mutate(city_county = paste0(City, ", ", Country)) %>% # need city and country or it will find the wrong ones
  mutate(map_df(city_county, geocode)) # find the coordinates for each city


# creates a world map
world <- ggmap(get_stamenmap(bbox = c(left = -150, bottom = -50, right = 160, top = 75), zoom = 3, maptype = "terrain-background"))

# maps on where the tennis tournaments are being played and level/surface!
world + geom_point(data = wta, 
                   aes(x = lon, y = lat, shape = Level, color = Surface), 
                   size = 3) + 
  scale_color_manual(values = c("Orange", "Green", "blue")) + 
  ggtitle("WTA Events")



#### morels example
### just trying to show how to add 
library(rinat)

# lets downlaod all the morel sightings from 2020
morels <- get_inat_obs(query = "Morchella", year = 2020, maxresults = 5000)

# can see what species are included
unique(morels$scientific_name)

# get map of US
usa <-
  get_googlemap(location='united states', zoom=3, maptype = "terrain")


ggmap(usa) + geom_point(data = morels, 
                        aes(x = longitude, y = latitude), 
                        size = 0.1, alpha = 0.4, color = "red")

# makes a density layer instead. Doesn't work too well for this example...
ggmap(usa)  + 
  geom_point(data = morels, aes(x = longitude, y = latitude, ), 
                         size = 0.1, alpha = 0.4, color = "black")+
  stat_density2d(data = morels, 
                          aes(x = longitude, y = latitude, fill = ..level..), 
                          size = 0.5, bins = 25, geom = "polygon", alpha = 0.2) + 
  scale_fill_gradient(low = "yellow",high= "red")


### bike paths example ####

#### Draw bike paths on 
bike_shape <- readOGR("data/biketrails", "biketrails")

# check the projection
proj4string(bike_shape)

# going to switch to this projection
newproj <- CRS("+proj=longlat +datum=WGS84")

# update projection
bike_transformed <- spTransform(bike_shape, newproj) 
bike_transformed

bike_transformed@data %>% View()

# using spdplyr to select specific shapes
bike_lane <- 
  bike_transformed %>%
  filter(PathType == "BikeLane - A portion of the street with a designated lane for bicycles.")

# plot the bike paths using  geom_path
abq + geom_path(data = bike_lane, 
          aes(x = long, y = lat, group = group),
          color = 'yellow', size = .5)



# lets add hiking trails!
hiking <- 
  bike_transformed %>%
  filter(PathType == "Hiking trail - An unpaved trail open to foot traffic only.")



# zoomed out abq
abq_zoomed_out <-ggmap(get_googlemap("Albuquerque", zoom = 10, maptype = "satellite"))


abq_zoomed_out + 
  geom_path(data = bike_lane, 
            aes(x = long, y = lat, group = group),
            color = 'yellow', size = .5) + 
  geom_path(data = hiking, 
            aes(x = long, y = lat, group = group),
            color = 'green', size = .5)  
  




### polygon examples ####

# groundwater basins polygon #
groundwater <- readOGR("/Users/michaelmann/Dropbox/Masters_Program/TA/Stakeholders/data/OSE_Declared_Groundwater_Basins-shp", "Declared_Goundwater_Basins")

# check the projection
proj4string(groundwater)

groundwater_transformed <- spTransform(groundwater, newproj) 

ggmap(NM_basemap) + 
  geom_polygon(data = groundwater_transformed, 
                                 aes(x = long, y = lat, group = id), 
                                 fill = "blue", color = "black", alpha = 0.2) + 
  geom_point(data = sensors, aes(x = long, y = lat), size = 1, color = "red")

### vegetation example ####

# vegetation polygon example!
veg_shape <- readOGR("/Users/michaelmann/Dropbox/Masters_Program/TA/Stakeholders/data/veg0001_shp/", "veg0001")

# projection looks good!
proj4string(veg_shape)


# susbset the data so its only deserts and grasslands
desert_grassland <- 
  veg_shape %>% 
  filter(str_detect(VEG_NAME, pattern = "DESERT") | str_detect(VEG_NAME, pattern = "GRASSLAND"))


### If you want to use the metadata along with the shape files you need to clean it up ###

# pulling rownames and setting it to id 
desert_grassland@data$id <- rownames(desert_grassland@data)

# extracting coordinates using tidy from broom
coordinates <- tidy(desert_grassland)

# merge the two and join by the id column. 
desert_grassland_clean <- left_join(coordinates, desert_grassland@data, by = "id")


# now you can color the fill by the vegation type!
ggmap(NM_basemap) + 
  geom_polygon(data = desert_grassland_clean,
               aes(x = long, y = lat, group = id, fill = VEG_NAME), 
              alpha = 0.8)  + scale_fill_brewer(palette = "Set2")
