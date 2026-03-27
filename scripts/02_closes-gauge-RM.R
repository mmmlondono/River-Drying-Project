####READ ME####
#The purpose of this script is to calculate the closest river mile of each gauge

####libraries####
library(geosphere)
library(dplyr)
####Loading data and fixing data####
siteInfo <- readNWISsite(c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                           "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                           "08354900"))
latlong_o <- read.csv("Data/WholeRiverMiles_LatLong.csv", header = TRUE)

####Calculating distance matrix####
#chaging column names for easier handling
siteInfo$longitude <- siteInfo$dec_long_va
siteInfo$latitude <- siteInfo$dec_lat_va
latlong_o$longitude <- latlong_o$Longitude
latlong_o$latitude <- latlong_o$Latitude
#Create a matrix of coordinates for each data frame:
coords1 <- siteInfo[, c("longitude", "latitude")]
coords2 <- latlong_o[, c("longitude", "latitude")]
#make sure coords are as numeric
coords1$longitude<- as.numeric(coords1$longitude)
coords1$latitude <- as.numeric(coords1$latitude)
# calculate distance matrix between df1 and df2
dist_mat <- distm(coords1[,c("longitude", "latitude")],
                  coords2[,c("longitude", "latitude")],
                  fun = distHaversine)
# find closest point in df2 for each point in df1
closest <- apply(dist_mat, 1, which.min)
# add the closest point to df1
coords1$closest <- closest
####merging gauge number and RM to coords1 df####
coords1 <- merge(coords1, siteInfo[,c("site_no", "latitude")], by = "latitude", all.x = TRUE)
#coords1 <- merge(coords1, latlong_o[,c("RMNum", "closest")], by = "FID", all.x = TRUE)
