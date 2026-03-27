#### READ ME####
#The purpose of this script is to gather the data from both USGS gage system 
# FOR THE SUMMER ONLY - APRIL 1st TO OCTOBER 31st FOR EVERY YEAR and explore it
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation

install.packages("tidyverse")
#### libraries ####
library(dataRetrieval) #USGS data pagacke
library(tidyverse)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping

# intro to package 
vignette("dataRetrieval", package = "dataRetrieval")

#### data info ####

## service is either uv = 15 min data or dv = day data
# gages of interest (add a cero at the begining of each one!)

# 8313000	RIO GRANDE AT OTOWI BRIDGE, NM
# 8313150	Rio Grande abv Buckman Diversion, nr White Rock,NM
# 8317400	RIO GRANDE BELOW COCHITI DAM, NM
# 8319000	RIO GRANDE AT SAN FELIPE, NM
# 8329918	RIO GRANDE AT ALAMEDA BRIDGE AT ALAMEDA, NM
# 8329928	RIO GRANDE NR ALAMEDA, NM
# 8330000	RIO GRANDE AT ALBUQUERQUE, NM
# 8330830	RIO GRANDE AT VALLE DE ORO, NM
# 8330875	RIO GRANDE AT ISLETA LAKES NR ISLETA, NM
# 8331160	RIO GRANDE NEAR BOSQUE FARMS, NM
# 8331510	RIO GRANDE AT STATE HWY 346 NEAR BOSQUE, NM
# 8332010	RIO GRANDE FLOODWAY NEAR BERNARDO, NM
# 8354900	RIO GRANDE FLOODWAY AT SAN ACACIA, NM

#### retrieving data USGS for the summer only####

#use paste function and for loop to get all the dates from only the summer
#from 2003 to 208, because of the River Eyes Data 

#start date
start.date1 = c(2003:2018)
start.date <- vector(mode="character", length=length(start.date1))
for (i in 1:length(start.date1)){
  start.date[i] = paste(start.date1[i], "04","01", sep="-")
}
start.date
start.date = as.Date(start.date)
#do I need it as a date?

#end date
end.date1 = c(2003:2018)
end.date <- vector("character", length(end.date1))
for (i in seq_along(end.date1)){
  end.date[i] = paste(end.date1[i], "10","31", sep="-")
}
end.date
end.date = as.Date(end.date)

# Initialize an empty data frame to hold the results
discharge <- data.frame()

# Loop through each date range and retrieve the corresponding data
for (i in seq_along(start.date)) {
  # Retrieve data for the current date range
  temp <- readNWISdv(siteNumbers = siteNumber,
                     parameterCd = pCode,
                     startDate = start.date[i],
                     endDate = end.date[i])
  
  # Append the data to the results data frame
  discharge <- rbind(discharge, temp)
}

# View the resulting data frame
discharge
)

#### check distributions####
#Otowi
temp = discharge[discharge$site_no == "08313000",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#White Rock
temp = discharge[discharge$site_no == "08313150",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Cochiti
temp = discharge[discharge$site_no == "08317400",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Albuquerque
temp = discharge[discharge$site_no == "08330000",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Bosque Farms
temp = discharge[discharge$site_no == "08331160",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Isleta
temp = discharge[discharge$site_no == "08330875",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#San Acacia
temp = discharge[discharge$site_no == "08354900",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal

#Otowi
temp = discharge[discharge$site_no == "08313000",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#White Rock
temp = discharge[discharge$site_no == "08313150",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#Cochiti
temp = discharge[discharge$site_no == "08317400",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#Albuquerque
temp = discharge[discharge$site_no == "08330000",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#San Acacia
temp = discharge[discharge$site_no == "08354900",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))

# my data is not normal!
# this looks like a lognormal, Gamma, or Weibull distribution
# it is bounded above zero and is right-skewed
# what happens if I log-transform it?

#otowi
temp = discharge[discharge$site_no == "08313000",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#white rock
temp = discharge[discharge$site_no == "08313150",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003))
#cochiti
temp = discharge[discharge$site_no == "08317400",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#Albuquerque
temp = discharge[discharge$site_no == "08330000",]
qqPlot(log10(temp$X_00060_00003[0:5000])); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#isleta
temp = discharge[discharge$site_no == "08330875",]
qqPlot(log10(temp$X_00060_00003[0:5000])); shapiro.test(log10(temp$X_00060_00003[0:5000]))

range(temp$X_00060_00003)
is.infinite(temp$X_00060_00003)

# a log10 transformation did the trick! That tells me that it is lognormal. I will note in my report that a log10 transformation is a possible option if my models don't meet assumptions.
# Also note the stair-steps in the data at lower values. This could result from detection limits where the low value was replaced with a standard value. It shouldn't be a huge problem, but it is worth noting as a thing to investigate if the analyses don't turn out well. 
#for now, it doesn't matter if I can't "name the distribution of my data". It will
# be important down the road when I'm dealing with the residuals in my model, but 
# for now it's ok.

#### temporal autocorrelation####
#### Otowi
### subset data to be one site and one parameter
temp = discharge[discharge$site_no == alist("08313000") & discharge$X_00060_00003,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(Date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(Date = seq(min(Date), max(Date), by = "1 day"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = Date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occurred. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$X_00060_00003, frequency=365, start=c(2003, 04, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 

### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't 
#include 0 (which is always 1) and shows month #s by default instead of decimal years. 
#Note the different options for dealing with NAs and how this changes the results 
#(see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)
#PACF only describes the direct relationship between an observation and its lag

#### San Acacia
### subset data to be one site and one parameter
temp = discharge[discharge$site_no == alist("08354900") & discharge$X_00060_00003,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(Date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(Date = seq(min(Date), max(Date), by = "1 day"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = Date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occurred. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$X_00060_00003, frequency=365, start=c(2018, 04, 01)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 

### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't 
#include 0 (which is always 1) and shows month #s by default instead of decimal years. 
#Note the different options for dealing with NAs and how this changes the results 
#(see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

#### spatial autocorrelation ####
# how many sites are there?
length(unique(discharge$site_no))
# 12
#obtain information available (i.e. lat long) for a particular USGS site (or sites)
siteInfo <- readNWISsite(c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                           "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                           "08354900"))
#merge the coordinates to my full data frame
discharge <- merge(discharge, siteInfo[,c("site_no", "lat_va")], by = "site_no", all.x = TRUE)
discharge <- merge(discharge, siteInfo[,c("site_no", "long_va")], by = "site_no", all.x = TRUE)

### winter day 2019
dat_winter = discharge[discharge$Date >= as.Date("2018-02-01") &
                             discharge$Date < as.Date("2018-02-02"),]
temp = dat_winter %>%  filter(X_00060_00003 %in% dat_winter)

## Moran.I winter 2019
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_winter$long_va, dat_winter$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_winter$X_00060_00003, dists.inv)

#### spring day 2019
dat_spring = discharge[discharge$Date >= as.Date("2018-04-01") &
                             discharge$Date < as.Date("2018-04-02"),]

## Moran.I spring 
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_spring$long_va, dat_spring$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_spring$X_00060_00003, dists.inv)

#### fall day 2019
dat_fall = discharge[discharge$Date >= as.Date("2019-10-01") &
                           discharge$Date < as.Date("2019-10-02"),]

## Moran.I fall
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_fall$long_va, dat_fall$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_fall$X_00060_00003, dists.inv)

#### Monsoon day 2019
dat_monsoon = discharge[discharge$Date >= as.Date("2019-06-01") &
                              discharge$Date < as.Date("2019-06-02"),]

## Moran.I Monsoon
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_monsoon$long_va, dat_monsoon$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_monsoon$X_00060_00003, dists.inv)

## Mantel test winter
# generate spatial distance matrix
site_dists = dist(cbind(dat_winter$long_va, dat_winter$lat_va))
# generate response distance matrix 
resp_dists = dist(dat_winter$X_00060_00003)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)


## Mantel test spring
# generate spatial distance matrix
site_dists = dist(cbind(dat_spring$long_va, dat_spring$lat_va))
# generate response distance matrix 
resp_dists = dist(dat_spring$X_00060_00003)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

## Mantel test fall
# generate spatial distance matrix
site_dists = dist(cbind(dat_fall$long_va, dat_fall$lat_va))
# generate response distance matrix 
resp_dists = dist(dat_fall$X_00060_00003)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

## Mantel test Monsoon
# generate spatial distance matrix
site_dists = dist(cbind(dat_monsoon$long_va, dat_monsoon$lat_va))
# generate response distance matrix 
resp_dists = dist(dat_monsoon$X_00060_00003)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)
# if 'observation' is low (negative) there is no correlation between the distance matrices
# if p value if high, it suggests that they are NOT correlated
# Based on this test, there is detectable in correlation each season 
# since p-values are small and observations are high.

## Map
proj = CRS("+proj=longlat +datum=WGS84")
temp_spatial  <- SpatialPointsDataFrame(coords= cbind(dat_monsoon$long_va, dat_monsoon$lat_va),
                                        data = as.data.frame(cbind(dat_monsoon$site_no, dat_monsoon$X_00060_00003)),
                                        proj4string = proj)
plot(temp_spatial)







