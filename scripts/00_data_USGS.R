#### READ ME####
#The purpose of this script is to gather the data from both USGS gage system 
#and the River Eyes dataset and explore both of them by.
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables

install.packages("tidyverse")
#### packages ####
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

#### retrieving data USGS ####

# retrieve data for one site
otowi <- readNWISdata(siteNumbers = "08313000",
                      parameterCd = "00060",
                      startDate = "2002-04-01",
                      endDate = "2022-04-01",
                      service="dv")

#retrieve data for all dates on all 8 gages
siteNumber <- c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                "08354900")
#this code retrieves the discharge data
pCode <- "00060"
#start and end date from 2002 to 2022
sdate <- "2002-04-01"
edate <- "2022-10-31"

#get all the data from 2002 to 2022
fulldischarge <- readNWISdv(siteNumbers = siteNumber,
                            parameterCd = pCode,
                            startDate = sdate,
                            endDate = edate,
                            )

# add year and day of year for plotting
fulldischarge$year = lubridate::year(fulldischarge$Date)
fulldischarge$day = lubridate::yday(fulldischarge$Date)

#are there any missing values?
sum(is.na(fulldischarge$X_00060_00003)) #no

#separate data set by sites for plotting for Otowi
dis_otowi <- 
  fulldischarge %>% 
  filter(site_no=="08319000") %>%
  arrange(Date)

# add year and day of year for plotting Just did that for whole dataset
dis_otowi$year = lubridate::year(dis_otowi$Date)
dis_otowi$day = lubridate::yday(dis_otowi$Date)

# plot discharge by year 
ggplot(data=dis_otowi, aes(x=day, y=X_00060_00003))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# can also look at single years in detail
ggplot(data=dis_otowi, aes(x=Date, y=X_00060_00003))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2019-04-01"), as.Date("2019-10-31")))+
  theme(legend.title = element_blank()) +
  theme_bw()

#separate data set by sites for plotting for San Acacia
dis_sanacacia <- 
  fulldischarge %>% 
  filter(site_no=="08354900") %>%
  arrange(Date)

# add year and day of year for plotting Just did that for whole dataset
dis_sanacacia$year = lubridate::year(dis_sanacacia$Date)
dis_sanacacia$day = lubridate::yday(dis_sanacacia$Date)

# plot discharge by year 
ggplot(data=dis_sanacacia, aes(x=day, y=X_00060_00003))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# can also look at single years in detail
ggplot(data=dis_sanacacia, aes(x=Date, y=X_00060_00003))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2019-04-01"), as.Date("2019-10-31")))+
  theme(legend.title = element_blank()) +
  theme_bw()

#### check distributions####
#Otowi
temp = fulldischarge[fulldischarge$site_no == "08313000",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#White Rock
temp = fulldischarge[fulldischarge$site_no == "08313150",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Cochiti
temp = fulldischarge[fulldischarge$site_no == "08317400",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#San Acacia
temp = fulldischarge[fulldischarge$site_no == "08354900",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal

#Otowi
temp = fulldischarge[fulldischarge$site_no == "08313000",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#White Rock
temp = fulldischarge[fulldischarge$site_no == "08313150",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#Cochiti
temp = fulldischarge[fulldischarge$site_no == "08317400",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#San Acacia
temp = fulldischarge[fulldischarge$site_no == "08354900",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))

# my data is not normal!
# this looks like a lognormal, Gamma, or Weibull distribution
# it is bounded above zero and is right-skewed
# what happens if I log-transform it?

#otowi
temp = fulldischarge[fulldischarge$site_no == "08313000",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#white rock
temp = fulldischarge[fulldischarge$site_no == "08313150",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003))
#cochiti
temp = fulldischarge[fulldischarge$site_no == "08317400",]
qqPlot(log10(temp$X_00060_00003)); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#san acacia
temp = fulldischarge[fulldischarge$site_no == "08354900",]
qqPlot(log10(temp$X_00060_00003[0:5000])); shapiro.test(log10(temp$X_00060_00003[0:5000]))
#san acacia is not working!!!!!!!!!!!!!!!

range(temp$X_00060_00003)
is.infinite(temp$X_00060_00003)

# a log10 transformation did the trick! That tells me that it is lognormal. I will note in my report that a log10 transformation is a possible option if my models don't meet assumptions.
# Also note the stair-steps in the data at lower values. This could result from detection limits where the low value was replaced with a standard value. It shouldn't be a huge problem, but it is worth noting as a thing to investigate if the analyses don't turn out well. 



#### temporal autocorrelation####
#### Otowi
### subset data to be one site and one parameter
temp = fulldischarge[fulldischarge$site_no == alist("08313000") & fulldischarge$X_00060_00003,]
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
temp_ts = ts(temp_ts$X_00060_00003, frequency=365, start=c(2002, 04, 01)) 
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
temp = fulldischarge[fulldischarge$site_no == alist("08354900") & fulldischarge$X_00060_00003,]
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
temp_ts = ts(temp_ts$X_00060_00003, frequency=365, start=c(2019, 04, 01)) 
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
length(unique(fulldischarge$site_no))
# 12
#obtain information available for a particular USGS site (or sites)
siteInfo <- readNWISsite(c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                 "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                 "08354900"))
#merge the coordinates to my full data frame
fulldischarge <- merge(fulldischarge, siteInfo[,c("site_no", "lat_va")], by = "site_no", all.x = TRUE)
fulldischarge <- merge(fulldischarge, siteInfo[,c("site_no", "long_va")], by = "site_no", all.x = TRUE)

### winter day 2019
dat_winter = fulldischarge[fulldischarge$Date >= as.Date("2019-02-01") &
                           fulldischarge$Date < as.Date("2019-02-02"),]
temp = dat_winter %>%  filter(X_00060_00003 %in% dat_winter)

## Moran.I winter 2019
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_winter$long_va, dat_winter$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_winter$X_00060_00003, dists.inv)

#### spring day 2019
dat_spring = fulldischarge[fulldischarge$Date >= as.Date("2019-04-01") &
                             fulldischarge$Date < as.Date("2019-04-02"),]

## Moran.I spring 
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_spring$long_va, dat_spring$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_spring$X_00060_00003, dists.inv)

#### fall day 2019
dat_fall = fulldischarge[fulldischarge$Date >= as.Date("2019-10-01") &
                             fulldischarge$Date < as.Date("2019-10-02"),]

## Moran.I fall
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(dat_fall$long_va, dat_fall$lat_va)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(dat_fall$X_00060_00003, dists.inv)

#### Monsoon day 2019
dat_monsoon = fulldischarge[fulldischarge$Date >= as.Date("2019-06-01") &
                           fulldischarge$Date < as.Date("2019-06-02"),]

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







