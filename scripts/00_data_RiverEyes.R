#### read me ####

# The purpose of this script is to explore my River Eyes data, including
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables

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

#### load and tidy River Eyes data  ####

Reyes <- read.csv("Data/DailyOccurrenceDryRm.csv", header = TRUE)
latlong_o <- read.csv("Data/WholeRiverMiles_LatLong.csv", header = TRUE)
Annual_dry_rm <- read.csv("Data/Persistence.txt")

# format date/time
# as.POSIXct is for date + time + time zone
# as.Date is for date only
#?strptime: Date-time Conversion Functions to and from Character

Reyes$Date_RE = as.POSIXct(Reyes$Date, format="%Y-%m-%d")

# convert characters that should be factors (categories) to factors
# and int that should be numbers (the year and month)
Reyes$Condition = as.factor(Reyes$Condition)
Reyes$RM = as.factor(Reyes$RM)
Reyes$Year = as.numeric(Reyes$Year)
Reyes$Month = as.numeric(Reyes$Month)

#are there any missing values?
sum(is.na(Reyes$Condition)) #no

##are there any duplicates?
# Load the data.table package
#library(data.table)
# Generate example data with duplicates
#Reyes_table <- data.table(Reyes)
# Check for duplicates
#dups <- Reyes_table[, .N, by = c("RM", "Date")][N > 1]$V1
# View the duplicated rows
#Reyes_table[RM %in% dups]
## there are no duplicates

# plot all RM by number of days dry 
options(scipen = 999)

ggplot(data = Reyes, aes(x = RM, y = Year, fill = Condition))+
  geom_col()+
  ylab("Number of days") + xlab("Year") + ggtitle("Drying at individual river miles between 2003 and 2018")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")


# can also look at single years in detail
ggplot(data=Reyes, aes(x=RM, y= Year, fill = Condition))+
  geom_col() +
  facet_wrap(~Year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

#### making continuous data and plotting it####

#format data for year and sum all the days the river mile was dry throughout the year 
#using occurrence data 
#Annual_dry_rm <- Reyes %>% 
#  mutate(Yr = lubridate::year(Date)) %>%
#  group_by(Year, RM) %>% 
# summarise(Sum_days_rm_dry = sum(Condition.b))

# convert characters that should be numeric
# and int that should be date (the year)
Annual_dry_rm$Sum_days_rm_dry = as.numeric(Annual_dry_rm$Sum_days_rm_dry)
#Annual_dry_rm$Year = as.Date(Annual_dry_rm$Year, format="%y") DOES THIS WORK WITH JUST ONE YEAT?

#merge the coordinates to my full data frame. I am doing this to perform 
#the spatial autocorrelation but had to do it here before starting to mess with 
#the data sets too much.
#First I ned to get rid of the dublicate river miles in the latlong data frame
# Remove rows with index 177, 156, 126, 111, 109, 78
latlong <- latlong_o[-c(177, 156, 126, 111, 109, 78), ]

#177 fid 176 rm 68
#156 fid 155 rm 87
#111 fid 110 rm 130
#126 fid 125 rm 116
#109 fid 108 rm 150
#78 fid 77 rm 161

#use the merge function, the binding columns need to have the same name,
#change the RM name column in the latlong data frame to match the RM name in the Reyes data frame
latlong$RM<- latlong$RMNum 
#we merge by RM to add latitude and longitude in the Reyes data set
Reyes <- merge(Reyes, latlong[,c("RM", "Latitude")], by = "RM", all.x = TRUE)
Reyes <- merge(Reyes, latlong[,c("RM", "Longitude")], by = "RM", all.x = TRUE)
#and in the Days dry per year data set
Annual_dry_rm <- merge(Annual_dry_rm, latlong[,c("RM", "Latitude")], by = "RM", all.x = TRUE)
Annual_dry_rm <- merge(Annual_dry_rm, latlong[,c("RM", "Longitude")], by = "RM", all.x = TRUE)

# Add leading zero to numbers from 54 to 99
#Annual_dry_rm$RM <- ifelse(as.integer(as.character(Annual_dry_rm$RM)) < 100,
                       # paste0("0", as.character(Annual_dry_rm$RM)),
                       # as.character(Annual_dry_rm$RM))

# plot all RM by number of days dry 
options(scipen = 999)

ggplot(data=Annual_dry_rm, aes(x=RM, y=Sum_days_rm_dry))+
  geom_col()+
  ylab("Number of days") + xlab("River Mile") + ggtitle("Total number of days dry per RM - 2003 and 2018")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(legend.position = "none")

# plot discharge by year 
ggplot(data=Annual_dry_rm, aes(x=RM, y=Sum_days_rm_dry))+
  geom_col()+
  facet_wrap(~Year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

##it looks like RM 68 and RM 160 have odd values making them outliers. 

#### check distributions ####
qqPlot(Annual_dry_rm$Sum_days_rm_dry); shapiro.test(Annual_dry_rm$Sum_days_rm_dry) 
# not normal.W = 0.49723, p-value < 0.00000000000000022
# For normal data, W should be close to 1, above 0.9 and
# p-value if it is really really low (usually bellow 0.05)) then you don't have normal data
# if it is high, then it is normal

##to cacluate spread out RM
#numbericRM <- as.numeric(Annual_dry_rm$RM)
#med=median(numbericRM) = 113
#quantile(numbericRM) = 25%=83, 75=140
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='54']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='54']) #not normal
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='70']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='70']) #not normal
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='75']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='75']) #normal
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='83']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='83']) #normal
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='95']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='95']) #normalish
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='113']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='113']) #not normal at all
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='140']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='140']) #not normal at all
qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='167']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='167']) #not normal at all

qqPlot(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='86']); shapiro.test(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='86'])

#distributions around RM83 are more normal than the rest!!!!

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data?
# if data is still non-normal, what distribution is it?
summary(Annual_dry_rm$Sum_days_rm_dry)
hist(Annual_dry_rm$Sum_days_rm_dry)
plot(density(Annual_dry_rm$Sum_days_rm_dry))
# my data is not normal!
# it is bounded above zero and is right-skewed
#If we look at them individually by river miels
#RM 54
summary(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='54'])
hist(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='54'])
plot(density(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='54']))
#RM 83
summary(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='83'])
hist(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='83'])
plot(density(Annual_dry_rm$Sum_days_rm_dry[Annual_dry_rm$RM=='83']))

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data?
# if data is still non-normal, what distribution is it?

range(Annual_dry_rm$Sum_days_rm_dry)
range(Annual_dry_rm$RM)
#### temporal autocorrelation ####
# I'm going to check these one site at a time. This is making the ts object for the analysis
dat_r = 
  Annual_dry_rm %>% 
  group_by(RM, Sum_days_rm_dry) %>% 
  arrange(Year)

dat_r <- dat_r %>%
  mutate(date = as.Date(paste0(Year, "-04-01")))
# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, yearly,
dat_yearly = 
  dat_r %>%
  mutate(yr = lubridate::year(date)) %>%
  mutate(mo = lubridate::month(date)) %>%
  dplyr::select(RM, yr, mo, Sum_days_rm_dry) %>%
  group_by(RM, yr, mo) %>%
  summarise(Value.mn = mean(Sum_days_rm_dry, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "1", sep="-")) %>%
  mutate(date = as.Date(date))


### subset data to be one site and one parameter
temp <- dat_yearly[dat_yearly$RM == 83 ,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "year"),
           fill = list(value = NA)) %>%
  as_tsibble(index = date)

## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occurred. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=1, start= c(year(temp_ts$date[1]), 1)) 
#This should create a yearly time series object with the same start year and end year as temp_ts, and a frequency of 1 observation per year.
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

# how many sites/River Miles are there?
length(unique(Reyes$RM))
# 114

# calculate cumulative number of days dry per site.
#for example: if you are looking at site 74 and it was dry on Monday, that would be 1. Dry again on Tuesday that is 2. Dry again on Wednesday = 3. But then wet on Thursday the value resets to 0. 
#Reyes <- Reyes %>% 
#  group_by(RM) %>% 
#  mutate(dry_days = ifelse(Condition == "Dry", cumsum(Condition == "Dry"), 0))

#dat_reyes = Reyes[Reyes$Date_RE >= as.Date("2013-09-02") &
#                    Reyes$Date_RE < as.Date("2013-09-03"),]


dat_yearly$RM <- sub("^0+", "", dat_yearly$RM)
dat_yearly <- merge(dat_yearly, latlong[,c("RM", "Latitude")], by = "RM", all.x = TRUE)
dat_yearly <- merge(dat_yearly, latlong[,c("RM", "Longitude")], by = "RM", all.x = TRUE)
### days for 200x
days2018 = dat_yearly[dat_yearly$date >= as.Date("2018-04-01") &
                        dat_yearly$date < as.Date("2018-10-31"),]

# Moran.I River Eyes data 
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(days2006$Longitude, days2006$Latitude)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(days2006$Value.mn, dists.inv)

## Mantel test winter
# generate spatial distance matrix
site_dists = dist(cbind(days2018$Longitude, days2018$Latitude))
# generate response distance matrix 
resp_dists = dist(days2018$Value.mn)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)
# if 'observation' is low (negative) there is no correlation between the distance matrices
# if p value if high, it suggests that they are NOT correlated

## Map
proj = CRS("+proj=longlat +datum=WGS84")
temp_spatial  <- SpatialPointsDataFrame(coords= cbind(days2003$Longitude, days2003$Latitude),
                                        data = as.data.frame(cbind(days2003$RM, days2003$Value.mn)),
                                        proj4string = proj)
plot(temp_spatial)
