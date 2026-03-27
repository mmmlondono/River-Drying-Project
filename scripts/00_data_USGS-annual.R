####READ ME####
#The purpose of this script is to gather the data from the USGS gage system 
# FOR THE SUMMER ONLY - APRIL 1st TO OCTOBER 31st FOR EVERY YEAR from 2003 - 2018 
# and sum it!!!

####libraries####
library(dataRetrieval) #USGS data pagacke
library(tidyverse)
library(lubridate)
library(DHARMa) #testing glm assumptions
library(lme4)


####data info ####

## service is either uv = 15 min data or dv = day data
#default setting for day data is mean.
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

####retrieving data USGS for the summer only####

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

#retrieve data for all dates on all 8 gages
siteNumber <- c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                "08354900")
#this code retrieves the discharge data
pCode <- "00060"

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


####summing discharge data by year####
# add year column from date 
discharge$year = lubridate::year(discharge$Date)

#add by year and by site number
discharge_sum <- discharge %>%
  group_by(site_no, year) %>%
  summarise(discharge_sum = sum(X_00060_00003))

####merging USGS sum discharge and River Eyes data frames####
Annual_dry_rm$year <- Annual_dry_rm$Year
merged_data <- merge(discharge_sum, Annual_dry_rm, by = "year")
merged_data$RM <- as.character(merged_data$RM)


# plot sum discharge by gauge 
ggplot(data=discharge_sum, aes(x=year, y=discharge_sum))+
  geom_point() + geom_path()+
  facet_wrap(~site_no, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# plot all gauges together
ggplot(data=discharge_sum, aes(x=year, y=discharge_sum, color=site_no))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw()

####Linear models####

# Filter data for a specific site
site_no <- "08313000" #Otowi gauge
RM <- "248" #RM for Otowi 257???
site_year_data <- discharge_sum %>%
  filter(site_no == site_no, RM == RM)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"
site_year_data <- discharge_sum %>%
  filter(site_no == site_no, RM == RM)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

####GLM####
# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"
site_year_data <- merged_data %>%
  filter(site_no == site_no, RM == RM)
# build model
glm_full <- glm(Sum_days_rm_dry ~ discharge_sum, 
                family = "poisson", 
                data = site_year_data)

#satisfies the assumptions
simulateResiduals(glm_full, plot = T)

#run anova
car::Anova(glm_full, type = "III")

# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"
site_year_data <- merged_data %>%
  filter(site_no == site_no, RM == RM)
  
tmod_lme4_L <- glmer(Sum_days_rm_dry ~ factor(discharge_sum)  + (1|site_no/year),
                     family="poisson",data= site_year_data)


tmod_lme4_bad <- lmer(TICKS ~ factor(YEAR) + cHEIGHT  + (1|Site/BROOD),
                      data= tickdata)

qqnorm(resid(tmod_lme4_L))
qqline(resid(tmod_lme4_L))

simulateResiduals(tmod_lme4_L, plot = T)

plot(resid(tmod_lme4_L))

MuMIn::AICc(tmod_lme4_L, tmod_lme4_bad)

car::Anova(tmod_lme4_L, type = "III")



