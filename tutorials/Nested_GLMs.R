library(palmerpenguins)
library(tidyverse)
#install.packages("DHARMa") # testing glm assumptions
library(DHARMa)
library(lme4)


#### snow example nested structure ######

# cleaning up data. Same as before except I am keeping the county. 
snow_sites <- 
  read_csv("data/snowdepthsites.csv") %>%
  separate(site_name, into = c("Site", "Station"), sep = " \\(") %>% #clean site and statino so they match the dataset
  mutate(Station = str_replace(Station, pattern = "\\)", "")) %>%
  rename(elevation = `elev (ft)`) %>%  # clean up elevation column
  select(Site, Station, county, elevation) %>%
  mutate(Site = str_replace(Site, "PanchueLa", "Panchuela"))



snowpack <- 
  read_csv("data/historicmonthlysnowdepth.csv") %>%
  rename(Year = `Water Year`) %>%
  select(Site, Station, Year, Feb_2) %>%
  drop_na()

snowpack_clean <- 
  snowpack %>%
  left_join(snow_sites, by = c("Site", "Station")) %>%
  mutate(Feb_2 = as.numeric(Feb_2))

# make a vector of the sites with missing matching stations
missing_sites <- 
  snowpack_clean %>%
  filter(is.na(elevation)) %>%
  select(Site) %>%
  unique() %>% 
  pull()

# extract elevation for missing sites. average if multiple stations are present at that site
extra_sites <- 
  snow_sites %>%
  filter(Site %in% missing_sites) %>%
  group_by(Site) %>% 
  summarise(elevation = mean(elevation))

# join it again. This will make elevation now be elevation.x and elevation.y. This ok because there is a function
# called coalesce that will merge these two so the NAs are replaced with real values.
snowpack_clean_elevations <- 
  snowpack_clean %>%
  left_join(extra_sites, by = "Site")


# rewrite elevation with the newly merged elevation data
snowpack_clean <- 
  snowpack_clean %>%
  mutate(elevation = coalesce(snowpack_clean_elevations$elevation.x, snowpack_clean_elevations$elevation.y)) %>%
  drop_na()


# fully nested model
snow_model_nested <- lmer(sqrt(Feb_2) ~ Year+scale(elevation) + (1|county/Site), 
                          data = snowpack_clean, REML = FALSE)

# just using site as a random effect
snow_model <- lmer(sqrt(Feb_2) ~ (Year)+scale(elevation) + (1|Site), 
                   data = snowpack_clean, REML = FALSE)

# just using county as a random effect
snow_model_2 <- lmer(sqrt(Feb_2) ~ (Year)+scale(elevation) + (1|county), 
                     data = snowpack_clean, REML = FALSE)



plot(snow_model_nested)
# not perfect but definitely an improvement
qqnorm(resid(snow_model))
qqline(resid(snow_model))

car::Anova(snow_model_nested, type = "III")


MuMIn::AICc(snow_model_nested, snow_model, snow_model_2)



#####





##### GLM example using binomial clutch data. ######

# clean up data
penguin_clutch <- 
  penguins_raw %>%
  select(Species, 'Body Mass (g)', 'Clutch Completion') %>%
  rename(mass =  'Body Mass (g)', 
         clutch_completion = 'Clutch Completion') %>%
  separate(Species, into = "Species", sep = " ", extra = "drop") %>%
  drop_na() %>%
  mutate(clutch_completion = case_when(clutch_completion == "Yes" ~ "1", 
                                       clutch_completion == "No" ~ "0" )) %>%
  mutate(clutch_completion = as.numeric(clutch_completion))



# table summary
penguin_clutch %>%
  group_by(Species, clutch_completion) %>%
  count() %>%
  pivot_wider(names_from = clutch_completion, values_from = n) %>%
  rename(Success = '1', Failed = '0') %>%
  mutate(percent = 100 * Success / sum(Success + Failed))

# plot of raw data
penguin_clutch %>%
  ggplot(aes(x = mass, y = clutch_completion)) + geom_jitter(height = 0.05) + facet_wrap(~Species)



# build model
glm_full <- glm(clutch_completion ~ Species + mass, 
                family = "binomial", 
                data = penguin_clutch)

#satisfies the assumptions
simulateResiduals(glm_full, plot = T)

#run anova
car::Anova(glm_full, type = "III")


#### 2 tick data #####

tickdata <- read.table("data/Elston2001_tickdata.txt",header=TRUE,
                      colClasses=c("factor","numeric","factor","numeric","factor","factor"))

tickdata %>% 
  ggplot(aes(x = HEIGHT,y = TICKS, colour = YEAR))+
  stat_sum(aes(size=..n..),alpha=0.7)+
  scale_y_log10()+
  scale_size_continuous(breaks=c(2,6,10),range=c(2,7))+
  geom_smooth(method="glm",method.args=list(family=quasipoisson))



#centering
tickdata <- transform(tickdata,cHEIGHT= HEIGHT-mean(HEIGHT))


tmod_lme4_L <- glmer(TICKS ~ factor(YEAR) + cHEIGHT  + (1|LOCATION/BROOD),
                     family="poisson",data= tickdata)


tmod_lme4_bad <- lmer(TICKS ~ factor(YEAR) + cHEIGHT  + (1|LOCATION/BROOD),
                    data= tickdata)
      
qqnorm(resid(tmod_lme4_L))
qqline(resid(tmod_lme4_L))

simulateResiduals(tmod_lme4_L, plot = T)

plot(resid(tmod_lme4_L))

MuMIn::AICc(tmod_lme4_L, tmod_lme4_bad)

car::Anova(tmod_lme4_L, type = "III")



