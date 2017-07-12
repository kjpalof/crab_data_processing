#k.palof
# Goal: prep pot lat / long data for Kellii so she can assign pre-2005 data
#     a 2005 strata location. 

# step 1: prep data to assign old pots strata here 
# step 2: use file from Kellii to do what's in "jnu_retro_weighted_CPUE.R" for all other areas.

### load packages ---------
library(tidyverse)

#### load data ------------
rm("lat_long", "lat_long_bypot")

lat_long <- read.csv("./data/redcrab/Excursion/RKC survey_historicpots_ei.csv")
lat_long <- read.csv("./data/redcrab/LynnSisters/RKC survey_historicpots_LS.csv")
lat_long <- read.csv("./data/redcrab/Peril/RKC survey_historicpots_ps.csv")
lat_long <- read.csv("./data/redcrab/Gambier/RKC survey_historicpots_GB.csv")
lat_long <- read.csv("./data/redcrab/Seymour/RKC survey_historicpots_SC.csv")
lat_long <- read.csv("./data/redcrab/Pybus/RKC survey_historicpots_PB.csv")

### create lat long file for Kellii -----
# this only had to be done once, does not need to be executed again.
# need to be done for each area
head(lat_long) # confirm correct area

# need a lat and long for each pot from start to 2004, exclude 2005 on since they already have them.

lat_long %>% 
  filter(Year <= 2004) %>% 
  group_by(Year, Project.Code, Trip.No, Location.Code, Location, Pot.No, Latitude.Decimal.Degrees, Longitude.Decimal.Degrees) %>% 
  summarise(n = n()) -> lat_long_bypot


write.csv(lat_long_bypot, './data/redcrab/Pybus/PB_lat_long_bypot.csv', row.names = FALSE)

