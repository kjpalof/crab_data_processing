#k.palof
# code to recalculate weighted CPUE and SD for the juneau area by placing all pots older than 2005 into current (post-2005)
#   strata.  

# step 1: assign old pots strata
# step 2: summarise data as in JNUprocessingCODE.R
# step 3: combine file (from Kellii) with file pulled from OceanAK with all data summarised

### load packages ---------
library(tidyverse)
library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(weights)
library(broom)
#### load data ------------
jnu_lat_long <- read.csv("./data/redcrab/Juneau/RKC_survey_lat_long_all.csv")
area <- read.csv("./data/redcrab/Juneau/Juneau_Barlow_strata_area.csv")
dat <- read.csv("./data/redcrab/Juneau/RKC_survey_CSA_jnu.csv")

### create lat long file for Kellii -----
head(jnu_lat_long)

jnu_lat_long %>% 
  group_by(Year, Project.Code, Trip.No, Location.Code, Location, Pot.No, Latitude.Decimal.Degrees, Longitude.Decimal.Degrees) %>% 
  summarise(n = n()) -> jnu_lat_long_bypot
write.csv(jnu_lat_long_bypot, './results/redcrab/Juneau/jnu_lat_long_bypot.csv', row.names = FALSE)

#### crab data processing ------------