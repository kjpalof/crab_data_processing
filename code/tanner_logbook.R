# K.Palof 10-6-17
# Code to review logbook data for Tanner crab fishery.
# needed to seperate out catch for Lynn Sisters and North Juneau, previously done in .JMP and Excel

# logbook data
rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(readxl)

#####Load Data ---------------------------------------------------
# change input file and input folder for each
logb <- read_excel(path = "./data/TannerLogbookData_2017.xlsx", sheet = 1)
glimpse(logb)


# need data and comments from stat area 115-10
logb %>% 
  filter(DISTRICT == 115 & SUB_DISTRICT == 10) %>% 
  select(YEAR, DISTRICT, SUB_DISTRICT, AREA_DESCRIPTION, NUMBER_POTS_LIFTED, 
         TARGET_SPECIES_RETAINED, COMMENTS) ->log11510
