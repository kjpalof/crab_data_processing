# K.Palof 10-16-18 updated 
# Code to review logbook data for Tanner crab fishery.
# needed to seperate out catch for Lynn Sisters and North Juneau, previously done in .JMP and Excel

# logbook data from ALEX (as of 10-16-18) pull only data from district 115 - have to sort here for sub-district 10
#    since that's all that is needed. Pull for all years and save file in results/tanner 


#####Load Packages ---------------------------------
library(tidyverse)
library(readxl)

#####Load Data -------------------------------------
# change input file and input folder for each
logb <- read_excel(path = "./data/logbook_tanner_statarea115_97to2018.xlsx", sheet = "AlexData", 
                   skip = 13)
glimpse(logb)

# need data and comments from stat area 115-10
logb %>% 
  filter(DISTRICT == 115 & SUB_DISTRICT == 10) %>% 
  select(YEAR, DISTRICT, SUB_DISTRICT, AREA_DESCRIPTION, NUMBER_POTS_LIFTED, 
         TARGET_SPECIES_RETAINED, COMMENTS) ->log11510

# sort into LS or NJ -----
#unique(log11510$AREA_DESCRIPTION)
log11510 %>% 
  mutate(survey.area = case_when(grepl("james", AREA_DESCRIPTION, ignore.case = TRUE) ~ "Lynn Sisters",
                                 grepl("lynn", AREA_DESCRIPTION, ignore.case = TRUE) ~ "Lynn Sisters",
                                 grepl("berner", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("ben", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("eagle", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("island", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("end", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("sher", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("er", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("sher", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("mary", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("stone", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("pt", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("boat", AREA_DESCRIPTION, ignore.case = TRUE) ~ "North Juneau")) -> log11510



