# K.Palof  ADF&G
# 7-10-18
# harvest data from fish tickets in OceanAK summarize for use in Southeast RKC CSA's 
# have to modify the output from "detailed fish tickets" need to add "Number of Animals...sum" to this.

# commercial catch
rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)

#####Load Data ---------------------------------------------------
# change input file and input folder for each
harvest <- read.csv("./data/redcrab/RKC_fish_tickets.csv")
glimpse(harvest)
survey.area <- read.xlsx('data/redcrab/rkc_biomass_2017_model.xlsx', sheetName = "Sheet2") #stat area to survey area conversion


### # by stat area ------
unique(harvest$Species.Code.and.Name)
harvest %>% 
  select(Species.Class, Season, CFEC, ADFG.Number, Date.of.Landing, Stat.Area, 
         Species.Code.and.Name, Number.Of.Animals, Whole.Weight..sum., Pot.Lifts) %>%  
  group_by(Season, Stat.Area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) %>% 
  rename(stat.area = Stat.Area) -> harvest2

# add survey area ---
harvest2 %>% 
  left_join(survey.area) -> harvest3
harvest3 %>% 
  group_by(Season, survey.area) %>% 
  summarise(permits = sum(permits), 
            numbers = sum(numbers), 
            pounds = sum(pounds), 
            pots = sum(pots)) -> catch_by_survey


write.csv(harvest3, './results/redcrab/comm_catch_by_statarea.csv')
write.csv(catch_by_survey, './results/redcrab/comm_catch_by_surveyarea.csv')


# lynn sister and north juneau need to be manually split up in area 115-10
write.csv(comm.catch.sum, './results/redcrab/tanner_comm_catch.csv')
### mid-catch date ------------------
harvest %>%
  filter (Season == "Sep2016 - Aug17") %>% 
  group_by(survey.area, Date.of.Landing) %>%
  summarise(numbers = sum(Number.Of.Animals)) ->mid.catch
write.csv(mid.catch, './results/redcrab/tanner_mid_catch_date.csv')

### total annual harvest  ---------------------
comm.catch.sum %>%
  group_by(Season)%>%
  summarise(numbers = sum(numbers), pounds = sum(pounds)) -> annual_catch

write.csv(annual_catch, './results/tanner/tanner_annual_catch_17.csv')
