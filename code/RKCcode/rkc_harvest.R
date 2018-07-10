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
harvest <- read.csv("./data/harvest_17.csv")
glimpse(harvest)
survey.area <- read.xlsx('data/redcrab/rkc_biomass_2017_model.xlsx', sheetName = "Sheet2") #stat area to survey area conversion

unique(harvest$Stat.Area)
# need to create column that does what 'Survey area 3' does in Excel sheet
# refer to '2014-2015 fish tickets.xlsx'


# by stat area, not needed for this analysis
harvest %>%
  group_by(Season, Stat.Area, survey.area) %>%
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> harvest2

write.csv(harvest2, './results/tanner/comm_catch_by_statarea.csv')
#dat %>%
#  filter(Stat.Area == 11510, Season == 'Sep2015 - Aug16') %>%
#  select(Season, CFEC, Stat.Area, )

### by survey area --------------------------
harvest %>%
  group_by(Season, survey.area)%>%
  summarise(permits = length(unique(CFEC)), numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> comm.catch.sum

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
