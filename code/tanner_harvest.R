#K.Palof 
# ADF&G 11-2-16 / 10-4-18
# data from OceanAK summarize for use in Tanner CSA's 
# have to modify the output from "detailed fish tickets" need to add "Number of Animals...sum" to this.

# commercial catch

# Load ---------------------------
library(tidyverse)
cur_yr = 2018

# Data ---------------------------------------------------
# change input file to most recent year's catch from OceanAK for each
harvest <- read.csv("./data/Tanner_Detailed Fish Tickets.csv")
glimpse(harvest)

unique(harvest$Stat.Area)
# need to create column that does what 'Survey area 3' does in Excel sheet
# refer to '2014-2015 fish tickets.xlsx'
harvest %>%
  mutate(survey.area = ifelse(Stat.Area ==11023, 'Gambier', ifelse(Stat.Area == 11423, 'Icy', 
                                ifelse(Stat.Area == 11470, 'Glacier Bay', 
                                   ifelse(Stat.Area == 11012, 'Thomas', ifelse(Stat.Area == 11150,'North Juneau', 
                                    ifelse(Stat.Area ==11021 | Stat.Area ==11022, 'Pybus', 
                                     ifelse(Stat.Area == 11480 |Stat.Area ==11425, 'Excursion', 
                                      ifelse(Stat.Area==11120 | Stat.Area ==11121, 'Holkham', 
                                       ifelse(Stat.Area==11140|Stat.Area==11141|Stat.Area==11142|Stat.Area==11143,
                                              'Stephens', 
                              ifelse(Stat.Area == 11351|Stat.Area == 11352|Stat.Area == 11353|
                                       Stat.Area == 11354|Stat.Area == 11355|Stat.Area == 11356|Stat.Area == 11357|
                                       Stat.Area == 11358, 'Peril', 
                                              ifelse(Stat.Area == 11101|Stat.Area == 11102|Stat.Area == 11103|Stat.Area == 11104|
                                                  Stat.Area == 11105|Stat.Area == 11106|Stat.Area == 11107|Stat.Area == 11108|
                                                    Stat.Area == 11109|Stat.Area == 11110|Stat.Area == 11111|Stat.Area == 11112|
                                                    Stat.Area == 11113|Stat.Area == 11114|Stat.Area == 11115|Stat.Area == 11116|
                                                    Stat.Area == 11117|Stat.Area == 11118, 'Seymour', 
                                          ifelse(Stat.Area == 11431|Stat.Area == 11432|Stat.Area == 11433|Stat.Area == 11434, 'PFred', 
                                                  ifelse(Stat.Area == 11510|Stat.Area == 11215, 'Lynn', 
                                              ifelse(Stat.Area == 10940|Stat.Area == 10941|Stat.Area == 10942|Stat.Area == 10943|Stat.Area ==10532,
                                                           'Camden', 'Other'))))))))))))))) -> harvest
# remove 11511 from Lynn Canal - make it part of 'other'
# by stat area, not needed for this analysis
harvest %>%
 group_by(Season, Stat.Area, survey.area) %>%
  summarise(permits = length(unique(CFEC)), 
                             numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> harvest2

write.csv(harvest2, paste0('./results/tanner/comm_catch_by_statarea', cur_yr,'.csv'))
#dat %>%
#  filter(Stat.Area == 11510, Season == 'Sep2015 - Aug16') %>%
#  select(Season, CFEC, Stat.Area, )

### by survey area --------------------------
harvest %>%
  group_by(Season, survey.area)%>%
  summarise(permits = length(unique(CFEC)), numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> comm.catch.sum

# lynn sister and north juneau need to be manually split up in area 115-10
write.csv(comm.catch.sum, paste0('./results/tanner/tanner_comm_catch', cur_yr,'.csv'))
### mid-catch date ------------------
harvest %>%
  #filter (Season == "Sep2017 - Aug18") %>% 
  group_by(survey.area, Date.of.Landing) %>%
  summarise(numbers = sum(Number.Of.Animals)) ->mid.catch

mid.catch %>% 
  group_by(survey.area) %>% 
  summarise(total = sum(numbers)) -> step1
mid.catch %>% 
  left_join(step1) %>% 
  mutate(ratio_catch = numbers/total) -> mid.catch2

write.csv(mid.catch2, paste0('./results/tanner/tanner_mid_catch_date', cur_yr, '.csv'))

### total annual harvest  ---------------------
comm.catch.sum %>%
  group_by(Season)%>%
  summarise(numbers = sum(numbers), pounds = sum(pounds)) -> annual_catch

write.csv(annual_catch, './results/tanner/tanner_annual_catch_17.csv')
