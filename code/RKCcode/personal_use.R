# K.Palof  ADF&G
# 7-11-18, updated 7-8-19

# personal use summary for 11-A
# all years that have permit data available


#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)
cur_yr = 2019 # fsurvey year

prv_yr = 2018 # fishery year NOT survey year

#####Load Data ---------------------------------------------------
personal_use <- read.csv("./data/redcrab/personal_use_RKC_juneau_allyear_19.csv")

## reported number ----
# ** in order to get permits not returned that do NOT have catch need to click on "xyz" and select "include rows with only null values"
personal_use %>% # **fix** not all permits are being displaye in output - no permits not fished, also crab in permits not returend.
  filter(Year == cur_yr | Year == prv_yr) %>%  # remove this to do all years, currently just want current 18/19 season
  group_by(Area, Year, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number)), 
            number = sum(Number.of.Crab, na.rm = TRUE), 
            pots = sum(Number.of.Pots.or.Tows, na.rm = TRUE)) -> by_status

by_status %>% 
  filter(Year == cur_yr) %>% 
  mutate(status = ifelse(Permit.Returned.Status == "Permit entered online", 1, 
                         ifelse(Permit.Returned.Status == "Permit phoned in", 1, 
                                ifelse(Permit.Returned.Status == "Permit returned", 1, 
                                       ifelse(Permit.Returned.Status == "Permit returned - did not fish", 2, 
                                              0))))) %>% 
  mutate(cpue = number/pots, cpue_permits = number/n) -> by_status_current
write.csv(by_status_current, paste0('./results/redcrab/Juneau/personal_use_raw_summary_', cur_yr,'.csv'), row.names = FALSE)

by_status_2017 %>% 
  summarise(sum(number)) -> total_c
# 0 = permit not returned
# 1 = permit returned and fished
# 2 = permit returned but NOT fished

## estimated number ----
# pervious notes on personal use suggest that an equation was used to estimate harvest from those permits
#   that were not returned
#     
# percent not returned 
by_status_2017 %>% 
  group_by(Year, status) %>% 
  summarise(n = sum(n), 
            number = sum(number), 
            pots = sum(pots)) %>% 
  select(status, n) %>% 
  spread(status, n) %>% 
  mutate(pct.r.that.fished = (`1`) / (`1` + `2`), 
         pnr = (`0`) / (`1` + `2` +`0`), 
         total_permits = sum(`1` + `2` +`0`), 
         adjustment = (total_permits / (total_permits - 0.762*(`0`))), 
         est.total.catch.numbers = adjustment*as.numeric(total_c[2])) -> summary_17
write.csv(summary_17, './results/redcrab/Juneau/personal_use_estimate_total.csv', row.names = FALSE)


### ** fix ** to use current year's data 
## can use legal weight from 2017 to extrapolate this into pounds
## only works IF the male_weights is loaded from the processing code - if not need to bring it in from
###     results folder
summary_17 %>% 
  mutate(est.catch.lbs = est.total.catch.numbers*male_weights$legal_lbs[1]) -> summary_17

by_status_2017 %>% 
  group_by(Year, status) %>% 
  summarise(n = sum(n), 
            number = sum(number), 
            pots = sum(pots)) %>% 
  mutate(total_permits = sum(n), 
         total_returned = sum(n[status !=0]), 
         cpue.pots = number/pots, 
         cpue.permits = number/n) 



