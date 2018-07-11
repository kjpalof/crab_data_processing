# K.Palof  ADF&G
# 7-11-18
# personal use summary for 11-A
# just for 2017/18 but need to evalute how it was done for previous years 

rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)

#####Load Data ---------------------------------------------------
personal_use <- read.csv("./data/redcrab/personal_use_RKC_11A_2017.csv")

## summarize data ----
personal_use %>% 
  group_by(Year, Permit.Returned.Status) %>% 
  summarise(number = sum(Number.Of.Crab, na.rm = TRUE), 
            pots = sum(Number.Of.Pots, na.rm = TRUE)) -> by_status

by_status %>% 
  summarise(sum(number))  
