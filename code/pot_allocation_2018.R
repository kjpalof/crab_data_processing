# survey pot allocation.  Every 3 years review cpue and sd by strata to see if we should re-allocate effort.  
# 2018 - results here go into 'survey allocation table.xls'
## !!!! DISCLAIMER: you must have run CSA output for current year (2017) this code uses output from this analysis

# Katie Palof 
# katie.palof@alaska.gov

# load----------
library(tidyverse)
library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(weights)
library(broom)
library(grid)
library(gridExtra)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))


source('./code/functions.R')

# excursion -----
# data 
dat <- read.csv("./results/redcrab/Excursion/EI_perpot_all_17.csv") # output from CSA assessment summary by pot for all years 

# total crab cpue and sd by strata 
dat %>% 
  mutate(mature.male = Pre_Recruit + Recruit + Post_Recruit) %>% 
  mutate(total.c = (Missing + Juvenile + Large.Females + Post_Recruit + Pre_Recruit + Recruit + Small.Females)) -> dat
  
dat %>%
  filter(Year >= 2012) %>% 
  group_by(Year, Strata.Code) %>%
  summarise(total.c_u = mean(total.c), total.c_sd = sd(total.c), male.c_u = mean(mature.male), 
            male.c_sd = sd(mature.male)) -> ei_allocation

# 2012-2014
ei_allocation %>% 
  filter(Year <= 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))

#2015-2017
ei_allocation %>% 
  filter(Year > 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))

# pybus -----
# data 
dat <- read.csv("./results/redcrab/Pybus/PB_perpot_all_17.csv") # output from CSA assessment summary by pot for all years 

# total crab cpue and sd by strata 
dat %>% 
  mutate(mature.male = Pre_Recruit + Recruit + Post_Recruit) %>% 
  mutate(total.c = (Missing + Juvenile + Large.Females + Post_Recruit + Pre_Recruit + Recruit + Small.Females)) -> dat

dat %>%
  filter(Year >= 2012) %>% 
  group_by(Year, Strata.Code) %>%
  summarise(total.c_u = mean(total.c), total.c_sd = sd(total.c), male.c_u = mean(mature.male), 
            male.c_sd = sd(mature.male)) -> pb_allocation

# 2012-2014
pb_allocation %>% 
  filter(Year <= 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))

#2015-2017
pb_allocation %>% 
  filter(Year > 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))



# gambier ---
# data 
dat <- read.csv("./results/redcrab/Gambier/GB_perpot_all_17.csv") # output from CSA assessment summary by pot for all years 

# total crab cpue and sd by strata 
dat %>% 
  mutate(mature.male = Pre_Recruit + Recruit + Post_Recruit) %>% 
  mutate(total.c = (Missing + Juvenile + Large.Females + Post_Recruit + Pre_Recruit + Recruit + Small.Females)) -> dat

dat %>%
  filter(Year >= 2012) %>% 
  group_by(Year, Strata.Code) %>%
  summarise(total.c_u = mean(total.c), total.c_sd = sd(total.c), male.c_u = mean(mature.male), 
            male.c_sd = sd(mature.male)) -> gb_allocation

# 2012-2014
gb_allocation %>% 
  filter(Year <= 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))

#2015-2017
gb_allocation %>% 
  filter(Year > 2014) %>% 
  group_by(Strata.Code) %>% 
  summarise(mean = mean(total.c_u), sd = mean(total.c_sd), mature_u = mean(male.c_u), mature_sd = sd(male.c_sd))
