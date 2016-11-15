#K.Palof 
# ADF&G 11-15-16
# Areas: tanner crab assessment of red crab areas : North Juneau and Stephens Passage
# done seperately because they need to be divided into these two areas based on the pot locations
# process 2016 data first and then add to older data already stored. 

# code to process data from Ocean AK to use in crab CSA models.  
#Currently this is done in excel then JMP, prior to 2016  

rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(tidyverse)
library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(weights)
library(broom)

#####Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/nj_stp/rkcs_tanner_nj_stp_oceanAKraw.csv")
# this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/nj_stp/stp_strata_area.csv") 
seperate <- read.csv("./data/nj_stp/Pots_SP2016_Katie.csv") 
# avoid this by bringing in last four year in original OceanAK data pull.  
#histdat <- read.csv("./data/nj_stp/2Juneau Stratified CPUE 2015_area formula.csv")
## !!!!  In future years this file will be 'JNU_CPUE_ALL' and just get updated with current years data.
#females <- read.csv("./data/Juneau/RKC_11_16_large females_by_pot.csv")
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data ---------------------------------

# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) -> test1 # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - tanner soak time should be between 16-20??? double check this

##### seperate NJ and Juneau (also known as SP) ---------------------
dat %>%
  mutate(area = ifelse(Location == "Barlow Cove", "NJ", 
                       ifelse(Location == "Juneau" & Pot.No %in% seperate$PotNo, "Juneau", "NJ"))) ->dat
#seperating the areas since North Juneau does not have density strata - since it's a red crab area
# and Juneau does since it's based on the Tanner Stephens Passage strata.
dat %>% 
  filter(area == "NJ") ->dat.NJ
dat %>% 
  filter(area == "Juneau") -> dat.SP


