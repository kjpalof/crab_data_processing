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

#### North Juneau ----------------------
###  need to keep barlow (location code 12, 1) and juneau (location code 13, 23) seperate
##### Historic file ---------------------------------------
###
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match
histdat <- read.csv("./data/nj_stp/NJ ONLY!_RKC_tannerdata_09-15.csv")
glimpse(histdat) # make sure the column names here match those in dat.NJ
data.NJ.all <- rbind(histdat, dat.NJ)
write.csv(data.NJ.all, './results/nj_stp/NJ_rawdata_all.csv')

### data manipulations ----------------------
# easier area since there are NO strata
data.NJ.all %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
                  !is.na(Width.Millimeters), 'Juvenile', 
                   ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
                    !is.na(Width.Millimeters),'Pre_Recruit', 
                     ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                      !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                       ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                        Width.Millimeters >169|(Shell.Condition.Code >3 & Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                         ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                          ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                           ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) %>%
  mutate(sub_area = ifelse(Location.Code == 12 | Location.Code == 1, 'Barlow', 'Juneau')) -> Tdat1

###
##### By Pot ----------------------------------------------------
#### Keep sub_area in the data frame!!!!!!!!!!!
#Now summarize by pot - only one area - NJ
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, sub_area, Pot.No, mod_recruit) %>% # use area here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + sub_area + Pot.No ~ mod_recruit, sum, drop=TRUE)
write.csv(dat3, './results/nj_stp/dat3.csv')
#head(dat3)# check to make sure things worked.

# No weighting by strata here for RKCS data due to it being designed for RKC.
####
##### CPUE historic -----------------------------------
####
#This version is ready to calculate CPUE for each recruit class
#Calculates a  mean CPUE and SE for each recruit class # not weighted due to lack of tanner specific strata on red crab survey
dat3 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_ALL
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_ALL, './results/nj_stp/NJ_CPUE_ALL.csv')

###
##### Short term trends -------------------------------------
###
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

head(dat3)
dat3 %>%
  filter(Year >=2013) -> dat3 # confirm that is only contains the last 4 years.  This year needs to be changed every year

dat3_long <- gather(dat3, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

dat3_long %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(mod_recruit) %>%
  do(fit = lm(crab ~ Year, data =.)) -> short_term

short_term %>%
  tidy(fit) -> short_term_slope

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(mod_recruit, r.squared, p.value)->short_term_out2
short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, './results/nj_stp/NJ_shortterm.csv')

dat3_long %>%
  filter(mod_recruit %in% recruit_used) ->st_dat3_long
ggplot(st_dat3_long, aes(Year, crab, color = mod_recruit))+geom_point() 
###

