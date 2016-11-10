#K.Palof 
# ADF&G 11-4-16
# Areas: Tanner crab survey areas - includes Holkham, Thomas, Glacier Bay and Icy Strait
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
dat <- read.csv("./data/TCS/tanner_survey_13_16.csv")
# this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/TCS/TCSstrata_area.csv") 
# avoid this by bringing in last four year in original OceanAK data pull.  
#histdat <- read.csv("./data/Juneau/2Juneau Stratified CPUE 2015_area formula.csv")
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

##### Tanner specific manipulations -----------------------------
###Survey areas ONLY 
# confirm that only the four surveys areas are present.
levels(dat1$Location) # 2015 presence of one port camden pot.  remove this.

dat1 %>%
  filter(Location != "Port Camden") -> dat1

### add columns used later 
dat1 %>%
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
                                                                                                            ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) -> Tdat1
#
##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class USE mod_recruit here.
Tdat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code,mod_recruit) %>% # use AREA here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No + Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

##
##### Weighted CPUE current year -----------------------------------
##
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area_km) ->dat5

#check to make sure there aren't crab without a assigned recruit class. 
dat5 %>%
  filter(No_crab > 0)

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Location, Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females))))),
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females)))))) -> CPUE_wt_16
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_wt_16, './results/TCS/CPUE_16.csv') # contains last four years of survey data 

###
##### Short term trends -------------------------------------
###
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

head(dat3) # make sure this is the file with each recruit class as a column by year, location and pot no
dat3 %>%
  filter(Year >=2013) -> dat3 # confirm that is only contains the last 4 years.  This year needs to be changed every year

dat3_long <- gather(dat3, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

dat3_long %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(Location, mod_recruit) %>%
  do(fit = lm(crab ~ Year, data =.)) -> short_term

short_term %>%
  tidy(fit) -> short_term_slope

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(Location, mod_recruit, r.squared, p.value)->short_term_out2

short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(Location, mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, './results/TCS/TCS_shortterm.csv')

dat3_long %>%
  filter(mod_recruit %in% recruit_used) ->st_dat3_long
  
ggplot(st_dat3_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~Location)
###
### just thomas bay Large.Females
dat3_long %>%
  filter(Location == 'Thomas Bay', mod_recruit == 'Large.Females') -> graph1
ggplot(graph1, aes(Year, crab, color = mod_recruit)) + geom_point()

###
##### Long term trends ---------------------
###
#compare 2016 CPUE distribution to the long term mean, keep Location seperate
dat3 %>%
  filter(Year == 2016) ->dat3_2016
#make sure you have a file with only 2016 data
# long term baseline values are different for each area, I guess make a file for each area?
#
# the y = has to be changed for each area but once they are set they are the same from year to year
# THIS NEEDS TO BE A WEIGHTED MEAN - see processingCODE.R
dat3_2016 %>%
  filter(Location == "Glacier Bay") ->long_term_16
t.test(long_term_16$Large.Females, mu = 5.77)
t.test(long_term_16$Pre_Recruit, mu = 5.62)
t.test(long_term_16$Recruit, mu = 1.37)
t.test(long_term_16$Post_Recruit, mu = 1.13)
#
dat3_2016 %>%
  filter(AREA == "GB") ->long_term_16
t.test(long_term_16$Large.Females, mu = 5.77)
t.test(long_term_16$Pre_Recruit, mu = 4.91)
t.test(long_term_16$Recruit, mu = 3.39)
t.test(long_term_16$Post_Recruit, mu = 1.72)
#
dat3_2016 %>%
  filter(AREA == "SC") ->long_term_16
t.test(long_term_16$Large.Females, mu = 4.19)
t.test(long_term_16$Pre_Recruit, mu = 2.96)
t.test(long_term_16$Recruit, mu = 2.83)
t.test(long_term_16$Post_Recruit, mu = 1.19)
#
dat3_2016 %>%
  filter(AREA == "LS") ->long_term_16
t.test(long_term_16$Large.Females, mu = 3.33)
t.test(long_term_16$Pre_Recruit, mu = 3.47)
t.test(long_term_16$Recruit, mu = 3.61)
t.test(long_term_16$Post_Recruit, mu = 2.73)
#
dat3_2016 %>%
  filter(AREA == "EI") ->long_term_16
t.test(long_term_16$Large.Females, mu = 9.15)
t.test(long_term_16$Pre_Recruit, mu = 6.91)
t.test(long_term_16$Recruit, mu = 3.78)
t.test(long_term_16$Post_Recruit, mu = 2.36)
#
dat3_2016 %>%
  filter(AREA == "PS") ->long_term_16
t.test(long_term_16$Large.Females, mu = 2.55)
t.test(long_term_16$Pre_Recruit, mu = 4.19)
t.test(long_term_16$Recruit, mu = 0.94)
t.test(long_term_16$Post_Recruit, mu = 1.27)
