#K.Palof 
# ADF&G 8-1-16 updated /10-6-16 
# Areas: RKCS areas for Tanner crab - EXCLUDES north juneau and stephens passage
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP, prior to 2016  

rm(list = ls()) # clear workspace since data frames have same names
#####Load Packages ---------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
#library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(weights)

##################################################################
#####Load Data ---------------------------------------------------
##################################################################
# change input file and input folder for each
dat <- read.csv("./data/rkc_tanner/RKCS_forTanner.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
#area <- read.csv("./data/Juneau/Juneau_Barlow_strata_area.csv") 
                  #this file is the same every year.  Unless the survey methods change
#histdat <- read.csv("./data/Juneau/2Juneau Stratified CPUE 2015_area formula.csv")
                  ## !!!!  In future years this file will be 'JNU_CPUE_ALL' and just get updated with current years data.
#females <- read.csv("./data/Juneau/RKC_11_16_large females_by_pot.csv")
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##################################################################
##### Initial review of new data ---------------------------------
##################################################################
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

####################################################################
##### Tanner specific manipulations -----------------------------
##### Survey areas ONLY -----------------------
# remove Juneau and Barlow - do these seperately due to needing GIS to seperate the areas.
# also need to remove other "experimental" areas - simplify to areas used in the assessment
levels(dat1$Location)
rkc_usable_areas <- c("Deadman Reach", "Excursion Inlet", "Gambier Bay", "Lynn Sisters", "Pybus Bay", 
                      "Seymour Canal", "St. James Bay")
dat1 %>%
  filter(Location %in% rkc_usable_areas) -> dat1a
##### add columns used later ----------------------------
dat1a %>%
  mutate(AREA = ifelse(Location.Code == 26 | Location.Code == 42, 'LS',
                       ifelse(Location.Code == 4, 'PS', ifelse(Location.Code == 9, 'EI', 
                            ifelse(Location.Code== 15, 'GB', ifelse(Location.Code == 37, 
                                'PB', ifelse(Location.Code==39, 'SC', 0))))))) ->dat1ab

dat1ab %>%
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', ifelse(Sex.Code ==1 & Width.Millimeters <110, 'Juvenile', 
                              ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138,'Pre_Recruit', 
                               ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 & Shell.Condition.Code <4, 'Recruit',
                                ifelse(Sex.Code ==1 & Width.Millimeters >169|(Shell.Condition.Code >3 & Width.Millimeters >137), 'Post_Recruit', 
                                  ifelse(Sex.Code ==2 & Egg.Development.Code==4, 'Small.Female', 
                                         ifelse(Sex.Code ==2 & Width.Millimeters>0, 'Large.Female', 'Missing')))))))) -> Tdat1
write.csv(Tdat1, './results/problemstanner.csv')
# need to STOP here and fix problems with data.  Not sure why some females are showing up as NA....also need to fill in missing 
#  widths and density strata (although this applies to red crab NOT tanner)
##################################################################
##### By Pot ----------------------------------------------------
##################################################################
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, AREA, Pot.No, Density.Strata.Code, mod_recruit) %>% # use AREA here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + AREA + Pot.No +Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

dat3 %>%
  select(NA > 0)
#head(dat3)# check to make sure things worked.

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

##################################################################
##### Weighted CPUE current year -----------------------------------
##################################################################
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  rename(Missing = Var.5, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
# this is neccessary so that current years file (dat5) matches the historic file names

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_16
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_wt_16, './results/Juneau/JNU_CPUE_16.csv')

##################################################################
##### Historic file ---------------------------------------
##################################################################
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

historicdata <- histdat[,1:15] # remove last two column of total crab and 
 # has all data from 2001 to 2015

# need to add 2016 to historicdata file
# Locations in historic file are numbers.  Here I have names, should I change this?
# only 2016 data 
dat5 %>%
  filter(Year == 2016) -> dat5_2016 
CPUE_ALL_YEARS <- rbind(historicdata, dat5_2016)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, './results/Juneau/JNU_perpot_all_16.csv')

##################################################################
##### Short term trends -------------------------------------
##################################################################
#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >=2013) -> BYPOT_ST_16 # short term file has last 4 years in it

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Juvenile)
Juv_fit <-lm(Juvenile ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(Juv_fit, col= 'red')
summary(Juv_fit)

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Large.Females)
Lfem_fit <-lm(Large.Females ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(Lfem_fit, col= 'red')
summary(Lfem_fit)

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Post_Recruit)
PR_fit <-lm(Post_Recruit ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(PR_fit, col= 'red')
summary(PR_fit)

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Pre_Recruit)
PreR_fit <-lm(Pre_Recruit ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(PreR_fit, col= 'red')
summary(PreR_fit)

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Recruit)
R_fit <-lm(Recruit ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(R_fit, col= 'red')
summary(R_fit)

plot(BYPOT_ST_16$Year, BYPOT_ST_16$Small.Females)
smF_fit <-lm(Small.Females ~ Year, data = BYPOT_ST_16, weights = weighting)
abline(smF_fit, col= 'red')
summary(smF_fit)

##################################################################
##### Long term trends ---------------------
##################################################################
#compare 2016 CPUE distribution to the long term mean
dat5 %>%
  filter(Year == 2016) ->dat5_2016
#make sure you have a file with only 2016 data

#Uses a weighted mean to help calculate the t.test - part of package weights
# the y = has to be changed for each area but once they are set they are the same from year to year
wtd.t.test(dat5_2016$Juvenile, y = 5.51, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Large.Females, y = 8.07, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Post_Recruit, y = 2.19, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Pre_Recruit, y = 3.07, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Recruit, y = 1.98, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Small.Females, y = 6.35, weight = dat5_2016$weighting, samedata=FALSE)

##################################################################
##### Weights from length - weight relatinship.
##################################################################
# Linear model is changed for each area
# Juneau linear model: exp(3.03*log(length in mm)-7.23)*2.2/1000
glimpse(dat1) # raw data for both 2015 and 2016 
dat1 %>%
  mutate(weight_lb = (exp((3.03*log(Length.Millimeters))-7.23))*(2.2/1000)) -> dat1

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")
#Mature
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status %in% Mature)%>%
  group_by(Year) %>%
  summarise(mature_lbs = wt.mean(weight_lb, Number.Of.Specimens))
#legal
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status %in% Legal)%>%
  group_by(Year) %>%
  summarise(legal_lbs = wt.mean(weight_lb, Number.Of.Specimens))
#Pre-Recruit
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status == "Pre_Recruit")%>%
  group_by(Year) %>%
  summarise(legal_lbs = wt.mean(weight_lb, Number.Of.Specimens))

##################################################################
##### Females - large or mature females --------------------------
##################################################################
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
# need to change these to 0. 
LgF_dat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1

LgF_dat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_dat1 # where 1 is yes and 2 is no

LgF_dat1 %>%
  group_by(Year, Location, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Year)%>%
  summarise(Pclutch = mean(var1) , Pclutch.se = (sd(var1))/sqrt(sum(!is.na(var1))))
# check to see if these match JMP file

##################################################################
##### Long term females -------------------------
##################################################################
glimpse(poorclutch1)
#compare 2016 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == 2016) ->poorclutch1_2016
#make sure you have a file with only 2016 data
#calculate the t.test
t.test(poorclutch1_2016$var1, mu = 0.10)

##################################################################
##### Short term females ------------------------
##################################################################
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
#After that this should create a file to use in the future
# open female input file (.csv) and delete N.rows and Missing columns also change variables names to n, y, var1
females_all <- rbind(females, poorclutch1_2016)
# here use females because it already has 2016
females_16 <- females

females_16 %>%
  filter(Year >=2013) -> LgF_short # short term file has last 4 years in it
#output this file as .csv to add to next year
write.csv(LgF_short, './results/Juneau/poorclutchfemales_16.csv')

plot(LgF_short$Year, LgF_short$var1)
LgF_fit <-lm(var1 ~ Year, data = LgF_short)
abline(LgF_fit, col= 'red')
summary(LgF_fit)

##################################################################
##### egg percentage overall -----------------------------------
##################################################################
LgF_dat1 %>%
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean)))))


#############################################################
##### input for CSA in R ---------------------------
############################################################













