#K.Palof 
# ADF&G 7-15-16
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP.  

#####Load Packages ---------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(tidyr)
library(plotrix)
library(SDMTools)
library(weights)

#####Load Data --------------------------------------
# change input file and input folder
dat <- read.csv("C:/Users/kjpalof/Documents/R projects/data processing/data/JNU_15_16_oceanAK_out_RAW.csv")
# this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("C:/Users/kjpalof/Documents/R projects/data processing/data/Juneau_Barlow_strata_area.csv")
  #this file is the same every year.  Unless the survey methods change
histdat <- read.csv("C:/Users/kjpalof/Documents/R projects/data processing/data/2Juneau Stratified CPUE 2015_area formula.csv")
  ## !!!!  In future years this file will be 'JNU_CPUE_ALL' and just get updated with current years data.
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data -------------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

##### By Pot -------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
dat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarize(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

#head(dat3)# check to make sure things worked.

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarize(npots  = length(Pot.No)) -> pots_per_strata

#####Weighted CPUE current year -----------------------------------
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
  summarize(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_JNU_16
# check to confirm last years CPUEs match - that's why we use two years.

##### Historic file ---------------------------------------
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

historicdata <- histdat[,1:15] # remove last two column of total crab and 
 # has all data from 2001 to 2015

# need to add 2016 to historicdata file
# Locations in historic file are nubmers.  Here I have names, should I change this?
# only 2016 data 
dat5 %>%
  filter(Year == 2016) -> dat5_2016 
JNU_CPUE_ALL <- rbind(historicdata, dat5_2016)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)

##### Short term trends -------------------
#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
# How to take weights into account here?
JNU_CPUE_ALL %>%
  filter(Year >=2013) -> JNU_ST_16 # short term file has last 4 years in it

plot(JNU_ST_16$Year, JNU_ST_16$Juvenile)
Juv_fit <-lm(Juvenile ~ Year, data = JNU_ST_16, weights = weighting)
abline(Juv_fit, col= 'red')
summary(Juv_fit)

plot(JNU_ST_16$Year, JNU_ST_16$Large.Females)
Lfem_fit <-lm(Large.Females ~ Year, data = JNU_ST_16, weights = weighting)
abline(Lfem_fit, col= 'red')
summary(Lfem_fit)

plot(JNU_ST_16$Year, JNU_ST_16$Post_Recruit)
PR_fit <-lm(Post_Recruit ~ Year, data = JNU_ST_16, weights = weighting)
abline(PR_fit, col= 'red')
summary(PR_fit)

plot(JNU_ST_16$Year, JNU_ST_16$Pre_Recruit)
PreR_fit <-lm(Pre_Recruit ~ Year, data = JNU_ST_16, weights = weighting)
abline(PreR_fit, col= 'red')
summary(PreR_fit)

plot(JNU_ST_16$Year, JNU_ST_16$Recruit)
R_fit <-lm(Recruit ~ Year, data = JNU_ST_16, weights = weighting)
abline(R_fit, col= 'red')
summary(R_fit)

plot(JNU_ST_16$Year, JNU_ST_16$Small.Females)
smF_fit <-lm(Small.Females ~ Year, data = JNU_ST_16, weights = weighting)
abline(smF_fit, col= 'red')
summary(smF_fit)

##### Long term trends ---------------------
#compare 2016 CPUE distribution to the long term mean
dat5 %>%
  filter(Year == 2016) ->dat5_2016
#make sure you have a file with only 2016 data

#Uses a weighted mean to help calculate the t.test - part of package weights
wtd.t.test(dat5_2016$Juvenile, y = 5.51, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Large.Females, y = 8.07, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Post_Recruit, y = 2.19, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Pre_Recruit, y = 3.07, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Recruit, y = 1.98, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Small.Females, y = 6.35, weight = dat5_2016$weighting, samedata=FALSE)

##### Females ----------------------------------------