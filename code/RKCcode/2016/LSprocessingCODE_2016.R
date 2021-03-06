#K.Palof 
# ADF&G 8-4-16 updated for Lynn Sister or Lynn canal or St.James - all the same
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP, prior to 2016  
rm(list = ls())# clear workspace from previous area 
#####Load Packages ---------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)
#library(tidyr)
library(plotrix)
library(SDMTools)
library(weights)

##################################################################
#####Load Data ---------------------------------------------------
##################################################################
# change input file and input folder for each
dat <- read.csv("./data/LynnSisters/RKCsurveyCSA_LS_15_16.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/LynnSisters/LynnCanal_strata_area.csv") 
                  #this file is the same every year.  Unless the survey methods change
histdat <- read.csv("./data/LynnSisters/LynnSistersCPUE_2015.csv")
                  ## !!!!  In future years this file will be 'JNU_CPUE_ALL' and just get updated with current years data.
females <- read.csv("./data/LynnSisters/LS_11_15_largefemales_bypot.csv")

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
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

##################################################################
##### By Pot ----------------------------------------------------
##################################################################
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
#Location name here is Deadman Reach
dat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

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
CPUE_wt_16
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_wt_16, './results/LynnSisters/LS_CPUE_16.csv')

##################################################################
##### Historic file ---------------------------------------
##################################################################
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

head(histdat) # see if any columns don't match those in dat5 - changed variable names in Excel to match dat 5
head(dat5)

#histdat %>%
 # select(-Total.Crab) -> historicdata
historicdata <- histdat# nothing needs to be removed here 
 # has all data from 2001 to 2015
dat5 %>%
  select(-npots) -> dat5

# need to add 2016 to historicdata file
# Locations in historic file are numbers.  Here I have names, should I change this?
# only 2016 data 
dat5 %>%
  filter(Year == 2016) -> dat5_2016 
CPUE_ALL_YEARS <- rbind(historicdata, dat5_2016)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, './results/LynnSisters/LS_perpot_all_16.csv')

##################################################################
##### Short term trends -------------------------------------
##################################################################
#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >=2013) -> BYPOT_ST # short term file has last 4 years in it

plot(BYPOT_ST$Year, BYPOT_ST$Juvenile)
Juv_fit <-lm(Juvenile ~ Year, data = BYPOT_ST, weights = weighting)
abline(Juv_fit, col= 'red')
summary(Juv_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Small.Females)
smF_fit <-lm(Small.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(smF_fit, col= 'red')
summary(smF_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Large.Females)
Lfem_fit <-lm(Large.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(Lfem_fit, col= 'red')
summary(Lfem_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Pre_Recruit)
PreR_fit <-lm(Pre_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PreR_fit, col= 'red')
summary(PreR_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Recruit)
R_fit <-lm(Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(R_fit, col= 'red')
summary(R_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Post_Recruit)
PR_fit <-lm(Post_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PR_fit, col= 'red')
summary(PR_fit)

##################################################################
##### Long term trends ---------------------
##################################################################
#compare 2016 CPUE distribution to the long term mean
dat5 %>%
  filter(Year == 2016) ->dat5_2016
#make sure you have a file with only 2016 data

#Uses a weighted mean to help calculate the t.test - part of package weights
# the y = has to be changed for each area but once they are set they are the same from year to year
wtd.t.test(dat5_2016$Juvenile, y = 3.95, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Small.Females, y = 4.37, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Large.Females, y = 3.40, weight = dat5_2016$weighting, samedata=FALSE)

wtd.t.test(dat5_2016$Pre_Recruit, y = 1.97, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Recruit, y = 1.27, weight = dat5_2016$weighting, samedata=FALSE)
wtd.t.test(dat5_2016$Post_Recruit, y = 1.41, weight = dat5_2016$weighting, samedata=FALSE)


##################################################################
##### Weights from length - weight relatinship ------------------
##################################################################
# Linear model is changed for each area
# Lynn Sisters linear model: exp(3.07*log(length in mm)-7.42)*2.2/1000
glimpse(dat1) # raw data for both 2015 and 2016 
dat1 %>%
  mutate(weight_lb = (exp((3.07*log(Length.Millimeters))-7.42))*(2.2/1000)) -> dat1

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")
#Mature
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status %in% Mature)%>%
  group_by(Year) %>%
  summarise(mature_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> a
#legal
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status %in% Legal)%>%
  group_by(Year) %>%
  summarise(legal_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> b
#Pre-Recruit
dat1 %>%
  filter(Sex.Code ==1, Recruit.Status == "Pre_Recruit")%>%
  group_by(Year) %>%
  summarise(preR_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> c

a %>%
  right_join(b, by = "Year") %>%
  right_join(c, by= "Year") -> weight_lb_16
weight_lb_16
write.csv(weight_lb_16, './results/LynnSisters/weights_2016.csv')
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

#write.csv(LgF_dat1, './results/Excursion/largefemales_16.csv')
LgF_dat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_dat1 # where 1 is yes and 2 is no

LgF_dat1 %>%
  group_by(Year, Location, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1 # specifically changed for Seymour since no poor clutches were observed
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
glimpse(females)
# above is specific to Pybus because no location column was there from JMP
females_all <- rbind(females, poorclutch1_2016)
# here use females because it already has 2016
#females_16 <- females
write.csv(females_all, './results/LynnSisters/LS_largeF_11_16.csv')

females_all %>%
  filter(Year >=2013) -> LgF_short # short term file has last 4 years in it
#output this file as .csv to add to next year
write.csv(LgF_short, './results/LynnSisters/poorclutchF_ST_16.csv')

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


###################################################################
##### Restrospective Analysis -----------------------------------
###################################################################
head(CPUE_ALL_YEARS)

# input file that has the old pots and their new assigned strata - prior to 2005
# Lynn Sisters and St.James Bay

retrodata <- read.csv("./data/1979_2004_RKCS_Strata.csv")
# this is input from OceanAK - set up as red crab survey data for CSA
LynnSisters = c("Lynn Sisters", "St. James Bay")
retrodata %>%
  filter(Location %in% LynnSisters) -> LSretrodata
LSretrodata %>%
  filter(Year > 1992) %>%
  select(-PROJECT_CO) -> LSretrodata

CPUE_ALL_YEARS %>%
  right_join(LSretrodata) -> strata79_04
# need to recreate weighting column for this data
strata79_04 %>%
  mutate(Density.Strata.Code = STRATA) %>%
  select(-Area) %>%
  select(-inverse_n) %>%
  select(-weighting) %>%
  right_join(area) -> tab1

tab1 %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

tab1 %>%
  right_join(pots_per_strata) -> strata79_04

strata79_04 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) -> strata79_04

# Now calculate CPUE from CPUE_ALL_YEARS and strata79_04 and compare values prior to 2005

CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all
write.csv(CPUE_wt_all, './results/LynnSisters/LS_CPUE_historical.csv')
#CPUE_wt_all <- read.csv("./results/LynnSisters/LS_CPUE_historical.csv")

strata79_04 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_79_04
CPUE_wt_79_04
write.csv(CPUE_wt_79_04, './results/LynnSisters/LS_CPUE_79_04.csv')
CPUE_wt_79_04 <- read.csv("./results/LynnSisters/LS_CPUE_79_04.csv")
#################################################
##### graph retrospective ------------------------
#############################################

ggplot(CPUE_wt_all, aes(Year, Pre_Recruit_wt)) + geom_point() +geom_line() + ylim(0,6)

#PreR
ggplot(CPUE_wt_all, aes(Year, Pre_Recruit_wt)) + geom_point() +geom_line(color = 'red') + 
  ylim(0,4) +ggtitle("LS Pre Recruits") +
  geom_line(data = CPUE_wt_79_04)
# Recruit
ggplot(CPUE_wt_all, aes(Year, Recruit_wt)) + geom_point() +geom_line(color = 'red') +
  ylim(0,3) + ggtitle("LS Recruits") +
  geom_line(data = CPUE_wt_79_04)
#Post Recruit
ggplot(CPUE_wt_all, aes(Year, Post_Recruit_wt)) + geom_point() +geom_line(color = 'red') +
  ylim(0,5) + ggtitle("LS Post Recruits")+
  geom_line(data = CPUE_wt_79_04)




