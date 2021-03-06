#K.Palof 
# ADF&G 7-11-16
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP.  
# This code is for the initial year - 2016, refer to code for processing after this year.

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

#####Load Data --------------------------------------
dat <- read.csv("./data/Juneau/JNU_15_16_oceanAK_out_RAW.csv")
area <- read.csv("./data/Juneau/Juneau_Barlow_strata_area.csv")
histdat <- read.csv("./data/Juneau/2Juneau Stratified CPUE 2016_area formula.csv")
females <- read.csv("./data/Juneau/RKC_11_16_large females_by_pot.csv")

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
dat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code) %>%
  summarise (total_crab = sum(Number.Of.Specimens)) #gets you total crab per pot.

# need Number of Specimens by recruit class
dat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

head(dat3)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

#####Weighted CPUE current year -----------------------------------
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  rename(Missing = Var.5, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
 
#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_JNU_16

write.csv(CPUE_wt_JNU_16, './results/JNU_CPUE_16.csv')

##### Historic file ---------------------------------------
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

hisdat <- read.csv("./data/2Juneau Stratified CPUE 2016_area formula.csv")
hisdat_15 <- read.csv("./data/2Juneau Stratified CPUE 2015_area formula.csv")
hisdat_15 <- hisdat_15[,1:15]
historicdata <- hisdat[,1:15] # has all data from 2001 to 2016

# need to add 2016 to hisdat_15 file
# Locations in historic file are nubmers.  Here I have names, should I change this?
# only 2016 data 
dat5 %>%
  filter(Year == 2016) -> dat5_2016
JNU_CPUE_ALL <- rbind(hisdat_15, dat5_2016)

write.csv(JNU_CPUE_ALL, './results/JNU_perpot_all_16.csv')

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

##### Weights from length - weight relatinship.
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

###### Females ----------------------------------------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

##### % poor (<10 %) clutch
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

##### Long term females -------------------------
glimpse(poorclutch1)
#compare 2016 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == 2016) ->poorclutch1_2016
#make sure you have a file with only 2016 data
#calculate the t.test - part of package weights
t.test(poorclutch1_2016$var1, mu = 0.10)

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# How to take weights into account here?
# this should only have to be done the first time.  
#After that this should create a file to use in the future
# open female input file (.csv) and delete N.rows and Missing columns also change variables names to n, y, var1
females_all <- rbind(females, poorclutch1_2016)
# here use females because it already has 2016
females_16 <- females

females_16 %>%
  filter(Year >=2013) -> LgF_short # short term file has last 4 years in it
#output this file as .csv to add to next year
write.csv(LgF_short, '/Users/kjpalof/Documents/R projects/data processing/results/poorclutchfemales_16.csv')

plot(LgF_short$Year, LgF_short$var1)
LgF_fit <-lm(var1 ~ Year, data = LgF_short)
abline(LgF_fit, col= 'red')
summary(LgF_fit)

##### egg percentage overall -----------------------------------
LgF_dat1 %>%
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean)))))
  


####### explore ------------------
#matches JMP but code below is more efficient - uses library SDMTools
#dat5 %>%
# group_by(Year) %>%
#  summarize(Pre_Recruit_wt = (sum(Pre_Recruit * weighting)/ sum(weighting)), Pre_R_SE = std.error(Pre_Recruit),
#            Recruit_wt = sum(Recruit*weighting)/sum(weighting)) # this matches what is being calculated in JMP



