#K.Palof 
# ADF&G 8-3-16 updated for Seymour Canal  / updated 8-8-17
# code to process data from Ocean AK to use in crab CSA models.  
#  
# Current year: 2017
rm(list = ls())# clear workspace from previous area 
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

source('./code/functions.R')

#####Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/redcrab/Seymour/RKCsurveyCSA_SC_16_17.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/redcrab/Seymour/Seymour_strata_area.csv") 
                  #this file is the same every year.  Unless the survey methods change
histdat <- read.csv("./data/redcrab/Seymour/SC_79_16_bypot.csv")
                  ## !!!!  In future years this file will be 'EI_perpot_all_16' and just get updated with current years data.
females <- read.csv("./data/redcrab/Seymour/SC_largeF_11_16.csv")

baseline <- read.csv("./data/redcrab/longterm_means.csv")
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)
# one female here that don't have recruit status due to no lengths

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2
# keep trip no to merge with historic data.

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

head(dat3)# check to make sure things worked.

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata


##### Weighted CPUE current year -----------------------------------
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
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
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_17
CPUE_wt_17
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_wt_17, './results/redcrab/Seymour/SC_CPUE_17.csv')

#### survey mid date -----
head(dat)
unique(dat$Time.Hauled)
# need to seperate time hauled to just have data hauled look for mid-date 
dat %>% filter(Year == 2017)  # 7-29
dat[628,8] # 7-31
# so mid-date would be 30th.


##### Historic file ---------------------------------------
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

head(histdat) # see if any columns don't match those in dat5 - why doesn't historic have npots?
# new historic data has density strata as "Strata.Code"

head(dat5)

histdat %>% select(Year, Location, Trip.No, Pot.No, Strata.Code, Missing, 
                   Juvenile, Large.Females, Post_Recruit, Pre_Recruit, 
                   Recruit, Small.Females, Area, npots, inverse_n, 
                   weighting) -> historicdata
dat5 %>% rename(Strata.Code = Density.Strata.Code) -> dat6

# need to add 2017 to historicdata file
# Locations in historic file are numbers.  Here I have names, should I change this?
# only 2017 data 
dat6 %>%
  filter(Year == 2017) -> dat5_2017
CPUE_ALL_YEARS <- rbind(historicdata, dat5_2017)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, './results/redcrab/Seymour/SC_perpot_all_17.csv')

##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >=2014) -> bypot_st # short term file has last 4 years in it

#function creates output file in folder /results/redcrab/'area'
short_t(bypot_st, 2017, "Seymour")
# output is saved as shortterm.csv
bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, factor_key = TRUE) 
ggplot(bypot_st_long, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)

### short term plots----------------
plot(BYPOT_ST$Year, BYPOT_ST$Juvenile)
Juv_fit <-lm(Juvenile ~ Year, data = BYPOT_ST, weights = weighting)
abline(Juv_fit, col= 'red')
summary(Juv_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Large.Females)
Lfem_fit <-lm(Large.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(Lfem_fit, col= 'red')
summary(Lfem_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Post_Recruit)
PR_fit <-lm(Post_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PR_fit, col= 'red')
summary(PR_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Pre_Recruit)
PreR_fit <-lm(Pre_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PreR_fit, col= 'red')
summary(PreR_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Recruit)
R_fit <-lm(Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(R_fit, col= 'red')
summary(R_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Small.Females)
smF_fit <-lm(Small.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(smF_fit, col= 'red')
summary(smF_fit)

##### Long term trends ---------------------
#compare 2016 CPUE distribution to the long term mean
dat6 %>%
 filter(Year == 2017) ->dat5_current
#make sure you have a file with only current years data - created above

long_t(dat5_current, baseline, 2017, 'Seymour', 'Seymour Canal')
# output is saved as longterm.csv

##### Weights from length - weight relatinship.-----------------
    # Linear model is changed for each area
    # Seymour linear model: exp(2.87*log(length in mm)-6.438)*2.2/1000
glimpse(dat1) # raw data for both 2016 and 2017
    # slope = 2.87
    # intercept = 6.438
    # use function found in functions.R code file
weights(dat1, 2.87, 6.438, "Seymour")
# output saved as maleweights.csv

##### Females - large or mature females --------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
# need to change these to 0 if just juvenile
#LgF_dat1 %>%
 # mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1
#need to remove if missing data
LgF_dat1 %>%
 filter(!is.na(Egg.Percent)) -> LgF_dat1

#write.csv(LgF_dat1, './results/Seymour/largefemales_16.csv')
poor_clutch(LgF_dat1, 'Seymour', 2017)
# problem since there are no poor clutches - therefore no y's.  
LgF_dat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_dat1 # where 1 is yes and 2 is no

LgF_dat1 %>%
  group_by(Year, Location, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>% mutate(y = 0) -> poorclutch1

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Year)%>%
  summarise(Pclutch = mean(var1) , Pclutch.se = (sd(var1))/sqrt(sum(!is.na(var1)))) -> poorclutch_17

poorclutch1 %>% filter(Year == 2017) -> poorclutch1_current
write.csv(poorclutch1_current, ('./results/redcrab/Seymour/poorclutch1_current.csv'),row.names =FALSE)
write.csv(poorclutch_17, ('./results/redcrab/Seymour/poorclutch_17.csv'), row.names =FALSE)


# output is saved as poorclutch_current.csv - which has all pots for 2017
#     and poorclutch_17.csv which has the percentage and SD of poor clutches for 2017 

##### Long term females -------------------------
poorclutch_current <- read.csv("./results/redcrab/Seymour/poorclutch1_current.csv")
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Seymour', 2017)
# output saved as lt_female.csv

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years

females_all <- rbind(females, poorclutch_current)

#function for short term trends and output saving.
poor_clutch_short(females_all, 'Seymour', 2017)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(LgF_dat1, 'Seymour', 2017)
# output saved as egg_percent_mean.csv

### total stock health table -----------------------
total_health('Seymour', 2017)
# works as long as all files are saved in folder with area name

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year, Location) %>%  select(Year, Location, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(funs(sum)) -> raw_samp
write.csv(raw_samp, './results/redcrab/Seymour/raw_sample.csv')
dat5 %>% group_by(Year) %>% summarise (n=n())

##### Restrospective Analysis -----------------------------------
head(CPUE_ALL_YEARS)

# input file that has the old pots and their new assigned strata - prior to 2005
# Lynn Sisters and St.James Bay

retrodata <- read.csv("./data/1979_2004_RKCS_Strata.csv")
levels(retrodata$Location)
# this is input from OceanAK - set up as red crab survey data for CSA
EI = c("Seymour Inlet")
retrodata %>%
  filter(Location %in% EI) -> EIretrodata
EIretrodata %>%
  filter(Year > 1992) %>%
  select(-PROJECT_CO) -> EIretrodata

CPUE_ALL_YEARS %>%
  right_join(EIretrodata) -> strata79_04
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
write.csv(CPUE_wt_all, './results/Seymour/EI_CPUE_historical.csv')

strata79_04 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_79_04
CPUE_wt_79_04
write.csv(CPUE_wt_79_04, './results/Seymour/EI_CPUE_79_04.csv')

##### graph retrospective ------------------------
ggplot(CPUE_wt_all, aes(Year, Pre_Recruit_wt)) + geom_point() +geom_line() + ylim(0,8)

#PreR
ggplot(CPUE_wt_all, aes(Year, Pre_Recruit_wt)) + geom_point() +geom_line(color = 'red') + 
  ylim(0,8) +ggtitle("EI Pre Recruits") +
  geom_line(data = CPUE_wt_79_04)
# Recruit
ggplot(CPUE_wt_all, aes(Year, Recruit_wt)) + geom_point() +geom_line(color = 'red') +
  ylim(0,3) + ggtitle("EI Recruits") +
  geom_line(data = CPUE_wt_79_04)
#Post Recruit
ggplot(CPUE_wt_all, aes(Year, Post_Recruit_wt)) + geom_point() +geom_line(color = 'red') +
  ylim(0,5) + ggtitle("EI Post Recruits")+
  geom_line(data = CPUE_wt_79_04)

  
  










