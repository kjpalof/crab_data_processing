#K.Palof 
# ADF&G 8-1-16 updated for Excursion Inlet  / updated 8-3-17
# code to process data from Ocean AK to use in crab CSA models.  
#  Currently this is done in excel then JMP, prior to 2016
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
library(grid)
library(gridExtra)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))


source('./code/functions.R')

#####Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/redcrab/Excursion/RKC survey CSA_EI_16_17.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/redcrab/Excursion/Excursion_strata_area.csv") 
                  #this file is the same every year.  Unless the survey methods change
histdat <- read.csv("./data/redcrab/Excursion/EI_79_16_bypot.csv")
                  ## !!!!  In future years this file will be 'EI_perpot_all_16' and just get updated with current years data.
females <- read.csv("./data/redcrab/Excursion/poorclutchfemales_16.csv")
raw_data <- read.csv("./data/redcrab/Excursion/RKC survey_historicpots_ei.csv")
        ## use this for raw historic female data in 2017, create input file for future

baseline <- read.csv("./data/redcrab/longterm_means.csv")
biomass <- read.csv("./data/redcrab/biomass.csv")
# file for all locations.  Has legal biomass from CSA, harvest
# mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
# adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
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
write.csv(CPUE_wt_17, './results/redcrab/Excursion/EI_CPUE_17.csv')

#### survey mid date -----
head(dat)
unique(dat$Time.Hauled)
# need to seperate time hauled to just have data hauled look for mid-date 
dat %>% filter(Year == 2017)  # 7-15
dat[2001,7] # 7-16
# so mid-date would be 16th.


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
write.csv(CPUE_ALL_YEARS, './results/redcrab/Excursion/EI_perpot_all_17.csv')

##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >=2014) -> bypot_st # short term file has last 4 years in it

#function creates output file in folder /results/redcrab/'area'
short_t(bypot_st, 2017, "Excursion")
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

long_t(dat5_current, baseline, 2017, 'Excursion', 'Excursion')
# output is saved as longterm.csv

##### Weights from length - weight relatinship.-----------------
    # Linear model is changed for each area
    # Excursion linear model: exp(3.12*log(length in mm)-7.67)*2.2/1000
glimpse(dat1) # raw data for both 2016 and 2017
    # slope = 3.12
    # intercept = 7.67
    # use function found in functions.R code file
weights(dat1, 3.12, 7.67, "Excursion")
# output saved as maleweights.csv

##### Females - large or mature females --------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1 # current 2 years
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
# need to change these to 0. 
LgF_dat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1


raw_data %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> largef
largef %>% select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
                  Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
                  Egg.Development.Code, Egg.Condition.Code) -> largef

LgF_dat1 %>% filter(Year == 2017) %>% select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
                                             Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
                                             Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_2017

largef_all <- rbind(largef, LgF_dat1_2017) # raw female data for all years.

##### % poor (<10 %) clutch -----------------------------------

poor_clutch(largef_all, 'Excursion', 2017)
# output is saved as poorclutch1_current.csv - which has all pots for 2017
# and poorclutch_summary_all.csv which has the percentage and 
#                                          SD of poor clutches for all years

##### Long term females -------------------------
poorclutch_current <- read.csv("./results/redcrab/Excursion/poorclutch1_current.csv")
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Excursion', 2017)
# output saved as lt_female.csv

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years
poorclutch_all <- read.csv("./results/redcrab/Excursion/poorclutch_all.csv")
#function for short term trends and output saving.
poor_clutch_short(poorclutch_all, 'Excursion', 2017)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(largef_all, 'Excursion', 2017)
# output saved as egg_percent_mean_all.csv, creates mean and SE egg percentage for all years

### total stock health table -----------------------
total_health('Excursion', 2017)
# works as long as all files are saved in folder with area name

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year, Location) %>%  select(Year, Location, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(funs(sum)) -> raw_samp
write.csv(raw_samp, './results/redcrab/Excursion/raw_sample.csv')
dat5 %>% group_by(Year) %>% summarise (n=n())

##### Restrospective Analysis -----------------------------------
head(CPUE_ALL_YEARS)

# input file that has the old pots and their new assigned strata - prior to 2005
# Lynn Sisters and St.James Bay

retrodata <- read.csv("./data/1979_2004_RKCS_Strata.csv")
levels(retrodata$Location)
# this is input from OceanAK - set up as red crab survey data for CSA
EI = c("Excursion Inlet")
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
### cpue all years --------------------
CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all
write.csv(CPUE_wt_all, './results/Excursion/EI_CPUE_historical.csv')

strata79_04 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_79_04
CPUE_wt_79_04
write.csv(CPUE_wt_79_04, './results/Excursion/EI_CPUE_79_04.csv')

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

### stock assessment figures --------------
head(CPUE_ALL_YEARS)
CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all  
CPUE_wt_all %>% filter(Year >= 1993) -> CPUE_wt_93_17

### Mature males-----
#create data frame that has mature males - just means
# data fame that has mature males - just SE
CPUE_wt_93_17 %>% select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
                         PreR_SE, Rec_SE, PR_SE) -> males
males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
males_long %>% mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                             "pre.recruit", 
                                             ifelse(recruit.status == "Recruit_wt", 
                                                    "recruit", ifelse(recruit.status == "PreR_SE", 
                                                    "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                      "recruit", "post.recruit "))))) %>% 
          mutate(type = ifelse(recruit.status == "PreR_SE",
                               "se", 
                               ifelse(recruit.status == "Rec_SE", 
                                      "se", ifelse(recruit.status == "PR_SE", 
                                                        "se", "mean"))))-> males_long
males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph

### females/juv prep ------------
CPUE_wt_93_17 %>% select(Year,Juvenile_wt, SmallF_wt, MatF_wt, 
                         Juv_SE, SmallF_SE, MatF_SE) -> femjuv
femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_wt:MatF_SE, factor_key = TRUE)
femjuv_long %>% mutate(recruit.class = ifelse(recruit.status == "Juvenile_wt",
                                   "juvenile.male", 
                                ifelse(recruit.status == "SmallF_wt", 
                                "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                                "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                "juvenile.female", "mature.female"))))) %>% 
  mutate(type = ifelse(recruit.status == "Juv_SE",
                       "se", 
                       ifelse(recruit.status == "SmallF_SE", 
                              "se", ifelse(recruit.status == "MatF_SE", 
                                           "se", "mean"))))-> femjuv_long
femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
#### F1a mature male plot -----------
p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class))+ 
  geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
  geom_line(aes(color = recruit.class, group = recruit.class))+
  scale_colour_manual(name = "", values = c("grey1", "grey62", "grey34"))+
  scale_shape_manual(name = "", values = c(15, 16, 17))+
  
  ylim(0,7) +ggtitle("Excursion Inlet") + ylab("CPUE (number/pot)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                width =.4) +
  geom_hline(yintercept = baseline[3,5], color = "grey62")+
  geom_hline(yintercept = baseline [3,6], color = "grey34")+
  geom_hline(yintercept = baseline [3,7], color = "black")+
  theme(legend.position = c(0.8,0.7))

### F1b females/juvenile plot ---------------
p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class))+ 
  geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
  geom_line(aes(color = recruit.class, group = recruit.class))+
  scale_colour_manual(name = "", values = c("grey34","grey62", "grey1"))+
  scale_shape_manual(name = "", values = c(17, 16, 15))+
  
  ylim(0,25) +ggtitle("") + ylab("CPUE (number/pot)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                width =.4) +
  geom_hline(yintercept = baseline[3,2], color = "grey62")+
  geom_hline(yintercept = baseline [3,3], color = "grey50")+
  geom_hline(yintercept = baseline [3,4], color = "grey1")+
  theme(legend.position = c(0.8,0.7))


### poor clutch egg percent data processing------------
poorclutch_summary <- read.csv("./results/redcrab/Excursion/poorclutch_summary_all.csv")
poorclutch_summary %>% filter(Year >= 1993) -> poorclutch_summary93
poorclutch_summary93 %>% mutate(Pclutch100 = Pclutch *100, 
                           Pclutch.se100 = Pclutch.se*100) %>% 
  select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary93
# file with year and mean percent poor clutch and se poor clutch from 1993 to current
egg_mean_all <- read.csv("./results/redcrab/Excursion/egg_percent_mean_all.csv")
egg_mean_all %>% filter(Year >= 1993) -> egg_mean_all_93

# combine these data sets for graphing.  Create one with means and one with SEs.
poorclutch_summary93 %>% left_join(egg_mean_all_93) -> female_egg
female_egg_long <- gather(female_egg, vname, value1, Pclutch100:egg.se, factor_key = TRUE)
female_egg_long %>% mutate(female.egg = ifelse(vname == "Pclutch100",
                               "% poor clutch", 
                              ifelse(vname == "mean", 
                              "total % clutch", ifelse(vname == "Pclutch.se100", 
                              "% poor clutch", "total % clutch")))) %>% 
  mutate(type = ifelse(vname == "Pclutch.se100",
                       "se", 
                       ifelse(vname == "egg.se", 
                              "se", "mean")))-> female_egg_long
female_egg_long %>% select (-vname) %>% spread(type, value1) -> female_egg_graph

#### F1c Female eggs graph -----------
p3 <- ggplot(female_egg_graph, aes(Year, mean, group = female.egg))+ 
  geom_point(aes(color = female.egg, shape = female.egg), size =3) +
  geom_line(aes(color = female.egg, group = female.egg))+
  scale_colour_manual(name = "", values = c("grey1", "black"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,100) +ggtitle("") + ylab("Percentage")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                width =.4) +
  theme(legend.position = c(0.2,0.5))

### biomass harvest graph --------------

# file for all locations.  Has legal biomass from CSA, harvest
# mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
# adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
biomass %>% filter(Location == "Excursion") %>% 
  select(Year, legal.biomass, harvest, mr.biomass, adj.biomass) ->ei.biomass
ei.biomass_long <- gather(ei.biomass, type, pounds, legal.biomass:adj.biomass, factor_key = TRUE)

p4 <- ggplot(ei.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1", "grey1", "grey62"))+
  scale_shape_manual(name = "", values = c(16, 1, 18, 18))+
  
  ylim(0,715000) +ggtitle("") + ylab("Legal biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
    theme(legend.position = c(0.8,0.6)) + 
  geom_hline(yintercept = 83351, color = "grey1")+
  geom_hline(yintercept = 411756, color = "grey62", linetype = "dashed")
  
ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal.biomass))
ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(adj.biomass))
### FINAL plot -------------
png('./results/redcrab/Excursion/figure1.png', res= 300, width = 8, height =11, units = "in")
grid.arrange(p1, p2, p3, p4, ncol = 1)
dev.off()

