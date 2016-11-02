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
library(broom)

##################################################################
#####Load Data ---------------------------------------------------
##################################################################
# change input file and input folder for each
dat <- read.csv("./data/rkc_tanner/RKCS_forTanner16.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
#area <- read.csv("./data/Juneau/Juneau_Barlow_strata_area.csv") 
                #NO area for this data since these are stratified by RKCS area.  
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
  filter(Recruit.Status == "", Width.Millimeters >= 1) -> test1 # this SHOULD produce NO rows.  If it does you have data problems go back and correct
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
  filter(Location %in% rkc_usable_areas) %>%
  mutate(experimental = ifelse(Location == 'Seymour Canal' & Pot.No > 54 & Year == 2015, 1, 0)) %>%
  filter(experimental == 0) -> dat1a
  # remove seymour canal swan cove pots, these are only pot #'s greater than 54 in 2015.

##### add columns used later ----------------------------
dat1a %>%
  mutate(AREA = ifelse(Location.Code == 26 | Location.Code == 42, 'LS',
                       ifelse(Location.Code == 4, 'PS', ifelse(Location.Code == 9, 'EI', 
                            ifelse(Location.Code== 15, 'GB', ifelse(Location.Code == 37, 
                                'PB', ifelse(Location.Code==39, 'SC', 0))))))) ->dat1ab

dat1ab %>%
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
#write.csv(Tdat1, './results/problemstanner3.csv')
# need to STOP here and fix problems with data.  Not sure why some females are showing up as NA....also need to fill in missing 
#  widths and density strata (although this applies to red crab NOT tanner)
##################################################################
##### By Pot ----------------------------------------------------
##################################################################
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, AREA, Pot.No, mod_recruit) %>% # use AREA here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + AREA + Pot.No ~ mod_recruit, sum, drop=TRUE)

#head(dat3)# check to make sure things worked.

# No weighting by strata here for RKCS data due to it being designed for RKC.

##################################################################
##### CPUE last four year -----------------------------------
##################################################################

#dat3 %>%
 # rename(Missing = Var.5, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
# this is neccessary so that current years file (dat5) matches the historic file names

#This version is ready to calculate CPUE for each recruit class
#Calculates a  mean CPUE and SE for each recruit class # not weighted due to lack of tanner specific strata on red crab survey
dat3 %>%
  group_by(AREA, Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_13_16
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_13_16, './results/RKCS_CPUE_13_16_2.csv')

##################################################################
##### Historic file ---------------------------------------
##################################################################
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

#historicdata <- histdat[,1:15] # remove last two column of total crab and 
 # has all data from 2001 to 2015

# need to add 2016 to historicdata file
# Locations in historic file are numbers.  Here I have names, should I change this?
# only 2016 data 
#dat5 %>%
#  filter(Year == 2016) -> dat5_2016 
#CPUE_ALL_YEARS <- rbind(historicdata, dat5_2016)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(dat3, './results/RKCS_perpot_allyears.csv')

##################################################################
###############################
##### Short term trends -------------------------------------
##################################################################
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

head(dat3)
library(tidyr)
dat3 %>%
  filter(Year >=2013) -> dat3 # confirm that is only contains the last 4 years.  This year needs to be changed every year

dat3_long <- gather(dat3, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

dat3_long %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(AREA, mod_recruit) %>%
  do(fit = lm(crab ~ Year, data =.)) -> short_term

short_term %>%
  tidy(fit) -> short_term_slope

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(AREA, mod_recruit, r.squared, p.value)->short_term_out2
short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(AREA, mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, './results/RKCS_shortterm.csv')
ggplot(dat3_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~AREA)
########################
####################

##################################################################
##### Long term trends ---------------------
##################################################################
#compare 2016 CPUE distribution to the long term mean
dat3 %>%
  filter(Year == 2016) ->dat3_2016
#make sure you have a file with only 2016 data
# long term baseline values are different for each area, I guess make a file for each area?
#
# the y = has to be changed for each area but once they are set they are the same from year to year
dat3_2016 %>%
  filter(AREA == "PB") ->long_term_16
t.test(long_term_16$Large.Females, mu = 2.18)
t.test(long_term_16$Pre_Recruit, mu = 1.61)
t.test(long_term_16$Recruit, mu = 1.20)
t.test(long_term_16$Post_Recruit, mu = 0.96)
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


##################################################################
##### Weights from length - weight relatinship--------------------
##################################################################
# Linear model is changed for each area
weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())

#AREA = unique(Tdat1$AREA) #"LS" "PS" "EI" "GB" "PB" "SC"
#slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10)
#coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55)

weight_length <- data.frame(AREA = unique(Tdat1$AREA), slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10),
                            coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55))

# Pybus Bay linear model: exp(3.05*log(length in mm)-8.34)*2.2/1000
glimpse(Tdat1) # raw data for both 2015 and 2016 
Tdat1 %>%
  filter(AREA == "PB") %>%
  mutate(weight_lb = (exp((3.05*log(Length.Millimeters))-8.34))*(2.2/1000)) -> dat1

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













