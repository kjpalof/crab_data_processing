#K.Palof 
# ADF&G 11-4-16 / 10-17-17
# Areas: Tanner crab survey areas - includes Holkham, Thomas, Glacier Bay and Icy Strait
# code to process data from Ocean AK to use in crab CSA models.  
#Prior to 2016 this was done in excel then JMP

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
library(grid)
library(gridExtra)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))


#####Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/TCS/TCS data_13_17.csv")
# this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/TCS/TCSstrata_area.csv") 
# brought in all data since 2013 - this was after survey was stratified.  Older data needs to be imported
# from data file and NOT OceanAK since it won't have survey strata designations in OceanAK
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

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - tanner soak time should be between 16-20??? double check this

##### Tanner specific manipulations -----------------------------
###     Survey areas ONLY 
#             confirm that only the four surveys areas are present.
levels(dat1$Location) # 2015 presence of one port camden pot.  remove this.

dat1 %>%
  filter(Location != "Port Camden") -> dat1
# remove SP from here also.  These are in their own file since they now use RKC only data.

### add columns used later 
dat1 %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', 
                              ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
                               !is.na(Width.Millimeters), 'Juvenile', 
                                ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
                                 !is.na(Width.Millimeters),'Pre_Recruit', 
                                  ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                                   !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                                    ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                                     Width.Millimeters >169|(Shell.Condition.Code >3 & 
                                      Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                                       ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                                        ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                                         ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) -> Tdat1

##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class USE mod_recruit here.
Tdat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code,mod_recruit) %>% 
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No + Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  select( -`NA`) %>% #remove NA column.  This is due to some data errors that need to be fixed in the entry
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
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females)))))) -> CPUE_wt_all
# check to confirm previous years CPUEs match
write.csv(CPUE_wt_all, './results/TCS/CPUE_all.csv') # contains last four years of survey data 

### historic file ---------
# eventually need to import data from 1997 to 2013 that has post-strata assignments.
# need this data for biomass and CPUE trend figures to be in R - need to get them out of Sigma Plot

##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years

# function 
head(dat3) # make sure this is the file with each recruit class as a column by year, location and pot no
dat3 %>%
  select (- `NA`) %>% 
  filter(Year >=2013) -> dat3 # confirm that is only contains the last 4 years.  This year needs to be changed every year

short_t_tanner(dat3, 2017)

dat3_long <- gather(dat3, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE)

ggplot(dat3_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~Location)

### just thomas bay Large.Females
dat3_long %>%
  filter(Location == 'Thomas Bay', mod_recruit == 'Large.Females') -> graph1
ggplot(graph1, aes(Year, crab, color = mod_recruit)) + geom_point() +geom_smooth(method = 'lm')
### just holkham bay for recruits
dat3_long %>%
  filter(Location == 'Holkham Bay', mod_recruit == 'Recruit') -> graph1
ggplot(graph1, aes(Year, crab, color = mod_recruit)) + geom_point() +geom_smooth(method ='lm')

##### Long term trends ---------------------
#compare 2016 CPUE distribution to the long term mean, keep Location seperate
# need to use dat5 because the weighting column is needed.
dat5 %>%
  filter(Year == 2016) ->dat5_2016
#make sure you have a file with only 2016 data
# long term baseline values are different for each area, I guess make a file for each area?
#
# the y = has to be changed for each area but once they are set they are the same from year to year
# THIS NEEDS TO BE A WEIGHTED MEAN - see processingCODE.R
dat5_2016 %>%
  filter(Location == "Glacier Bay") ->long_term_16
wtd.t.test(long_term_16$Large.Females, y = 5.77, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Pre_Recruit, y = 5.62, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Recruit, y = 1.37, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Post_Recruit, y = 1.13, weight = long_term_16$weighting, samedata=FALSE)
#
dat5_2016 %>%
  filter(Location == "Icy Strait") ->long_term_16
wtd.t.test(long_term_16$Large.Females, y = 12.21, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Pre_Recruit, y = 13.67, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Recruit, y = 16.08, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Post_Recruit, y = 3.27, weight = long_term_16$weighting, samedata=FALSE)

#
dat5_2016 %>%
  filter(Location == "Thomas Bay") ->long_term_16
wtd.t.test(long_term_16$Large.Females, y = 29.10, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Pre_Recruit, y = 10.85, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Recruit, y = 6.37, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Post_Recruit, y = 3.33, weight = long_term_16$weighting, samedata=FALSE)
#
dat5_2016 %>%
  filter(Location == "Holkham Bay") ->long_term_16
wtd.t.test(long_term_16$Large.Females, y = 3.86, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Pre_Recruit, y = 3.29, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Recruit, y = 2.08, weight = long_term_16$weighting, samedata=FALSE)
wtd.t.test(long_term_16$Post_Recruit, y = 1.24, weight = long_term_16$weighting, samedata=FALSE)

#
##### Weights from length - weight relatinship--------------------
#
# Linear model is changed for each area
weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())

#Location = unique(dat5$Location) #"Glacier Bay" "Holkham Bay" "Icy Strait"  "Thomas Bay" 
#slope = c(3.30, 3.34, 3.29, 3.32) # these are from W-L relationships established for each area
#coeff = c(9.48, 9.73, 9.48, 9.67)

weight_length <- data.frame(Location = unique(dat5$Location), slope = c(3.30, 3.34, 3.29, 3.32),
                            coeff = c(9.48, 9.73, 9.48, 9.67))

# Pybus Bay linear model: exp(3.05*log(length in mm)-8.34)*2.2/1000
glimpse(Tdat1) # raw data for both 2015 and 2016 
Tdat1 %>%
  right_join(weight_length) %>%
  mutate(weight_lb = (exp((slope*log(Width.Millimeters)) - coeff ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")

datWL %>%
  filter(Sex.Code ==1, mod_recruit %in% Mature ) %>%
  group_by (Location, mod_recruit, Year) %>%
  summarise(mean_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> weight_all
weight_all %>%
  filter(mod_recruit == "Pre_Recruit") %>%
  group_by(Location, Year) -> weight_pre
datWL %>%
  filter(Sex.Code ==1, mod_recruit %in% Mature ) %>%
  group_by (Location, Year) %>%
  summarise(mature_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> weight_mature

datWL %>%
  filter(Sex.Code ==1, Recruit.Status %in% Legal)%>%
  group_by(Location, Year) %>%
  summarise(legal_lbs = wt.mean(weight_lb, Number.Of.Specimens)) -> weight_legal
#summarise(mature_lbs = wt.mean(weight_lb, Number.Of.Specimens), legal_lb)

weight_mature %>%
  right_join(weight_legal) %>%
  right_join(weight_pre)  %>%
  select( -mod_recruit) %>%
  rename(pre_recruit_lb = mean_lbs) -> weights_summary
write.csv(weights_summary, './results/TCS/TCS_weights.csv')

####
##### Females - large or mature females --------------------------
####
# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

#make sure this does NOT include immature females
# this is egg_condition_code == 4
LgF_Tdat1 %>%
  filter(Egg.Condition.Code == 4)
##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_Tdat1[is.na(LgF_Tdat1$Egg.Percent),]
# need to change these to 0. 
LgF_Tdat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_Tdat1

LgF_Tdat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_Tdat1 # where 1 is yes and 2 is no

LgF_Tdat1 %>%
  group_by(Year, Location, Pot.No, Less25) %>%
  summarise(no_sum = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Location, Year)%>%
  summarise(Pclutch = mean(var1)*100 , Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, './results/TCS/TCS_precent_low_clutch.csv')
# check to see if these match JMP file

####
##### Long term females -------------------------
####
glimpse(poorclutch1)
#compare 2016 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == 2016) ->poorclutch1_2016
#make sure you have a file with only 2016 data
#calculate the t.test
poorclutch1_2016 %>%
  filter(Location == "Glacier Bay") -> LT_poor
t.test(LT_poor$var1, mu = 0.10)
poorclutch1_2016 %>%
  filter(Location == "Thomas Bay") -> LT_poor
t.test(LT_poor$var1, mu = 0.10)
poorclutch1_2016 %>%
  filter(Location == "Icy Strait") -> LT_poor
t.test(LT_poor$var1, mu = 0.10)
poorclutch1_2016 %>%
  filter(Location == "Holkham Bay") -> LT_poor
t.test(LT_poor$var1, mu = 0.10)

# attemps to do this all at once
poorclutch1_2016 %>%
  summarise_each(funs(t.test(.[Location == 'Glacier Bay'], .[Location == "Thomas Bay"], 
                             .[Location == "Icy Strait"], .[Location == "Holkham Bay"])$p.value), 
                             vars = var1, mu = 0.10)
                             
                        #  (test = t.test(var1, mu = 0.10)) -> clutch_LT
poorclutch1_2016 %>%
  group_by(Location) %>%
  do(tidy(t.test(var1, mu=0.10, data=.))) ->lt_clutch
  
  #filter(Location == 'Glacier Bay')%>%
  #do(t.test(vars1, mu = 0.10))

####
##### Short term females ------------------------
####
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
#After that this should create a file to use in the future
head(poorclutch1) # should have the last 4 years from OceanAK

poorclutch1 %>%
  filter(Year >=2013) -> LgF_short # short term file has last 4 years in it
#output this file as .csv to add to next year
write.csv(LgF_short, './results/TCS/poorclutchfemales_16.csv')

# need to run the regression for each area.
LgF_short %>% 
  group_by(Location) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  tidy(fit) %>% select(Location, estimate) -> one
LgF_short %>% 
  group_by(Location) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  glance(fit) %>% select(Location, r.squared, p.value) ->two
one %>%
  right_join(two) -> F_short_term_results # estimate here is slope from regression

#Now need to add column for significance and score
F_short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> F_short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(F_short_term_results, './results/TCS/TCS_Fem_shortterm.csv')
ggplot(poorclutch1, aes(Year, var1))+geom_point() +facet_wrap(~Location)
###

##### egg percentage overall -----------------------------------
####
LgF_Tdat1 %>%
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Location, Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch
write.csv(percent_clutch, './results/TCS/TCS_percent_clutch.csv')
# these don't match previous calculations in JMP- why ??


