#K.Palof 
# ADF&G 8-1-16 updated /10-6-16 / 8-18-17/9-25-17
# Areas: RKCS areas for Tanner crab - EXCLUDES north juneau and stephens passage
# includes: Excursion, Seymour Canal, Pybus Bay, Gambier Bay, Peril Strait, and Lynn Sisters
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP, prior to 2016  

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
#font_import()
#loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
source('./code/tanner_rkc_functions.R') # need to create versions of this code to deal with mutiple areas at once.

###
#####Load Data ---------------------------------------------------
###
# change input file and input folder for each
dat <- read.csv("./data/rkc_tanner/red crab survey for Tanner crab CSA.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA
#area <- read.csv("./data/Juneau/Juneau_Barlow_strata_area.csv") 
                #NO area for this data since these are stratified by RKCS area.  
#histdat <- read.csv("./data/Juneau/2Juneau Stratified CPUE 2015_area formula.csv")
                  ## !!!!  In future years this file will be 'JNU_CPUE_ALL' and just get updated with current years data.
#females <- read.csv("./data/Juneau/RKC_11_16_large females_by_pot.csv")
head(dat)
glimpse(dat) # confirm that data was read in correctly.

baseline <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")
biomass <- read.csv("./data/rkc_tanner/biomass_tanner_RKCareas.csv")

###
##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1) -> test1
# come back and redo this analysis once the 2017 data is edited and fixed!!!!!!!!!

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

####
##### Tanner specific manipulations -----------------------------
####Survey areas ONLY 
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
Tdat1 %>% filter(mod_recruit == "Missing")

Tdat1 %>% filter(is.na(mod_recruit))
###
##### By Pot ----------------------------------------------------
####
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, AREA, Pot.No, mod_recruit) %>% # use AREA here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit)) -> dat2 #remove any NAs due to data issues.

dat3 <- dcast(dat2, Year + AREA + Pot.No ~ mod_recruit, sum, drop=TRUE)

#head(dat3)# check to make sure things worked.

# No weighting by strata here for RKCS data due to it being designed for RKC.

####
##### CPUE for all years ----------------------------------

#This version is ready to calculate CPUE for each recruit class
#Calculates a  mean CPUE and SE for each recruit class # not weighted due to lack of tanner specific strata on red crab survey
dat3 %>%
  group_by(AREA, Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_all
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_all, './results/RKCS_tanner/RKCS_CPUE_all.csv')

###
##### Historic file ---------------------------------------
###
#brought in all the years - 2013 to 2016 - needed at once from OceanAK in the future can do this or add current
#   year to this file.
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(dat3, './results/RKCS_tanner/RKCS_perpot_allyears.csv')


###
##### Short term trends -------------------------------------
###
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

head(dat3)
dat3 %>%
  filter(Year >=2014) %>% 
  select( -Missing) -> dat3a # confirm that is only contains the last 4 years.  This year needs to be changed every year
#remove Missing and NA columns

short_t_tanner(dat3a, 2017)

dat3a_long <- gather(dat3a, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

ggplot(dat3a_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~AREA)
###

###
##### Long term trends ---------------------
###
#compare 2017 CPUE distribution to the long term mean - need to create or have file with long term means.
dat3 %>%
  filter(Year == 2017) ->dat3_2017
baseline
#make sure you have a file with only current years data 

# create a function to run the t.test and loop it over each area
#test1 <- long_ttest('EI', 2017, baseline = baseline, bypot = dat3)

# long_ttest function does not need editing each year, BUT the long_loop_17 
#     function needs to be edited with the current year and confirmed that the 
#     input files are the same names.

areas <- c('PB', 'EI', 'LS', 'GB', 'SC', 'PS')

long_term <- lapply(areas, long_loop_17)
write.csv(long_term, './results/RKCS_tanner/long_term.csv')

##### Weights from length - weight relatinship--------------------

# Linear model is changed for each area
weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())

#AREA = unique(Tdat1$AREA) #"LS" "PS" "EI" "GB" "PB" "SC"
#slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10)
#coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55)

weight_length <- data.frame(AREA = unique(Tdat1$AREA), slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10),
                            coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55))

glimpse(Tdat1) # raw data for both 2015 and 2016 
Tdat1 %>%
  right_join(weight_length) %>%
  mutate(weight_lb = (exp((slope*log(Width.Millimeters)) - coeff ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")

datWL %>% 
  group_by(AREA, Year) %>% 
  filter(Sex.Code == 1) %>% 
  summarise(mature_lbs = wt.mean(weight_lb[mod_recruit %in% Mature], 
                                 Number.Of.Specimens[mod_recruit %in% Mature]), 
            legal_lbs = wt.mean(weight_lb[mod_recruit %in% Legal], 
                                Number.Of.Specimens[mod_recruit %in% Legal]), 
            prer_lbs = wt.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                               Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights

write.csv(male_weights, './results/RKCS_tanner/RKCS_weights.csv')


##### mid-date survey-------------
glimpse(Tdat1)
# need just the date of Time.Set and then to get the mid-date
#Tdat1 %>%
# mutate(time.set = as.POSIXlt(Time.Set)) -> Tdat1

##### Females - large or mature females --------------------------

# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_Tdat1[is.na(LgF_Tdat1$Egg.Percent),]
# change to 0 only if followed by a Egg.Development.Code and Egg.Condition. Code
LgF_Tdat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent) & Egg.Development.Code > 0,
                             0, Egg.Percent)) -> LgF_Tdat1

LgF_Tdat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_Tdat1 # where 1 is yes and 2 is no

LgF_Tdat1 %>%
  group_by(Year, AREA, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + AREA + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(AREA, Year)%>%
  summarise(Pclutch = mean(var1)*100 , Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, './results/RKCS_tanner/RKCS_percent_low_clutch.csv')

##### egg percentage overall -----------------------------------
####
LgF_Tdat1 %>%
  group_by(Year, AREA, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(AREA, Year)%>%
  summarise(mean = mean(egg_mean), 
            egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch

# add this to the table with percent_low_clutch?
percent_low_clutch %>%
  right_join(percent_clutch) -> female_clutch_info
write.csv(female_clutch_info, './results/RKCS_tanner/RKCS_percent_clutch.csv')


##### Long term females -------------------------
####
glimpse(poorclutch1)
#compare 2017 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == 2017) ->poorclutch1_current
#make sure you have a file with only 2016 data
#calculate the t.test

Fem_long_term <- lapply(areas, Fem_long_loop)
write.csv(Fem_long_term, './results/RKCS_tanner/Female_long_term.csv')
# need to figure out a way to store these results in a better format

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above

head(poorclutch1) # has all years of data from OceanAK

# need to run the regression for each area.
# use function found in source function code
poor_clutch_short(poorclutch1, 2017)

ggplot(poorclutch1, aes(Year, var1))+geom_point() +facet_wrap(~AREA)
###


####
##### input for CSA in R ---------------------------
####













