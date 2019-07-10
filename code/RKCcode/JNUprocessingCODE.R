# K.Palof 
# katie.palof@alaska.gov
# ADF&G 7-3-19/ updated for JUNEAU area
# code to process data from Ocean AK to use in crab CSA models.    
# This code is for -2019, refer to code for processing after this year.


##Load Packages/functions ---------------------------------
source('./code/functions.R')

## setup year --------
cur_yr <- 2019
pr_yr <- cur_yr -1
survey.location <- 'Juneau'

#####Load Data --------------------------------------
dat <- read.csv("./data/redcrab/Juneau/jnu_18_19_oceanAK_out_RAW.csv")
          # this is input from OceanAK - set up as red crab survey data for CSA
          #   survey area should match that in the name of this script file
          #   Juneau area includes Juneau and Barlow
area <- read.csv("./data/redcrab/Juneau/Juneau_Barlow_strata_area.csv") # same every year

# use JNU_79_XX_bypot.csv created from previous year ** need to change year **
          # set up to read from annual folder.  If all files from last year
          #     are in one folder this should work with just a change in final year
histdat <- read.csv(paste0('./results/redcrab/', survey.location, 
                    '/', pr_yr, '/JNU_perpot_all_', pr_yr,'.csv'))
## !!!!  this file will be 'JNU_perpot_all_pr_yr' and just get updated with current years data.
females <- read.csv(paste0('./results/redcrab/', survey.location, 
                 '/', pr_yr, '/largef_all.csv'))

baseline <- read.csv("./data/redcrab/longterm_means.csv") # same every year
biomass <- read.csv("./data/redcrab/biomass.csv") # ** update ** from CSA model
          # file for all locations. Has biomass estimates from CSA,
          #   must be updated after CSA model is run for current year

##### Initial review -------------------------------------
head(dat)
glimpse(dat) # confirm that data was read in correctly.

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

##### By Pot -------------------------------
# Now summarize by pot - remember to keep areas seperate.
# need Number of Specimens by recruit class
# keep trip no. to merge wwith historic data 
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No + Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

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
  dplyr::rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
 
#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/
                            (sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt

write.csv(CPUE_wt, paste0('./results/redcrab/', survey.location,'/', 
                  cur_yr, '/JNU_CPUE_' , cur_yr, '.csv'), row.names = FALSE)

# weighted cpue by strata --- just for comparison
dat5 %>%
  group_by(Year, Density.Strata.Code) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/
                                                                          (sqrt(sum(!is.na(Small.Females)))))) 


# raw CPUE of legals 6.09
#### survey mid date -----  
#  ** fix **  make this calculated from data not just visual
head(dat)
unique(dat$Time.Hauled)
# need to seperate time hauled to just have data hauled look for mid-date 
dat[1,7] # 6-20
dat[5843,7] # 6-27
# so mid-date would be 24th.
### ***fix *** this needs to be calculated better

##### Historic file ---------------------------------------
# need to add current years pot summary to the historic pot summary file.  
# For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match
head(histdat)
head(dat5)

histdat %>% select(Year, Location, Trip.No, Pot.No, Strata.Code, Missing, 
                   Juvenile, Large.Females, Post_Recruit, Pre_Recruit, 
                   Recruit, Small.Females, Area, npots, inverse_n, 
                   weighting) -> historicdata
dat5 %>% 
  dplyr::rename(Strata.Code = Density.Strata.Code) -> dat6 # masking???

# need to add current year to historicdata file
# only current years
dat6 %>%
  filter(Year == cur_yr) -> dat5_cur_yr
CPUE_ALL_YEARS <- rbind(historicdata, dat5_cur_yr)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, paste0('./results/redcrab/', 
            survey.location, '/', cur_yr, '/JNU_perpot_all_', cur_yr,'.csv'), 
          row.names = FALSE)

##### Short term trends -------------------

#look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >= cur_yr - 3) -> bypot_st # short term file has last 4 years in it

#function creates output file in folder /results/redcrab/'area'
short_t(bypot_st, cur_yr, "Juneau")
# output is saved as shortterm.csv
bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, 
                        factor_key = TRUE) 
ggplot(bypot_st_long, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)


##### Long term trends ---------------------
#compare current year CPUE distribution to the long term mean
dat5_cur_yr
#make sure you have a file with only current years data - created above

long_t(dat5_cur_yr, baseline, cur_yr, 'Juneau', 'Juneau')
# output is saved as longterm.csv


##### Weights from length - weight relatinship.-----------------
# Linear model is changed for each area
# Juneau linear model: exp(3.03*log(length in mm)-7.23)*2.2/1000
glimpse(dat1) # raw data for last 2 years
# slope = 3.03
# intercept = 7.23
# use function found in functions.R code file
weights(dat1, 3.03, 7.23, "Juneau", cur_yr)
# output saved as maleweights.csv

###### Females ----------------------------------------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
# need to change these to 0 if applicable. 
#LgF_dat1 %>%
#  mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1

LgF_dat1 %>% 
  filter(Year == cur_yr) %>% 
  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_curyr

## ** left off here *********************
# develop file for historic data ----
##
##raw_data <- read.csv("./results/redcrab/Juneau/2017/JNU_raw_pots_79_17.csv")
##levels(raw_data$Pot.Condition)
##raw_data %>%
##  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> raw_dat1
##raw_dat1 %>%
##  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> all_LgF_dat1
##all_LgF_dat1[is.na(all_LgF_dat1$Egg.Percent),]
##all_LgF_dat1 %>% 
##  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
##         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
##         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_all

# In future years just load the largef_all.csv file and add current year
head(females)


# want to add 0's for egg percent if egg development code is 3 or 4
#LgF_dat1_all %>% 
#  mutate(Egg.Percent = ifelse((Egg.Development.Code == 3 & 
#                                 Egg.Condition.Code == 4 |Egg.Condition.Code == 5), 
#                              0, Egg.Percent)) -> LgF_dat1_all


largef_all <- rbind(females, LgF_dat1_curyr) # raw female data for all years.
write.csv(largef_all, (paste0('./results/redcrab/', survey.location, '/', cur_yr, '/', 
                       'largef_all.csv')))
##### % poor (<10 %) clutch -----------------------------------

poor_clutch(largef_all, 'Juneau', cur_yr)
# output is saved as poorclutch1_current.csv - which has all pots for 2017
# and poorclutch_summary_all.csv which has the percentage and 
#                                          SD of poor clutches for all years

##### Long term females -------------------------
poorclutch_current <- read.csv(paste0('./results/redcrab/', survey.location,'/', 
                                      cur_yr, '/poorclutch1_current.csv'))
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Juneau', cur_yr)
# output saved as lt_female.csv

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years
poorclutch_all <- read.csv(paste0('./results/redcrab/', survey.location, '/',
                                    cur_yr,'/poorclutch_all.csv'))
#function for short term trends and output saving.
poor_clutch_short(poorclutch_all, 'Juneau', cur_yr)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(largef_all, 'Juneau', cur_yr)
# output saved as egg_percent_mean_all.csv, creates mean and SE egg percentage for all years

### total stock health table -----------------------
total_health('Juneau', cur_yr)
# works as long as all files are saved in folder with area name


#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year, Location) %>%  select(Year, Location, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(sum) -> raw_samp

dat5 %>% 
  group_by(Year) %>% 
  summarise (effective_no_pots=n()) %>% 
  right_join(raw_samp) %>% 
  as.data.frame() -> raw_samp

write.csv(raw_samp, paste0('./results/redcrab/', survey.location, '/', cur_yr, '/raw_sample.csv'))

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
CPUE_wt_all %>% filter(Year >= 1993) -> CPUE_wt_from93

write.csv(CPUE_wt_from93, paste0('results/redcrab/', survey.location, '/', 
                                 cur_yr, '/cpue_wt_all_yrs.csv'), row.names = FALSE)


panel_figure('Juneau', 2018, 'Juneau')

panel_figure('Juneau', 2018, 'Juneau', 1, 0) # panel with all 3 figures
panel_figure('Juneau', 2018, 'Juneau', 2, 0) # male panel
panel_figure('Juneau', 2018, 'Juneau', 3, 0) # female panel

# panel_figure <- function(survey.location, cur_yr, base.location)
# base.location is the location name in the baseline file, can be different

### see 'juneau_hindcast_figure.R' file for other figures for the Juneau document

### presentation figure -----
panel_figure_NC_PRES('Juneau', 2018, 'Juneau', 2, 0)
panel_figure_NC_PRES('Juneau', 2018, 'Juneau', 3, 0)

##### Restrospective Analysis -----------------------------------

CPUE_ALL_YEARS <- read.csv("./results/redcrab/Juneau/JNU_perpot_all_17.csv")
head(CPUE_ALL_YEARS)

CPUE_ALL_YEARS %>% 
  group_by(Year) %>% 
  summarise ( PreR = sum(Pre_Recruit), R = sum(Recruit), PR = sum(Post_Recruit), 
              meanPreR = mean(Pre_Recruit), meanR = mean(Recruit), meanPR = mean(Post_Recruit)) %>% 
  mutate(legal = R + PR) -> raw_counts

CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all
write.csv(CPUE_wt_all, './results/redcrab/Juneau/JNU_CPUE_historical.csv')


####### explore ------------------
#matches JMP but code below is more efficient - uses library SDMTools
#dat5 %>%
# group_by(Year) %>%
#  summarize(Pre_Recruit_wt = (sum(Pre_Recruit * weighting)/ sum(weighting)), Pre_R_SE = std.error(Pre_Recruit),
#            Recruit_wt = sum(Recruit*weighting)/sum(weighting)) # this matches what is being calculated in JMP



