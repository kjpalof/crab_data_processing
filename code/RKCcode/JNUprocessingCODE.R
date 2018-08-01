# K.Palof 
# katie.palof@alaska.gov
# ADF&G 7-9-18/ updated for JUNEAU area
# code to process data from Ocean AK to use in crab CSA models.  Currently this is done in excel then JMP.  
# This code is for -2018, refer to code for processing after this year.

rm(list = ls())# clear workspace from previous area 
##Load Packages/functions ---------------------------------
source('./code/functions.R')

## setup year --------
cur_yr <- 2018
pr_yr <- cur_yr -1
survey.location <- 'Juneau'

#####Load Data --------------------------------------
dat <- read.csv("./data/redcrab/Juneau/jnu_17_18_oceanAK_out_RAW.csv")
          # this is input from OceanAK - set up as red crab survey data for CSA
          #   survey area should match that in the name of this script file
          #   Juneau area includes Juneau and Barlow
area <- read.csv("./data/redcrab/Juneau/Juneau_Barlow_strata_area.csv") 
          # same every year
# use JNU_79_XX_bypot.csv created from previous year ** need to change year **
          # set up to read from annual folder.  If all files from last year
          #     are in one folder this should work with just a change in final year
histdat <- read.csv(paste0('./results/redcrab/', survey.location, 
                    '/', pr_yr, '/JNU_79_17_bypot.csv'))
## !!!!  this file will be 'EI_perpot_all_16' and just get updated with current years data.
females <- read.csv(paste0('./results/redcrab/', survey.location, 
                     '/', pr_yr, '/females_all.csv'))

baseline <- read.csv("./data/redcrab/longterm_means.csv")
biomass <- read.csv("./data/redcrab/biomass.csv")
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
  rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
 
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
dat5 %>% rename(Strata.Code = Density.Strata.Code) -> dat6

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
  summarise(Pclutch = mean(var1) , Pclutch.se = (sd(var1))/sqrt(sum(!is.na(var1)))) -> poorclutch_18
write.csv(poorclutch_18, './results/redcrab/Juneau/poorclutch_18.csv', row.names = FALSE)
# check to see if these match JMP file

##### Long term females -------------------------
glimpse(poorclutch1)
#compare 2016 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == 2018) ->poorclutch1_current
#make sure you have a file with only 2016 data
#calculate the t.test - part of package weights
lt_female <- t.test(poorclutch1_current$var1, mu = 0.10)

longt_female <- matrix(nrow = 1, ncol = 2)
rownames(longt_female) <- c("large.female")
colnames(longt_female) <- c("mean", "p.value")

longt_female[1,1] <-mean(poorclutch1_current$var1)
longt_female[1,2] <- lt_female$p.value

longt_female <- as.data.frame(longt_female)
longt_female %>%
  mutate(significant = ifelse(p.value < 0.05 & mean > 0.10, -1,
                              ifelse(p.value <0.05 & mean < 0.10, 1, 0))) %>% 
  mutate(recruit.status = c("large.female")) -> longt_female #estimate is slope from regression

write.csv(longt_female, './results/redcrab/Juneau/lt_female.csv', row.names = FALSE)

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# How to take weights into account here?
# this should only have to be done the first time.  
#After that this should create a file to use in the future
# open female input file (.csv) and delete N.rows and Missing columns also change variables names to n, y, var1
females_all <- rbind(females, poorclutch1_current)
# here use females because it already has 2016
#females_all <- females

females_all %>%
  filter(Year >=2015) -> LgF_short # short term file has last 4 years in it
#output this file as .csv to add to next year
write.csv(females_all, './results/redcrab/Juneau/females_all.csv', row.names = FALSE)

LgF_short %>% 
  mutate(per_poorclt = var1)  -> LgF_short

plot(LgF_short$Year, LgF_short$per_poorclt)
LgF_fit <-lm(per_poorclt ~ Year, data = LgF_short)
abline(LgF_fit, col= 'red')
summary(LgF_fit)

shortt_female <- matrix(nrow = 1, ncol = 4)
rownames(shortt_female) <- c("large.female")
colnames(shortt_female) <- c("intercept", "slope", "p.value", "r_squared")

shortt_female[1,1:2] <- tidy(LgF_fit)$estimate # extract estimate column which is intercept and slope
shortt_female[1,3] <- glance(LgF_fit)$p.value # extract r.squared, and p.value
shortt_female[1,4] <- glance(LgF_fit)$r.squared # extract r.squared, and p.value
shortt_female <- as.data.frame(shortt_female)
#Now need to add column for significance and score
shortt_female %>%
  mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                              ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> shortt_female #estimate is slope from regression
# final results with score - save here

write.csv(shortt_female, './results/redcrab/Juneau/short_female.csv', row.names = FALSE)

##### egg percentage overall -----------------------------------
LgF_dat1 %>%
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->egg_per_mean
  
write.csv(egg_per_mean, './results/redcrab/Juneau/egg_percent_mean.csv', row.names = FALSE)

#### stock health table -----
total_health <- sum(long_term_results$significant, short_term_results$score, 
                    longt_female$significant, shortt_female$score) # long term scores CPUE
# short term scores CPUE
# need females poorclutch short and long term
stock_health <- matrix(nrow = 1, ncol = 2)
rownames(stock_health) <- c("Juneau")
colnames(stock_health) <- c("location","score_f")

stock_health[1,1] <- "juneau"
stock_health[1,2] <- total_health
stock_health <- as.data.frame(stock_health)
stock_health %>% 
  mutate(score = as.numeric(levels(score_f))) -> stock_health
stock_health %>% 
  mutate(health_status = ifelse(score < -4.25, "poor", ifelse(score > -4.25 & score<= -1.75, "below average", 
                                                             ifelse(score > -1.75 & score <= 1.5, "moderate", 
                                                                    ifelse(score > 1.75 & score <= 4.25, "above average", 
                                                                           ifelse(score > 4.25, "healthy", "unknown")))))) %>% 
  mutate (harvest_per = ifelse(health_status == "poor", 0, ifelse(health_status == "below average", 0.05, 
                                                                  ifelse(health_status == "moderate", 0.10, 
                                                                         ifelse(health_status == "above average", 0.15,
                                                                                ifelse(health_status == "healthy", 0.20, "unk")))))) -> stock_health
  #select ( - score_f) -> stock_health
write.csv(stock_health, './results/redcrab/Juneau/stock_health.csv', row.names = FALSE)

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year) %>%  select(Year, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(funs(sum)) -> raw_samp
write.csv(raw_samp, './results/redcrab/Juneau/raw_sample.csv')
dat5 %>% group_by(Year) %>% summarise (n=n())

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



