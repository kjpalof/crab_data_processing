#k.palof
# code to recalculate weighted CPUE and SD for the juneau area by placing all pots older than 2005 into current (post-2005)
#   strata.  

# step 1: assign old pots strata
# step 2: summarise data as in JNUprocessingCODE.R
# step 3: combine file (from Kellii) with file pulled from OceanAK with all data summarised

### load packages ---------
library(tidyverse)
library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(weights)
library(broom)
#### load data ------------
jnu_lat_long <- read.csv("./data/redcrab/Juneau/RKC_survey_lat_long_all.csv")
area <- read.csv("./data/redcrab/Juneau/Juneau_Barlow_strata_area.csv")
dat <- read.csv("./data/redcrab/Juneau/RKC_survey_CSA_jnu.csv")

### create lat long file for Kellii -----
head(jnu_lat_long)

jnu_lat_long %>% 
  group_by(Year, Project.Code, Trip.No, Location.Code, Location, Pot.No, Latitude.Decimal.Degrees, Longitude.Decimal.Degrees) %>% 
  summarise(n = n()) -> jnu_lat_long_bypot
write.csv(jnu_lat_long_bypot, './results/redcrab/Juneau/jnu_lat_long_bypot.csv', row.names = FALSE)

# file from kellii
strata_jnu_hist <- read.csv("./data/redcrab/Juneau/Strata_RKC_BarlowJuneau_USE.csv")
### merge data with strata codes ---------
strata_jnu_hist %>% 
  select(Strata, Year = Year, Trip.No = Trip_No, Location, Pot.No = Pot_No) -> strata_jnu
dat %>% left_join(strata_jnu) -> dat_a

##### Initial review of new data -------------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat_a$Pot.Condition)
dat_a %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)# this SHOULD produce NO rows. 
dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(!(Recruit.Status == "" & Number.Of.Specimens >= 1))->dat1 # removed rows without lengths to determine recruit class
# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

#### crab data processing ------------
##### By Pot -------------------------------
#Now summarize by pot - remember to keep areas seperate.
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Strata) %>%
  summarise (total_crab = sum(Number.Of.Specimens)) #gets you total crab per pot.

# need Number of Specimens by recruit class
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Strata, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No +Density.Strata.Code +Strata ~ Recruit.Status, sum, drop=TRUE)

head(dat3)

# prior to 2005 want strata, after 2005 want Density.Strata.Code
dat3 %>% 
  mutate(Strata.Code = ifelse(Year <=2004, Strata, Density.Strata.Code)) -> dat3a
# how many do not have strata code due to wrong lat long ???
dat3a %>% filter(is.na(Strata.Code)) # pots removed due to lack of strata - check lat / long on these later
dat3a %>% filter(!is.na(Strata.Code)) -> dat3b
# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
area %>% 
  rename(Strata.Code = Density.Strata.Code)-> area
dat3b %>%
  left_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Strata.Code) %>%
  summarise(npots  = n()) -> pots_per_strata

#####Weighted CPUE all years -----------------------------------
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  left_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  rename(Missing = Var.7, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_JNU

write.csv(CPUE_wt_JNU, './results/redcrab/Juneau/JNU_CPUE_allyears_wtd.csv', row.names = FALSE)


# unweighted CPUE for all years 

dat5 %>% 
  group_by(Year) %>% 
  summarise (Pre_R = mean(Pre_Recruit), PreR_SE = sd(Pre_Recruit), 
             Rec = mean(Recruit), Rec_SE = sd(Recruit), 
             Post_Rec= mean(Post_Recruit), PR_SE = sd(Post_Recruit), 
             Juv = mean(Juvenile), juv_SE = sd(Juvenile), 
             SmallF = mean(Small.Females), SmallF_SE = sd(Small.Females), 
             MatF = mean(Large.Females), MatF_SE = sd(Large.Females)) %>% 
  mutate(legal = Rec + Post_Rec) -> raw_counts

raw_counts %>% gather(recruit.class, value, -Year) ->raw_counts_long

ggplot(raw_counts, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)