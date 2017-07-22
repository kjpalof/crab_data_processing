#k.palof
# code to recalculate weighted CPUE and SD for the Excursion Inlet area by placing all pots older than 2005 into current (post-2005)
#   strata.  

# step 1: assign old pots strata (kellii did this)
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
area <- read.csv("./data/redcrab/Excursion/Excursion_strata_area.csv")
dat <- read.csv("./data/redcrab/Excursion/RKC survey_historicpots_ei.csv")# data up to 2016

### merge data with strata codes ---------
# file from kellii
strata_EI_hist <- read.csv("./data/redcrab/Excursion/ExcursionStrataRKC.csv")
strata_EI_hist %>% 
  select(Strata = GRIDCODE, Year = Year, Trip.No = TripNo, Location, Pot.No = PotNo) -> strata_ei
dat %>% left_join(strata_ei) -> dat_a

##### Initial review of new data -------------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat_a$Pot.Condition)
dat_a %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)# this SHOULD produce NO rows. 
#dat1 %>%
#  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
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

# save dat5 file for long term file. 
write.csv(dat5, './results/redcrab/Excursion/EI_79_16_bypot.csv', row.names = FALSE)
#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_EI

write.csv(CPUE_wt_EI, './results/redcrab/Excursion/EI_CPUE_allyears_wtd.csv', row.names = FALSE)

CPUE_wt_JNU %>% 
  select(Year, Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, Juvenile_wt, SmallF_wt, MatF_wt) ->CPUE_graph
CPUE_graph %>% gather(recruit.class, value, -Year) ->CPUE_graph_long

# calculate baseline values here???  1993 to 2007.

ggplot(CPUE_graph_long, aes(Year,value)) +geom_point() +facet_wrap(~recruit.class, scales = "free_y")


#### unweighted CPUE for all years ----

dat5 %>% 
  group_by(Year) %>% 
  summarise (Pre_R = mean(Pre_Recruit), Rec = mean(Recruit), 
             Post_Rec= mean(Post_Recruit),  
             Juv = mean(Juvenile),  
             SmallF = mean(Small.Females),  
             MatF = mean(Large.Females)) -> raw_cpue
dat5 %>% 
  group_by(Year) %>% 
  summarise (PreR_SE = sd(Pre_Recruit), 
             Rec_SE = sd(Recruit), 
             PR_SE = sd(Post_Recruit), 
             juv_SE = sd(Juvenile), 
             SmallF_SE = sd(Small.Females), 
             MatF_SE = sd(Large.Females))  -> raw_se

raw_cpue %>% gather(recruit.class, value, -Year) ->raw_cpue_long

ggplot(raw_cpue_long, aes(Year,value)) +geom_point() +facet_wrap(~recruit.class, scales = "free_y")

dat5 %>% 
  group_by(Year) %>% 
  summarise (Pre_R = mean(Pre_Recruit), PreR_SE = sd(Pre_Recruit), Rec = mean(Recruit), 
             Rec_SE = sd(Recruit),Post_Rec= mean(Post_Recruit),PR_SE = sd(Post_Recruit),  
             Juv = mean(Juvenile),  juv_SE = sd(Juvenile),SmallF = mean(Small.Females),  
             SmallF_SE = sd(Small.Females),MatF = mean(Large.Females), 
             MatF_SE = sd(Large.Females)) -> jnu_raw_cpue


write.csv(jnu_raw_cpue, './results/redcrab/Juneau/JNU_CPUE_allyears_raw.csv', row.names = FALSE)


### recalculation of stock health score 12-15 -----
head(dat5)

##### Long term trends ------
#compare current years CPUE distribution to the long term mean
# use dat5_current year
#### 2013 - 2016 long term --------------
dat5 %>% filter(Year == 2016) -> dat5_current

#make sure you have a file with only 2016 data
#Uses a weighted mean to help calculate the t.test - part of package weights
juv <- wtd.t.test(dat5_current$Juvenile, y = 2.53, weight = dat5_current$weighting, samedata=FALSE)
lfem <- wtd.t.test(dat5_current$Large.Females, y = 4.48, weight = dat5_current$weighting, samedata=FALSE)
postr <- wtd.t.test(dat5_current$Post_Recruit, y = 2.32, weight = dat5_current$weighting, samedata=FALSE)
prer <- wtd.t.test(dat5_current$Pre_Recruit, y = 2.45, weight = dat5_current$weighting, samedata=FALSE)
rec <- wtd.t.test(dat5_current$Recruit, y = 1.85, weight = dat5_current$weighting, samedata=FALSE)
sfem <- wtd.t.test(dat5_current$Small.Females, y = 1.65, weight = dat5_current$weighting, samedata=FALSE)

long_term <- matrix(nrow = 6, ncol = 2)
rownames(long_term) <- c("juv", "large.female", "post.recruit", "pre.recruit", "recruit", "small.female")
colnames(long_term) <- c("mean", "p.value")
#long_term[ , 1] <- c('juv', 'large.female', 'post.recruit', 'pre.recruit', 'recruit', 'small.female')
long_term[1,1] <-juv$additional["Mean"]
long_term[1,2] <- juv$coefficients["p.value"]
long_term[2,1] <-lfem$additional["Mean"]
long_term[2,2] <- lfem$coefficients["p.value"]
long_term[3,1] <-postr$additional["Mean"]
long_term[3,2] <- postr$coefficients["p.value"]
long_term[4,1] <-prer$additional["Mean"]
long_term[4,2] <- prer$coefficients["p.value"]
long_term[5,1] <-rec$additional["Mean"]
long_term[5,2] <- rec$coefficients["p.value"]
long_term[6,1] <-sfem$additional["Mean"]
long_term[6,2] <- sfem$coefficients["p.value"]


baseline <- c(2.53,4.48,2.32,2.45,1.85,1.65)
long_term_results <- cbind(long_term, baseline)
long_term_results <- as.data.frame(long_term_results)

long_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & mean > baseline, 1,
                              ifelse(p.value <0.05 & mean < baseline, -1, 0))) %>% 
  mutate(recruit.status = c("juv", "large.female", "post.recruit", 
                            "pre.recruit", "recruit", "small.female")) -> long_term_results #estimate is slope from regression

# final results with score - save here
write.csv(long_term_results, './results/redcrab/Juneau/matrix_baseline_redo/jnu_longterm_16.csv', row.names = FALSE)


