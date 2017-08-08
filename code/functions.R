# K.Palof   8-4-17

# Functions for processing of red king crab data
# need to keep area and year 

## load packages -----------
library(tidyverse)
library(weights)
library(broom)

### short term function ----------------
#input is file with last four years of data summarized by pot
      # area
      # year
short_t <- function(bypot_st, year, area) {
  bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, factor_key = TRUE) 
  
  bypot_st_long %>% 
    group_by(recruit.status) %>% 
    do(fit = lm(crab ~ Year, data = ., weights = weighting)) ->short_term
  
  short_term %>%
    tidy(fit) -> short_term_slope
  
  short_term %>%
    glance(fit) ->short_term_out
  
  short_term_out %>%
    select(recruit.status, r.squared, p.value)->short_term_out2
  
  short_term_slope %>%
    select(recruit.status, term,  estimate) %>%
    spread(term, estimate) %>% 
    right_join(short_term_out2)->short_term_results # estimate here is slope from regression
  short_term_results %>% 
    rename(slope = Year) -> short_term_results
  #Now need to add column for significance and score
  short_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/redcrab/', area, '/shortterm.csv'))
}


### Long term function -------------------
# need current years data and file with long term means

long_t <- function(dat5_current, baseline, year, area, location) {
  #baseline <- read.csv("./data/redcrab/longterm_means.csv")
  baseline %>% filter(Location == location)-> baseline_values
  baseline_values_long <- gather(baseline_values, recruit.status, lt_mean, Juvenile:Post_Recruit, factor_key = TRUE)
  #Uses a weighted mean to help calculate the t.test - part of package weights
  # the y = has to be changed for each area but once they are set they are the same from year to year
  
  juv <- wtd.t.test(dat5_current$Juvenile, y = baseline_values$Juvenile, weight = dat5_current$weighting, samedata=FALSE)
  sfem <- wtd.t.test(dat5_current$Small.Females, y = baseline_values$Small.Female, weight = dat5_current$weighting, samedata=FALSE)
  lfem <- wtd.t.test(dat5_current$Large.Females, y = baseline_values$Large.Female, weight = dat5_current$weighting, samedata=FALSE)
  
  prer <- wtd.t.test(dat5_current$Pre_Recruit, y = baseline_values$Pre_Recruit, weight = dat5_current$weighting, samedata=FALSE)
  rec <- wtd.t.test(dat5_current$Recruit, y = baseline_values$Recruit , weight = dat5_current$weighting, samedata=FALSE)
  postr <- wtd.t.test(dat5_current$Post_Recruit, y = baseline_values$Post_Recruit, weight = dat5_current$weighting, samedata=FALSE)
  
  long_term <- matrix(nrow = 6, ncol = 3)
  rownames(long_term) <- c("juv", "small.female", "large.female", "pre.recruit", "recruit", "post.recruit")
  colnames(long_term) <- c("mean", "p.value", "lt.mean")
  
  long_term[1,1] <-juv$additional["Mean"]
  long_term[1,2] <- juv$coefficients["p.value"]
  long_term[2,1] <-sfem$additional["Mean"]
  long_term[2,2] <- sfem$coefficients["p.value"]
  long_term[3,1] <-lfem$additional["Mean"]
  long_term[3,2] <- lfem$coefficients["p.value"]
  long_term[4,1] <-prer$additional["Mean"]
  long_term[4,2] <- prer$coefficients["p.value"]
  long_term[5,1] <-rec$additional["Mean"]
  long_term[5,2] <- rec$coefficients["p.value"]
  long_term[6,1] <-postr$additional["Mean"]
  long_term[6,2] <- postr$coefficients["p.value"]
  
  long_term[1:6,3] <- baseline_values_long$lt_mean
  
  long_term_results <- as.data.frame(long_term)
  
  long_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > lt.mean, 1,
                                ifelse(p.value <0.05 & mean < lt.mean, -1, 0))) %>% 
    mutate(recruit.status = baseline_values_long$recruit.status) -> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  write_csv(long_term_results, paste0('results/redcrab/', area, '/longterm.csv'))
  
}



### weights from weight -length relationship ------------
weights <- function(dat1, slope, intercept, area){
  dat1 %>%
    mutate(weight_lb = (exp((slope*log(Length.Millimeters))-intercept))*(2.2/1000)) -> dat1
  
  Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
  Legal =c("Recruit", "Post_Recruit")
  # summary of weights all together - would like these in one calc and one summary table
  dat1 %>% 
    group_by(Year) %>% 
    filter(Sex.Code == 1) %>% 
    summarise(mature_lbs = wt.mean(weight_lb[Recruit.Status %in% Mature], 
                                   Number.Of.Specimens[Recruit.Status %in% Mature]), 
              legal_lbs = wt.mean(weight_lb[Recruit.Status %in% Legal], 
                                  Number.Of.Specimens[Recruit.Status %in% Legal]), 
              prer_lbs = wt.mean(weight_lb[Recruit.Status == "Pre_Recruit"], 
                                 Number.Of.Specimens[Recruit.Status == "Pre_Recruit"])) -> male_weights
  # final results with score - save here
  write_csv(male_weights, paste0('results/redcrab/', area, '/maleweights.csv'))
}


### female percent poor clutch ---------

poor_clutch <- function (LgF_dat1, area, year){
# large or mature females
# % poor clutch - less than 10%
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
  summarise(Pclutch = mean(var1) , Pclutch.se = (sd(var1))/sqrt(sum(!is.na(var1)))) -> poorclutch_17

poorclutch1 %>% filter(Year == year) -> poorclutch1_current
write_csv(poorclutch1_current, paste0('results/redcrab/', area, '/poorclutch1_current.csv'))
write_csv(poorclutch_17, paste0('results/redcrab/', area, '/poorclutch_17.csv'))

}

poor_clutch_long <- function(poorclutch_current, area, year){
  lt_female <- t.test(poorclutch_current$var1, mu = 0.10)
  
  longt_female <- matrix(nrow = 1, ncol = 2)
  rownames(longt_female) <- c("large.female")
  colnames(longt_female) <- c("mean", "p.value")
  
  longt_female[1,1] <-mean(poorclutch_current$var1)
  longt_female[1,2] <- lt_female$p.value
  
  longt_female <- as.data.frame(longt_female)
  longt_female %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > 0.10, -1,
                                ifelse(p.value <0.05 & mean < 0.10, 1, 0))) %>% 
    mutate(recruit.status = c("large.female")) -> longt_female #estimate is slope from regression
  
  write_csv(longt_female, paste0('results/redcrab/', area,'/lt_female.csv'))
}

poor_clutch_short <- function(females_all, area, year){
  females_all %>%
    filter(Year >= (year-3)) -> LgF_short # short term file has last 4 years in it
  #output this file as .csv to add to next year
  write_csv(females_all, paste0('results/redcrab/', area,'/poorclutch_females_all.csv'))
  
  LgF_short %>% 
    mutate(per_poorclt = var1)  -> LgF_short
  
  plot(LgF_short$Year, LgF_short$per_poorclt)
  LgF_fit <-lm(per_poorclt ~ Year, data = LgF_short)
  #abline(LgF_fit, col= 'red')
  #summary(LgF_fit)
  
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
  
  write.csv(shortt_female, paste0('results/redcrab/', area,'/short_female.csv'))
}

### females egg percentage ------------
egg_percent <-function(LgF_dat1, area, year){
  LgF_dat1 %>%
    group_by(Year, Location, Pot.No) %>%
    summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot
  
  clutch_by_pot %>%
    group_by(Year)%>%
    summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) -> egg_per_mean
  write_csv(egg_per_mean, paste0('results/redcrab/', area,'/egg_percent_mean.csv'))
  
}


### total stock health table --------------
total_health <- function(area, year){
  longterm <- read_csv(paste0('results/redcrab/', area, '/longterm.csv'))
  shortterm <- read_csv(paste0('results/redcrab/', area, '/shortterm.csv'))
  lt_female <- read_csv(paste0('results/redcrab/', area, '/lt_female.csv'))
  short_female <- read_csv(paste0('results/redcrab/', area, '/short_female.csv'))

total_health <- sum(longterm$significant, shortterm$score, 
                    lt_female$significant, short_female$score) # long term scores CPUE
# short term scores CPUE
# need females poorclutch short and long term
stock_health <- matrix(nrow = 1, ncol = 2)
rownames(stock_health) <- c("area")
colnames(stock_health) <- c("location","score_f")

stock_health[1,1] <- "area"
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
write_csv(stock_health, paste0('results/redcrab/', area,'/stock_health.csv'))
}
