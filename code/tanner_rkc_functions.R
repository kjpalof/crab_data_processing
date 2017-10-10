#K.Palof 10-4-17

#Functions for processing Tanner crab data from the red crab survey.  All areas (except NJ) are
#     included in one file so need to add a grouping variable for Area/Location

## load packages -----------
library(tidyverse)
library(weights)
library(broom)

### short term function ----------------
#input is file with last four years of data summarized by pot
# area
# year
short_t_tanner <- function(bypot_st, year) {
  bypot_st_long <- gather(bypot_st, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) 
  
  bypot_st_long %>% 
    group_by(AREA, mod_recruit) %>% 
    do(fit = lm(crab ~ Year, data = .)) ->short_term
  
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
    rename(slope = estimate) %>% 
    select(AREA, mod_recruit, slope) %>%
    right_join(short_term_out2)->short_term_results # estimate here is slope from regression
  
  #Now need to add column for significance and score
  short_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/RKCS_tanner/shortterm.csv'))
}

### Long term function -------------------
# need current years data and file with long term means

long_ttest <- function(area, year, baseline, bypot){
  baseline %>% 
    filter(AREA == area) -> baseline_values
  bypot %>% 
    filter(AREA == area & Year == year) ->data.use
  lfem <- t.test(data.use$Large.Females, mu = baseline_values$Large.Female)
  prer <- t.test(data.use$Pre_Recruit, mu = baseline_values$Pre_Recruit)
  rec <- t.test(data.use$Recruit, mu = baseline_values$Recruit)
  postr <- t.test(data.use$Post_Recruit, mu = baseline_values$Post_Recruit)
  
  long_term <- matrix(nrow = 4, ncol = 3)
  rownames(long_term) <- c("large.female", "pre.recruit", "recruit", "post.recruit")
  colnames(long_term) <- c("mean", "p.value", "lt.mean")
  
  long_term[1,1] <-lfem$estimate
  long_term[1,2] <- lfem$p.value
  long_term[1,3] <- lfem$null.value
  long_term[2,1] <-prer$estimate
  long_term[2,2] <- prer$p.value
  long_term[2,3] <- prer$null.value
  long_term[3,1] <-rec$estimate
  long_term[3,2] <- rec$p.value
  long_term[3,3] <- rec$null.value
  long_term[4,1] <-postr$estimate
  long_term[4,2] <- postr$p.value
  long_term[4,3] <- postr$null.value
  
  long_term_results <- as.data.frame(long_term)
  
  long_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > lt.mean, 1,
                                ifelse(p.value <0.05 & mean < lt.mean, -1, 0))) %>% 
    mutate(recruit.status = c("large.female", "pre.recruit", "recruit", "post.recruit"))-> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  #write_csv(long_term_results, paste0('results/redcrab/', area, '/longterm.csv'))
  long_term_results 
}

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



