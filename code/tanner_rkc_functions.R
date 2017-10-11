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
    mutate(recruit.status = c("large.female", "pre.recruit", "recruit", "post.recruit")) %>% 
    mutate( AREA = area) -> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  #write_csv(long_term_results, paste0('results/redcrab/', area, '/longterm.csv'))
  long_term_results 
}

### function to loop long term function above ------------
long_loop_17 <- function(x){
  long_ttest(x, 2017, baseline = baseline, bypot = dat3)
}






