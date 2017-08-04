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

long_t <- function(dat5_current, year, area) {
  
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