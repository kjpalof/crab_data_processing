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
  write_csv(short_term_results, paste0('results/redcrab/', area, '/ei_shortterm.csv'))
}