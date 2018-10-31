#K.Palof 10-20-17

# Functions for processing Tanner crab data from the Tanner crab survey.  
#   There are four areas, all in one input file. 

## load packages -----------
library(tidyverse)
library(weights)
library(broom)

library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(grid)
library(gridExtra)
library(FNGr)
library(scales)
library(cowplot)

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

### short term function ----------------
#input is file with last four years of data summarized by pot
# area
# year
short_t_tanner <- function(bypot_st, year) {
  bypot_st_long <- gather(bypot_st, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) 
  
  bypot_st_long %>% 
    group_by(Location, mod_recruit) %>% 
    do(fit = lm(crab ~ Year, data = .)) ->short_term
  
  short_term %>%
    tidy(fit) -> short_term_slope
  
  short_term %>%
    glance(fit) ->short_term_out
  
  recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
  short_term_out %>%
    filter(mod_recruit %in% recruit_used) %>%
    select(Location, mod_recruit, r.squared, p.value)->short_term_out2
  
  short_term_slope %>%
    filter(mod_recruit %in% recruit_used, term == 'Year') %>%
    rename(slope = estimate) %>% 
    select(Location, mod_recruit, slope) %>%
    right_join(short_term_out2)->short_term_results # estimate here is slope from regression
  
  #Now need to add column for significance and score
  short_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/TCS/', year,'/shortterm.csv'))
}


### Long term function -------------------
# need current years data and file with long term means

long_ttest <- function(area, year, baseline, bypot){
  baseline %>% 
    filter(Location == area) -> baseline_values
  baseline_values_long <- gather(baseline_values, recruit.status, lt_mean, Large.Female:Post_Recruit, factor_key = TRUE)
  bypot %>% 
    filter(Location == area & Year == year) ->data.use
  lfem <- wtd.t.test(data.use$Large.Females, y = baseline_values$Large.Female, weight = data.use$weighting, samedata=FALSE)
  prer <- wtd.t.test(data.use$Pre_Recruit, y = baseline_values$Pre_Recruit, weight = data.use$weighting, samedata=FALSE)
  rec <- wtd.t.test(data.use$Recruit, y = baseline_values$Recruit, weight = data.use$weighting, samedata=FALSE)
  postr <- wtd.t.test(data.use$Post_Recruit, y = baseline_values$Post_Recruit, weight = data.use$weighting, samedata=FALSE)
  
  long_term <- matrix(nrow = 4, ncol = 3)
  rownames(long_term) <- c("large.female", "pre.recruit", "recruit", "post.recruit")
  colnames(long_term) <- c("mean", "p.value", "lt.mean")
  
  long_term[1,1] <-lfem$additional["Mean"]
  long_term[1,2] <- lfem$coefficients["p.value"]
  long_term[2,1] <-prer$additional["Mean"]
  long_term[2,2] <- prer$coefficients["p.value"]
  long_term[3,1] <-rec$additional["Mean"]
  long_term[3,2] <- rec$coefficients["p.value"]
  long_term[4,1] <-postr$additional["Mean"]
  long_term[4,2] <- postr$coefficients["p.value"]
 
  long_term[1:4, 3] <- baseline_values_long$lt_mean
  long_term_results <- as.data.frame(long_term)
  
  long_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > lt.mean, 1,
                                ifelse(p.value <0.05 & mean < lt.mean, -1, 0))) %>% 
    mutate(recruit.status = c("large.female", "pre.recruit", "recruit", "post.recruit")) %>% 
    mutate( Location = area) -> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  #write_csv(long_term_results, paste0('results/redcrab/', area, '/longterm.csv'))
  long_term_results 
}

### function to loop long term function above ------------
long_loop_17 <- function(x, curyr){
  long_ttest(x, curyr, baseline = baseline, bypot = dat5)
}
