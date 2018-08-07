# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Figure 2 is regional biomass from CSA estimate - use current year's model

# K.Palof
# katie.palof@alaska.gov
# 08/03/2018

rm(list = ls())# clear workspace - helpful when jumping between files 
# load -----
source('./code/functions.R')

# data -----
cur_yr <- 2018
mr_adjust <- read.csv('./data/redcrab/adj_final_stock_assessment.csv')
fishery.status <- read.csv('./data/redcrab/Juneau/hind_fore_cast_2018.csv') # has fishery status
#                     may want to save this information somewhere else in the future
biomass <- read.csv("./data/redcrab/biomass.csv") 
# file for all locations.  Has legal and mature biomass from current year CSA & harvest
# mr adjustments can be made in the function using mr_adjust file.
exploit_rate <- read.csv("./data/redcrab/table3.csv")

## clean up figure 2-------
# regional biomass
biomass %>% 
  group_by(Year) %>% 
  summarise(legal = sum(legal.biomass), mature = sum(mature.biomass)) %>% 
  as.data.frame() -> regional.b
fishery.status %>% 
  select(Year = year, status) %>% 
  mutate(status = ifelse(status == "PU only", "closed", as.character(status))) -> fishery.status

regional.b %>% 
  left_join(fishery.status) -> regional.b

# baseline
# 1993 - 2007 
regional.b %>% 
  filter(Year >= 1993 & Year <= 2007) %>% 
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("Legal (1993-2007)", "Mature (1993-2007)")) ->reg_baseline

# for graphing
#regional.b %>% 
#  gather(type, pounds, legal:mature, factor_key = TRUE) -> regional.


# Figure 2 regional biomass ---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline, aes(yintercept = pounds, linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "black", "grey60", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "solid", "dashed", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$mature,
                                                na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$mature, na.rm = TRUE) +100000), 
                                 by = 200000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
      legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline, aes(x = st_yr, y = pounds, label = label), 
          hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggsave(paste0('./figures/redcrab/regional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


  



# clean up tables --------
exploit_rate %>% 
  dplyr::select(area, equ.er.adj, avg.inc.hr, alt.equ.hr) %>% 
  mutate(Location = case_when(area == 'pybus' ~ 'Pybus', 
                              area == 'gambier' ~ 'Gambier', 
                              area == 'seymour' ~ 'Seymour', 
                              area == 'peril' ~ 'Peril', 
                              area == 'lynn' ~ 'LynnSisters', 
                              area == 'excursion' ~ 'Excursion', 
                              area == 'juneau' ~ 'Juneau')) %>% 
  dplyr::select(-area) -> equ_rate

mr_adjust %>% 
  select(-X) %>% 
  mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
  select(-area) -> mr_adjust2
  
biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(-harvest) %>% 
  left_join(equ_rate) %>% 
  left_join(mr_adjust2) %>%
  replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  mutate(legal.adj = legal.biomass*weighted_ADJ, 
         mature.adj = mature.biomass*weighted_ADJ) -> biomass_rate

# setup blue king crab and other areas 
bkc <- 0.0106
expasion <- 0.628
biomass_rate %>% 
  summarise(legal = sum(legal.biomass), mature = sum(mature.biomass)) %>% 
  gather(survey.area, surveyed, factor_key = TRUE) %>% 
  mutate(other.areas = surveyed/expasion - surveyed, 
         bkc = surveyed*bkc, 
         total = surveyed + other.areas + bkc) %>% 
  gather(Location, pounds, surveyed:total) %>% 
  cast(Location~survey.area)
  


# Table 3 - bioamss, adj, Equ.er.adj -----------


