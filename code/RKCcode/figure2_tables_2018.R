# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Figure 2 is regional biomass from CSA estimate - use current year's model

# K.Palof
# katie.palof@alaska.gov
# 08/03/2018

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

## clean up -------
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

# for graphing
regional.b %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) -> regional.


# Figure 2 regional biomass ---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,max(regional.b$mature,
                                                na.rm = TRUE)),
                     breaks= seq(min(0), max(max(regional.b$mature, na.rm = TRUE)), 
                                 by = 100000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) 
  
  #ggtitle("Juneau 2018 model") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5)) +
  geom_hline(yintercept = 298838, color = "grey1")+
  geom_hline(yintercept = 410878, color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.125,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8)) +
  geom_text(data = baseline_mean_18, aes(x = start_yr, y = baseline, label = label), 
            hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  guides(shape = guide_legend(ncol=2), group = guide_legend((ncol =2))) +
  ggsave('./figures/juneau_fig1_2018.png', dpi = 800, width = 7.5, height = 5.5)
