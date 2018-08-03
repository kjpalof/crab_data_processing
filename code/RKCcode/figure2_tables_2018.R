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
fishery.status <- read.csv(.'/data/redcrab/Juneau/hind_fore_cast_2018.csv') # has fishery status
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


# Figure 2 regional biomass ---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output


ggplot(biomass, aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  scale_shape_manual(name = "Fishery Status", values = c(0, 16, 2, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,700000),
                     breaks= seq(min(0), max(700000), by = 100000)) +
  
  ggtitle("Juneau 2018 model") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
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