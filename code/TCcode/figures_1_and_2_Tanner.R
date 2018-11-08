# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Figure 1: regional biomass from CSA estimate - using ONLY survey areas 
#                 - does NOT use current year's estimate.  Uses the estimate reported in the document each year
#                 - to update this file, add most recent year from excel "Tanner Matrix cur_yr.xlsx" 
#                   use pink cells labeled "put this in sigma plot graph"
#             **FIX** yes Ben this is ridiculous, I'm working on creating these values from the actual data
#     Figure 2: Harvest and standardized commercial CPUE (based on the year with the fewest pot lifts - 2008/09)  
#               - pot lifts in that season were 12,521

# K.Palof
# katie.palof@alaska.gov
# 11/07/2018

# load -----
source('./code/tanner_functions.R')

# data -----
cur_yr <- 2018
survey_biomass <- read.csv("./data/TCS/survey_areas_biomass.csv") 

# Figure 1 ------------
survey_biomass %>% 
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(color = type, group = type, linetype = type))+
  geom_point(aes(color = type, shape = type), size =3) +
  scale_colour_manual(name = "", values = c("grey1", "grey48"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  ylab("Biomass (lbs)") + 
  xlab("Survey Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(survey_biomass$Mature, 
                                                      na.rm = TRUE) + 1500000)) +
  theme(legend.position = c(0.55,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"))




ggplot(survey_biomass, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1", "grey48", "grey62"))+
  scale_shape_manual(name = "", values = c(1, 18, 32, 18))+
  scale_linetype_manual(name = "", values = c("blank", "solid", "solid", "dashed")) +
  ylab("Biomass (lbs)") + 
  xlab("Survey Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds, 
                                                      na.rm = TRUE) + 25000),
                     breaks= seq(min(0), max(max(biomass_graph$pounds, 
                                                 na.rm = TRUE)+25000), by = 100000)) +
  theme(legend.position = c(0.55,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold")) + 
  geom_hline(data = baseline_means, aes(yintercept = legal_mean), color = "grey1", 
             linetype = "dashed")