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
annual_catch <- read.csv("./results/tanner/tanner_annual_catch_98_2018.csv")
harvest <- read.csv("./data/Tanner_Detailed Fish Tickets_85_18.csv")
std_cpue <- read.csv("C:/Users/kjpalof/Documents/R projects/tanner-crab/results/std_commericial_cpue.csv")
#calculated in a seperate project "tanner-crab"

# Figure 1 ------------
survey_biomass %>% 
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, y = pounds/1000000, group = type)) +
  geom_line(aes(color = type, linetype = type))+
  geom_point(aes(fill = type, shape = type), size =3) +
  scale_fill_manual(name = "", values = c("black", "gray100")) + 
  scale_colour_manual(name = "", values = c("gray1", "grey48"))+
  scale_shape_manual(name = "", values = c(21, 21))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  ylab("Biomass (1,000,000 lbs)") + 
  xlab("Survey Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(survey_biomass$Mature/1000000, 
                                                      na.rm = TRUE) + 1.5), 
                     breaks= seq(min(0), max(max(survey_biomass$Mature/1000000, 
                                                 na.rm = TRUE)+ 1.5), by = 1.0)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

  ggsave(paste0('./figures/tanner/', cur_yr,'_figure1.png'), dpi = 800,
         width = 8, height = 5.75)


# Figure 2 data prep --------------
harvest %>% 
    group_by(Season) %>%
    summarise(permits = length(unique(CFEC)), 
              numbers = sum(Number.Of.Animals, na.rm = TRUE), 
              pounds = sum(Whole.Weight..sum., na.rm = TRUE)) -> annual_harvest
# add year ----
# need a season reference column in terms of years
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
annual_harvest %>% 
  mutate(Year = as.numeric(numextract(Season))+1) -> annual_harvest

annual_harvest %>% 
  select(Year, pounds) %>% 
  filter(Year > 1991) %>% 
  left_join(std_cpue) -> figure2


# Figure 2a ----
ggplot(figure2, aes(x = Year, y = pounds/1000000)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black") +
  ggtitle("Commercial Tanner crab harvest") +
  ylab("Harvest (1,000,000 lbs)") + 
  xlab(NULL) +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1991),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(figure2$pounds/1000000, 
                                                      na.rm = TRUE) + 0.5), 
                     breaks= seq(min(0), max(max(figure2$pounds/1000000, 
                                                 na.rm = TRUE)+ 0.5), by = 0.5)) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2a

# Figure 2b --------------
ggplot(figure2, aes(x = Year, y = avg.cpue)) +
  geom_line(aes(x = Year, y = avg.cpue)) +
  geom_point(aes(x = Year, y = avg.cpue), size =3) +
  geom_errorbar(aes(x = Year, ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), 
              width = 0.2, na.rm = TRUE) +
  expand_limits(y = 0) +
  ylab("CPUE (crab per pot))") + 
  xlab("Fishery Year") +
  scale_x_continuous(breaks = seq(min(1991),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0, 40), 
                     breaks= seq(min(0), max(40), by = 10)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2b#+
  #geom_hline(yintercept = mean(figure2$avg.cpue, na.rm = TRUE)) 


plot_grid(fig2a, fig2b, ncol = 1, align = 'v')
ggsave(paste0('./figures/tanner/', cur_yr,'_figure2.png'), dpi = 800,
       width = 8, height = 9.0)
