# K.Palof    8-29-17
# Figure creation in response to 8-29-17 RKC meeting.
# wants figures for each area with legal and mature biomass.  

# Load packages -------------
library(tidyverse)
library(extrafont)
library(grid)
library(gridExtra)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
#Load data ----------------
biomass <- read.csv("./data/redcrab/biomass.csv")

# Area figures ---------------------
biomass %>% filter(Location == "Excursion") %>% 
  select(Year, legal.biomass, harvest, mature.biomass) ->ei.biomass
ei.biomass_long <- gather(ei.biomass, type, pounds, legal.biomass:mature.biomass, factor_key = TRUE)

ggplot(ei.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1, 4))+
  
  ylim(0,200000) +ggtitle("") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.6)) + 
  geom_hline(yintercept = 83351, color = "grey1")+
  geom_hline(yintercept = 56513, color = "grey62", linetype = "dashed")

ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal.biomass))
ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature.biomass))
