# K.Palof
# 7-13-18
# Juneau area RKC forecast / hindcast figure 

# Load packages -------------
library(tidyverse)
library(readxl)
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
#biomass <- read.csv("./data/redcrab/biomass.csv") no record of historic mature biomass point estimates
# in each year so using 2017 model output

hindcast <- read.csv("./data/redcrab/Juneau/hind_fore_cast_2018.csv")

hindcast_long <- gather(hindcast, type, pounds, legal_2018:legal_forecast, factor_key = TRUE)

ggplot(hindcast_long, aes(year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(32,32,8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "blank")) +
  
  ggtitle("Juneau 2018 model with annual forecast") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1978),max(2020), by = 4))
#  theme(legend.position = c(0.8,0.7)) + 
