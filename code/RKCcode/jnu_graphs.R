# K.Palof 7-3-17
# attempt to create graphs in R instead of Sigma Plot

### Load packages -----
library(tidyverse)
library(extrafont)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))


#### Load data -----
biomass <- read.csv("./data/redcrab/Juneau/biomass_historic.csv")
cpue <- read.csv("./results/redcrab/Juneau/JNU_CPUE_allyears_wtd.csv")
harvest <- 

### intial graphs -----------

biomass %>% filter(year >= 1993) ->biomass1
ggplot(biomass1, aes(year, legal))+geom_line() +geom_point() +ylim(0,500000)+
  ylab("Legal biomass (lb)")+xlab("Year")+geom_hline(yintercept = 317250)
