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
