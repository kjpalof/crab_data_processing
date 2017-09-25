# K.Palof    8-29-17
# Figure creation in response to 8-29-17 RKC meeting.
# wants figures for each area with legal and mature biomass.  

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

biomass <- read_excel(path = "./data/2017_biomass_model.xlsx")

# Area figures ---------------------
# pybus ---------------
biomass %>% filter(Location == "Pybus") %>% 
  select(Year, legal, mature) ->pb.biomass
pb.biomass_long <- gather(pb.biomass, type, pounds, legal:mature, factor_key = TRUE)

pb <- ggplot(pb.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,325000) +ggtitle("Pybus Bay 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 102618, color = "grey1")+
  geom_hline(yintercept = 129266, color = "grey1", linetype = "dashed")

pb.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
pb.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))
# gambier ------------
biomass %>% filter(Location == "Gambier") %>% 
  select(Year, legal, mature) ->gb.biomass
gb.biomass_long <- gather(gb.biomass, type, pounds, legal:mature, factor_key = TRUE)

gb <- ggplot(gb.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,125000) +ggtitle("Gambier Bay 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 44763, color = "grey1")+
  geom_hline(yintercept = 62684, color = "grey1", linetype = "dashed")

gb.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
gb.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))

# Seymour  -----------
biomass %>% filter(Location == "Seymour") %>% 
  select(Year, legal, mature) ->sc.biomass
sc.biomass_long <- gather(sc.biomass, type, pounds, legal:mature, factor_key = TRUE)

sc <- ggplot(sc.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,600000) +ggtitle("Seymour Canal 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 119837, color = "grey1")+
  geom_hline(yintercept = 147620, color = "grey1", linetype = "dashed")

sc.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
sc.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))
# Peril strait ------------
biomass %>% filter(Location == "Peril") %>% 
  select(Year, legal, mature) ->ps.biomass
ps.biomass_long <- gather(ps.biomass, type, pounds, legal:mature, factor_key = TRUE)

ps <- ggplot(ps.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,175000) +ggtitle("Peril Strait 2017 Model") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 32486, color = "grey1")+
  geom_hline(yintercept = 67031, color = "grey1", linetype = "dashed")

ps.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
ps.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))
#Juneau ----------------
biomass %>% filter(Location == "Juneau") %>% 
  select(Year, legal, mature) ->jn.biomass
jn.biomass_long <- gather(jn.biomass, type, pounds, legal:mature, factor_key = TRUE)

jn <- ggplot(jn.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,700000) +ggtitle("Juneau Area 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 302966, color = "grey1")+
  geom_hline(yintercept = 419518, color = "grey1", linetype = "dashed")

jn.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
jn.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))
# lynn sisters ------------
biomass %>% filter(Location == "Lynn Canal") %>% 
  select(Year, legal, mature) ->lc.biomass
lc.biomass_long <- gather(lc.biomass, type, pounds, legal:mature, factor_key = TRUE)

lc <- ggplot(lc.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,60000) +ggtitle("Lynn Sisters 2017 Model") + ylab("Biomass (lbs)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 14989, color = "grey1")+
  geom_hline(yintercept = 24799, color = "grey1", linetype = "dashed")

lc.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
lc.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))

# excursion ---------------------
biomass %>% filter(Location == "Excursion") %>% 
  select(Year, legal, mature) ->ei.biomass
ei.biomass_long <- gather(ei.biomass, type, pounds, legal:mature, factor_key = TRUE)

ei <- ggplot(ei.biomass_long, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1"))+
  scale_shape_manual(name = "", values = c(16, 1))+
  
  ylim(0,150000) +ggtitle("Excursion Inlet 2017 Model") + ylab("Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(2017), by =2)) +
  theme(legend.position = c(0.8,0.7)) + 
  geom_hline(yintercept = 29095, color = "grey1")+
  geom_hline(yintercept = 56413, color = "grey1", linetype = "dashed")

ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(legal))
ei.biomass %>% filter(Year <= 2007) %>% summarise(mean(mature))


# summarize all together 
png('./results/redcrab/figure_mature.png', res= 300, width = 8, height =11, units = "in")
grid.arrange(pb, gb, sc, ps, ncol = 1)
dev.off()
png('./results/redcrab/figure_mature2.png', res= 300, width = 8, height =8.25, units = "in")
grid.arrange(jn, lc, ei, ncol = 1)
dev.off()
