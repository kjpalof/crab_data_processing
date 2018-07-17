# K.Palof
# 7-13-18
# Juneau area RKC forecast / hindcast figure 

# Load packages -------------
library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(gridExtra)
library(scales)
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

hindcast_long <- gather(hindcast, type, pounds, legal_2018:mature_forecast, factor_key = TRUE)

# Baseline ----
hindcast %>%
  filter(year > 1992 & year < 2008) %>% 
  gather(type, pounds, legal_2018:mature_forecast, factor_key = TRUE) %>%
  group_by(type) %>% 
  summarise(baseline = mean(pounds)) %>% 
  mutate(label = c("Legal (1993-2007)", "Mature (1993-2007)", "Legal (1993-2007)", "Mature (1993-2007)"), 
         start_yr = c(1979, 1979, 1979, 1979)) -> baseline_mean
baseline_mean_18 <- as.data.frame(baseline_mean[1:2,])
baseline_mean_forecast <- as.data.frame(baseline_mean[3:4,])  

# Figure 1 redo ---------
    # should have 2018 model with longterm baselines (1993-2007) and closure status. 
    #   also show 2018 forecast as distinct from model output
jnu_rkc_fig1 <- hindcast %>% 
  rename(legal_lb = legal_2018, mature_lb = mature_2018) %>% 
  select(-legal_forecast, -mature_forecast) %>% 
  gather(type, pounds, legal_lb:mature_lb, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
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
  theme(legend.position = c(0.065,0.80))

jnu_rkc_fig1 +
  geom_text(data = baseline_mean_18, aes(x = start_yr, y = baseline, label = label), 
            hjust = 0.25, vjust = -1, nudge_y = 0.05) 

# Figure A1 ---old Figure 1 - move to Appendix --------
# forecast for each year 
hindcast %>% 
  select(-legal_2018, -mature_2018) %>% 
  gather(type, pounds, legal_forecast:mature_forecast, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  scale_shape_manual(name = "", values = c(0, 8, 2, 4))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,700000),
                     breaks= seq(min(0), max(700000), by = 100000)) +
  
  ggtitle("Juneau annual forecast reported") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5)) +
  geom_hline(yintercept = 317238, color = "grey1")+
  geom_hline(yintercept = 456142, color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.15,0.80))


#  select(year, legal_2018)figure of 2018 model with forecast in each year -----
ggplot(hindcast_long, aes(year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey18", "grey1"))+
  scale_shape_manual(name = "", values = c(32,32,8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "blank")) +
  scale_y_continuous(labels = comma) +
  #ylim(0,700000) +
  ggtitle("Juneau 2018 model with annual forecast") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5))
#  theme(legend.position = c(0.8,0.7)) + 
