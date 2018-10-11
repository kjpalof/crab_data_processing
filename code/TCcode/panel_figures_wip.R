# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Once these are established they should be made into functions and places in the functions file.

# Panel figures for RKC areas that we assess for Tanner crab. Output should be same as RKC figures but
#   input may vary due to analysis being slightly different (all areas are processed in one file)

# K.Palof
# katie.palof@alaska.gov
# 10-5-18

# load -----
source('./code/tanner_rkc_functions.R')

# data -----
cur_yr <- 2018
survey.location <- 'EI' # survey locations here are codes: EI, PS, PB, GB, SC, LS
area = "Excursion Inlet" # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
     # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
     # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
 
CPUE_wt_graph <- read.csv(paste0('./results/RKCS_tanner/', cur_yr,
                               '/RKCS_CPUE_all.csv'))
poorclutch_summary <- read.csv(paste0('./results/RKCS_tanner/', cur_yr, '/RKCS_percent_low_clutch.csv'))
egg_mean_all <- read.csv(paste0('./results/RKCS_tanner/', cur_yr,
                                '/RKCS_percent_clutch.csv'))
# file with year and mean percent poor clutch and se poor clutch from 1993 to current

baseline <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")
biomass <- read.csv("./data/rkc_tanner/tanner_2018_biomassmodel.csv") 
harvest <- read.csv("./results/tanner/tanner_comm_catch_98_2018.csv")
# file for all locations.  Has legal and mature biomass from current year CSA 
# harvest comes from processing in 'tanner_harvest.R' - need to fix some survey areas using logbooks look at notes in that file.

# prep data ------
### Mature males-----
#create data frame that has mature males - just means
# data fame that has mature males - just SE
CPUE_wt_graph %>% 
  filter(AREA == survey.location) %>% 
  select(Year,Pre_Recruit_u, Recruit_u, Post_Recruit_u, 
                         PreR_SE, Rec_SE, PR_SE) -> males
males_long <- gather(males, recruit.status, value1, Pre_Recruit_u:PR_SE, factor_key = TRUE)
males_long %>% 
  mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_u",
                             "pre.recruit", ifelse(recruit.status == "Recruit_u", 
                                "recruit", ifelse(recruit.status == "PreR_SE", 
                                "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                "recruit", "post.recruit "))))) %>% 
  mutate(type = ifelse(recruit.status == "PreR_SE",
                      "se", 
                       ifelse(recruit.status == "Rec_SE", 
                              "se", ifelse(recruit.status == "PR_SE", 
                                           "se", "mean"))))-> males_long
males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph

### females/juv prep ------------
CPUE_wt_graph %>% 
  filter(AREA == survey.location) %>% 
  select(Year,Juvenile_u, SmallF_u, MatF_u, 
                         Juv_SE, SmallF_SE, MatF_SE) -> femjuv
femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_u:MatF_SE, factor_key = TRUE)
femjuv_long %>% 
  mutate(recruit.class = ifelse(recruit.status == "Juvenile_u", "juvenile.male", 
                            ifelse(recruit.status == "SmallF_u", 
                             "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                              "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                "juvenile.female", "mature.female"))))) %>% 
  mutate(type = ifelse(recruit.status == "Juv_SE",
                       "se", 
                       ifelse(recruit.status == "SmallF_SE", 
                              "se", ifelse(recruit.status == "MatF_SE", 
                                           "se", "mean"))))-> femjuv_long
femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph

### baseline cpue values ----
baseline %>% 
  filter(AREA == survey.location) ->baseline2

## poor clutch --------
poorclutch_summary %>% # this data is coming in as a percentage not a ratio
  filter(AREA == survey.location) %>% 
  select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
## mean egg percent -------
egg_mean_all %>% 
  filter(AREA == survey.location) %>% 
  select(Year, mean, egg.se) -> egg_mean_all_a
## female egg data -------
# combine these data sets for graphing.  Create one with means and one with SEs.
poorclutch_summary_a %>% 
  left_join(egg_mean_all_a) -> female_egg
female_egg_long <- gather(female_egg, vname, value1, Pclutch:egg.se, factor_key = TRUE)
female_egg_long %>% 
  mutate(female.egg = ifelse(vname == "Pclutch",
                             "% poor clutch", 
                             ifelse(vname == "mean", 
                                    "total % clutch", ifelse(vname == "Pclutch.se", 
                                                             "% poor clutch", "total % clutch")))) %>% 
  mutate(type = ifelse(vname == "Pclutch.se", "se", ifelse(vname == "egg.se", 
                                                              "se", "mean"))) %>% 
  select (-vname) %>% 
  spread(type, value1) -> female_egg_graph

## biomass manipulations --------
# file for all locations.  Has preR, legal, and mature biomass from CSAs
harvest %>% 
  select(Year, Area = survey.area, pounds) ->harvest_a
  
biomass %>% 
  left_join(harvest_a) %>% 
  select(Year, Area, harvest = pounds, Legal, Mature) %>% 
  gather(type, pounds, harvest:Mature, factor_key = TRUE) %>% 
  filter(Area == area) -> biomass_graph

biomass_graph %>% 
  filter(Year <= 2007) %>% 
  spread(type, pounds) %>% 
  summarise(legal_mean = mean(legal.biomass), legal_adj_mean = mean(adj.legal)) -> baseline_means

# Figure panel -----
#### F1a mature male plot -----------
p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class))+ 
  geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
  geom_line(aes(color = recruit.class, group = recruit.class))+
  scale_colour_manual(name = "", values = c("grey1", "grey62", "grey34"))+
  scale_shape_manual(name = "", values = c(15, 16, 17))+
  
  ylim(0,7) +ggtitle(survey.location) + ylab("CPUE (number/pot)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                width =.4) +
  geom_hline(yintercept = baseline2$Pre_Recruit, color = "grey62")+
  geom_hline(yintercept = baseline2$Recruit, color = "grey34")+
  geom_hline(yintercept = baseline2$Post_Recruit, color = "black")+
  theme(legend.position = c(0.7,0.8))

### F1b females/juvenile plot ---------------
p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class))+ 
  geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
  geom_line(aes(color = recruit.class, group = recruit.class))+
  scale_colour_manual(name = "", values = c("grey34","grey62", "grey1"))+
  scale_shape_manual(name = "", values = c(17, 16, 15))+
  
  ylim(0,25) +ggtitle("") + ylab("CPUE (number/pot)")+ xlab("")+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                width =.4) +
  geom_hline(yintercept = baseline2$Juvenile, color = "grey62")+
  geom_hline(yintercept = baseline2$Small.Female, color = "grey34")+
  geom_hline(yintercept = baseline2$Large.Female, color = "black")+
  theme(legend.position = c(0.7,0.8))


#### F1c Female eggs graph -----------
p3 <- ggplot(female_egg_graph, aes(Year, mean)) + 
  geom_point(aes(color = female.egg, shape = female.egg), size =3) +
  geom_line(aes(color = female.egg)) +
  scale_colour_manual(name = "", values = c("grey1", "black")) +
  scale_shape_manual(name = "", values = c(16, 1)) +
  #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
  ylim(0,100) + 
  ggtitle("") + 
  ylab("Percentage") + 
  xlab("") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                width =.4) +
  theme(legend.position = c(0.2,0.5)) 

### biomass harvest graph --------------
p4 <- ggplot(biomass_graph, aes(Year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("grey1", "grey1", "grey48", "grey62"))+
  scale_shape_manual(name = "", values = c(1, 18, 32, 18))+
  scale_linetype_manual(name = "", values = c("blank", "solid", "solid", "dashed")) +
  ggtitle("") + 
  ylab("Legal biomass (lbs)") + 
  xlab("Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,200000),
                     breaks= seq(min(0), max(200000), by = 50000)) +
  theme(legend.position = c(0.7,0.8)) + 
  geom_hline(data = baseline_means, aes(yintercept = legal_mean), color = "grey1")+
  geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean), color = "grey62", linetype = "dashed")


### FINAL plot -------------
png(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), res= 600, width = 8, height =11, units = "in")
#grid.arrange(p1, p2, p3, p4, ncol = 1)
plot_grid(p1, p2, p3, p4, ncol = 1, align = 'vh')
dev.off()



