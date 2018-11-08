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