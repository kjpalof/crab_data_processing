# Process Tanner crab survey historic data - pre stratification - to use with current data - 2013 +
#       Strata were assigned to these pots after the fact to allow for a calculation of weighted stratified CPUE
#         that could be used to compare CPUEs over the entire time series. Current processing code 
#         'TCS_processingCODE.R' includes 2013 to current year.  This file will be added to the current years output

# K.Palof
# 11-5-18

# Load -------
source('./code/tanner_functions.R')


# Data ----
hbdat <- read_excel('./data/TCS/historic_data_with_new_strata/HB_79_12_ONLY TCS_raw_W strata_USE.xlsx',
                    sheet = "raw data all years")
glbdat <- read_excel('./data/TCS/historic_data_with_new_strata/99 to 13_GLB_TCS_raw bio data_with strata.xlsx',
                     sheet = "99 to 13_GLB")
tbdat <- read_excel('./data/TCS/historic_data_with_new_strata/01_12_Thomas_TCS_raw.xlsx',
                    sheet = "01_13_TB")
isdat <- read_excel('./data/TCS/historic_data_with_new_strata/97_12_icy_tcs_raw with strata.xlsx',
                    sheet = "97_13_icy_tcs")
