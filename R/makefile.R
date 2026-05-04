#-----------------------------------------------------------------------------#
#             LCD Europe Wildfire Smoke Indicator 2027: Makefile              #
#-----------------------------------------------------------------------------#

library("terra")
library("sf")
library("lubridate")
library("ISOweek")
library("tidyverse")
library("data.table")
library("readxl")
library("rio")



# 1. Yearly population grids (HPC) ----
source("R/1_population.R") 

# 2. NUTS codes ----
source("R/2_nuts.R")

# 3a. Mortality data daily counts----
source("R/3_mortality.R")

# 3b. Mortality data weekly counts ----
source("R/3_mortality_weekly.R")

# 4a. Merge exposures, population, NUTS codes - daily (HPC) ----
source("R/4_assemble.R")

# 4b. Merge exposures, population, NUTS codes - weekly (HPC) ----
source("R/4_assemble_weekly.R")

# 5. Exposure and fire risk subindicator by area (HPC) ----
source("R/5_exposure.R")

# 6a. Attributable mortality subinidicator _ daily HIA (HPC) ----
source("R/6_HIA.R")

# 6b. Attributable mortality subinidicator - weekly HIA (HPC) ----
source("R/6_HIA_weekly.R")

# 6c. Attributable mortality subinidicator (HPC) ----
source("R/6_HIA_sens_lag0-1.R")

# 6d. Attributable mortality subinidicator (HPC) ----
source("R/6_HIA_sens_lag0-7.R")

# 6e. Attributable mortality subinidicator (HPC) ----
source("R/6_HIA_sens_RR_totalpm25.R")

# 7. FWI fire risk subindicator ----
source("R/7_FWI.R")

# 8. Figures and tables ----
source("R/8_figtab.R")
