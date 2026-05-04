#-----------------------------------------------------------------------------#
#             LCD Europe Wildfire Smoke Indicator 2027: Makefile              #
#-----------------------------------------------------------------------------#

library("terra")
library("sf")
library("lubridate")
library("ISOweek")
library("tidyverse")
library("data.table")
library("rio")


# 1. Yearly population grids (HPC) ----
source("R/1_population.R") 

# 2. NUTS codes (HPC) ----
source("R/2_nuts.R")

# 3. Mortality data daily counts (HPC) ----
source("R/3_mortality.R")

# 4. Merge exposures, population, NUTS codes - daily (HPC) ----
source("R/4_assemble.R")

# 5. Exposure and fire risk subindicator by area (HPC) ----
source("R/5_exposure.R")

# 6. Attributable mortality subinidicator _ daily HIA (HPC) ----
source("R/6_HIA.R")

# 7. FWI fire risk subindicator (HPC) ----
source("R/7_FWI.R")

# 8. Figures and tables ----
source("R/8_figtab.R")
