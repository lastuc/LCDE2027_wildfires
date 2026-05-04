#-----------------------------------------------------------------------------#
#         4a. Extract daily exposures and aggregate population counts         #
#-----------------------------------------------------------------------------#

library(tidyverse)
library(terra)
library(data.table)
library(here)

# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2027_wildfires/"
pathroot <- ""

# 1. Assign areas, exposure by grid of population for each day ----

# Read data
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
dfnuts <- as.data.frame(pointnuts)
population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
population <- subset(population, names(population) != "year")
names(population) <- as.character(c(2003:2025))

# Create date sequence
dayseq <- seq.Date(as.Date("2003-01-01"), as.Date("2025-12-31"), by = "1 day")


# Extract geographical IDs, population, SILAM for each day-cell per year
for(y in 2003:2025){

  print(y)

  # Extract population ----
  ypop <- population[as.character(y)]
  names(ypop) <- "population"
  ypop <- terra::extract(ypop, pointnuts)[-1]

  # Extract SILAM ----
  yindex <- year(dayseq) == y
  ysilam <- rast(paste0(pathroot, "data/raw/SILAM/europeFirePM25-MODIS-2003to2025DayMean.nc4"), 
                 lyrs = yindex)
  names(ysilam) <- as.character(as.Date(time(ysilam)))
  # ysilam <- ysilam*1e+9 # change of units
  ysilam <- terra::extract(ysilam, pointnuts)[-1] 
  
  # Merge, reformat and write----
  ydata <- cbind(dfnuts, ypop, ysilam)
  ydata <- tidyr::pivot_longer(ydata,
                               cols = -c("GRD_ID","NUTS_0","NUTS_2","NUTS_mort",
                                         "country_name","EEA","EEA_subregion",
                                         "region_UNgeo","WB_income","HDI_index",
                                         "population"),
                               names_to = "date", values_to = "pm25")
  readr::write_csv(ydata, paste0(pathroot, "data/processed/assembled/data_", y,".csv"))
  rm("ypop", "yindex", "ysilam", "ydata")
}




# # 2. Aggregate population counts ----
# 
# pop <- function(expofile){
#   
#   expodata <- read_csv(expofile)
#   # expodata <- dat03 %>% 
#     # mutate(eu = ifelse(eu == "Member", "Member","Not EU")) # EU remove category "candidate"
#   expodata <- expodata[c("NUTS_mort","NUTS_0", "EEA_subregion","date","population","pm25")]
#   expodata <- expodata[expodata$population > 0,]
#   
#   setDT(expodata)
#   
#   list(
#     "population_nuts" = expodata[, .(population_sum = sum(population), year = year(date)), by = .(NUTS_mort, date)][, .SD[1], by = .(year, NUTS_mort)][, date := NULL],
#     "population_country" = expodata[, .(population_sum = sum(population), year = year(date)), by = .(NUTS_0, date)][, .SD[1], by = .(year, NUTS_0)][, date := NULL],
#     "population_region" = expodata[, .(population_sum = sum(population), year = year(date)), by = .(EEA_subregion, date)][, .SD[1], by = .(year, EEA_subregion)][, date := NULL],
#     # "population_eu" = expodata[, .(population_sum = sum(population), year = year(date)), by = .(eu, date)][, .SD[1], by = .(year, eu)][, date := NULL],
#     "population_euro" = expodata[, .(population_sum = sum(population), year = year(date)), by = date][, .SD[1], by = year][, date := NULL]
#   )
#   
# }
# 
# # Execute  
# pop03 <- pop(paste0(pathroot, "data/processed/assembled/data_2003.csv"))
# pop04 <- pop(paste0(pathroot, "data/processed/assembled/data_2004.csv"))
# pop05 <- pop(paste0(pathroot, "data/processed/assembled/data_2005.csv"))
# pop06 <- pop(paste0(pathroot, "data/processed/assembled/data_2006.csv"))
# pop07 <- pop(paste0(pathroot, "data/processed/assembled/data_2007.csv"))
# pop08 <- pop(paste0(pathroot, "data/processed/assembled/data_2008.csv"))
# pop09 <- pop(paste0(pathroot, "data/processed/assembled/data_2009.csv"))
# pop10 <- pop(paste0(pathroot, "data/processed/assembled/data_2010.csv"))
# pop11 <- pop(paste0(pathroot, "data/processed/assembled/data_2011.csv"))
# pop12 <- pop(paste0(pathroot, "data/processed/assembled/data_2012.csv"))
# pop13 <- pop(paste0(pathroot, "data/processed/assembled/data_2013.csv"))
# pop14 <- pop(paste0(pathroot, "data/processed/assembled/data_2014.csv"))
# pop15 <- pop(paste0(pathroot, "data/processed/assembled/data_2015.csv"))
# pop16 <- pop(paste0(pathroot, "data/processed/assembled/data_2016.csv"))
# pop17 <- pop(paste0(pathroot, "data/processed/assembled/data_2017.csv"))
# pop18 <- pop(paste0(pathroot, "data/processed/assembled/data_2018.csv"))
# pop19 <- pop(paste0(pathroot, "data/processed/assembled/data_2019.csv"))
# pop20 <- pop(paste0(pathroot, "data/processed/assembled/data_2020.csv"))
# pop21 <- pop(paste0(pathroot, "data/processed/assembled/data_2021.csv"))
# pop22 <- pop(paste0(pathroot, "data/processed/assembled/data_2022.csv"))
# pop23 <- pop(paste0(pathroot, "data/processed/assembled/data_2023.csv"))
# pop24 <- pop(paste0(pathroot, "data/processed/assembled/data_2024.csv"))
# pop25 <- pop(paste0(pathroot, "data/processed/assembled/data_2025.csv"))
# 
# # Population by NUTS2
# attr_nuts <- bind_rows(pop03[[1]], pop04[[1]], pop05[[1]], pop06[[1]], pop07[[1]],
#                        pop08[[1]], pop09[[1]], pop10[[1]], pop11[[1]], pop12[[1]],
#                        pop13[[1]], pop14[[1]], pop15[[1]], pop16[[1]], pop17[[1]],
#                        pop18[[1]], pop19[[1]], pop20[[1]], pop21[[1]], pop22[[1]], 
#                        pop23[[1]], pop24[[1]], pop25[[1]])
# write_csv(attr_nuts, paste0(pathroot, "data/processed/population_nuts.csv"))
# 
# # Population by country
# attr_country <- bind_rows(pop03[[2]], pop04[[2]], pop05[[2]], pop06[[2]], pop07[[2]],
#                           pop08[[2]], pop09[[2]], pop10[[2]], pop11[[2]], pop12[[2]],
#                           pop13[[2]], pop14[[2]], pop15[[2]], pop16[[2]], pop17[[2]],
#                           pop18[[2]], pop19[[2]], pop20[[2]], pop21[[2]], pop22[[2]], 
#                           pop23[[2]], pop24[[2]], pop25[[2]])
# write_csv(attr_country, paste0(pathroot, "data/processed/population_country.csv"))
# 
# # Population by region
# attr_region <- bind_rows(pop03[[3]], pop04[[3]], pop05[[3]], pop06[[3]], pop07[[3]],
#                          pop08[[3]], pop09[[3]], pop10[[3]], pop11[[3]], pop12[[3]],
#                          pop13[[3]], pop14[[3]], pop15[[3]], pop16[[3]], pop17[[3]],
#                          pop18[[3]], pop19[[3]], pop20[[3]], pop21[[3]], pop22[[3]], 
#                          pop23[[3]], pop24[[3]], pop25[[3]])
# write_csv(attr_region, paste0(pathroot, "data/processed/population_region.csv"))
# 
# # # Population by the EU
# # attr_eu <- bind_rows(pop03[[4]], pop04[[4]], pop05[[4]], pop06[[4]], pop07[[4]],
# #                      pop08[[4]], pop09[[4]], pop10[[4]], pop11[[4]], pop12[[4]],
# #                      pop13[[4]], pop14[[4]], pop15[[4]], pop16[[4]], pop17[[4]],
# #                      pop18[[4]], pop19[[4]], pop20[[4]], pop21[[4]], pop22[[4]], 
# #                      pop23[[4]], pop24[[4]], pop25[[4]])
# # write_csv(attr_eu, paste0(pathroot, "data/processed/population_eu.csv"))
# 
# # Population by euro
# attr_euro <- bind_rows(pop03[[4]], pop04[[4]], pop05[[4]], pop06[[4]], pop07[[4]],
#                        pop08[[4]], pop09[[4]], pop10[[4]], pop11[[4]], pop12[[4]],
#                        pop13[[4]], pop14[[4]], pop15[[4]], pop16[[4]], pop17[[4]],
#                        pop18[[4]], pop19[[4]], pop20[[4]], pop21[[4]], pop22[[4]],
#                        pop23[[4]], pop24[[4]], pop25[[4]])
# write_csv(attr_euro, paste0(pathroot, "data/processed/population_euro.csv"))
# 
# # Clean
# rm(list = ls())
