#-----------------------------------------------------------------------------#
#          4. Extract daily exposures and aggregate population counts         #
#-----------------------------------------------------------------------------#

pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2027_wildfires/"


# 1. Read data ----

pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
dfnuts <- as.data.frame(pointnuts)
population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
population <- subset(population, names(population) != "year")
names(population) <- as.character(c(2003:2025))


# 2. Create date sequence ----

dayseq <- seq.Date(as.Date("2003-01-01"), as.Date("2025-12-31"), by = "1 day")


# 3. Function to extract geographical IDs, population, SILAM for each day-cell per year and save ----

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

# Clean
rm(list = ls())
