#-----------------------------------------------------------------------------#
#                     3a. Clean and add weekly mortality data                  #
#-----------------------------------------------------------------------------#


library(sf)
library(terra)
library(dplyr)
library(stringr)
library(readr)
library(rio)
library(here)
library(data.table)
library(tidyr)

# Warning:
# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) available at the NUTS1 level
# No data for Macedonia (MK)
# The rest of the countries are available at NUTS 2 level
# Estonia (EE) mortality data is for NUTS16, but that makes no difference at the
# NUTS2 level.


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2027_wildfires/"
pathroot <- ""


# 1. Clean mortality ----

# Read data
mort <- fread(paste0(pathroot, "data/raw/mortality/estat_demo_r_mwk2_ts.tsv"))
# Take years 2003-2024
mort <- dplyr::select(mort, 1, contains(as.character(2003:2025)))
# Disentangle 1st column, take both sexes and NUTS codes
names(mort)[1] <- "meta"
mort <- group_by(mort, meta) %>% 
  mutate(sex = strsplit(meta, ",")[[1]][2],
         NUTS_mort = strsplit(meta, ",")[[1]][4]) %>% 
  ungroup() %>% 
  filter(sex == "T") %>% 
  filter(nchar(NUTS_mort)==4|(nchar(NUTS_mort)==3 & grepl("DE|IE|HR|SI", NUTS_mort))) %>% 
  dplyr::select(-meta, -sex)
# Wide to long format, parse NAs and remove provisional flags
mort <- pivot_longer(mort, -NUTS_mort, names_to = "week", values_to = "deaths")
mort <- mutate(mort,
               deaths = ifelse(deaths == ":", NA, deaths),
               deaths = gsub(" p", "", deaths),
               deaths = as.numeric(deaths)) %>% 
  filter(!is.na(deaths))

# # Merge data of deaths for PT in 2023 and 2024
# mort <- mort[!(grepl("2023|2024", mort$week) & grepl("PT", mort$NUTS_mort)), ]
# mort_pt <- import(paste0(pathroot, "data/raw/mortality/PT_weeky_deaths_nuts2_2023_2024.xls"), which = "CleanedforR") |>
#   fill(week, .direction = "down") |>
#   mutate(week = paste0(str_extract(week, "\\d{4}"), "-W", str_extract(week, "\\d+")),
#          NUTS_mort = paste0("PT", nuts)) |>
#   select(-name, -nuts) |>
#   filter(nchar(NUTS_mort)==4) 
# mort <- bind_rows(mort, mort_pt)

# Discard XX/SIX NUTS2: unclassified and minor counts, no geometry available
mort <- mort[!grepl("XX", mort$NUTS_mort)&!grepl("SIX", mort$NUTS_mort),]
# Discard NO0B NUTS2 (Jan Mayen and Svalbard): always 0
mort <- mort[!grepl("NO0B", mort$NUTS_mort),]
# Discard extra Norway NUTS2: superseded (merged into larger NUTS2) in 2021 version
mort <- mort[!grepl("NO01|NO03|NO04|NO05", mort$NUTS_mort),] 
# Discard more NUTS2 for which we have no exposure data: Canary Islands (ES70),
# Guadeloupe (FRY1), Martinique (FRY2), Guyane (FRY3), La Reunion (FRY4),
# Mayotte (FRY5), Acores (PT20), Madeira (PT30)
mort <- mort[!grepl("ES70|FRY1|FRY2|FRY3|FRY4|FRY5|PT20|PT30", mort$NUTS_mort),] 
# Delete missing time
# mort <- mort[grepl("W99", mort$week),] # no missing time
# Merging deaths in the regions NL31 & NL33 (2015-2023), NL35 & NL36 due to 
# change in NUT2 boundaries, and name it NL31NL33 (2015-2025)
mort <- mort %>%
  filter(!NUTS_mort %in% c("NL31", "NL33")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NL31", "NL33")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NL31NL33", 
                deaths = sum(deaths))) %>%
  filter(!NUTS_mort %in% c("NL35", "NL36")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NL35", "NL36")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NL31NL33", 
                deaths = sum(deaths))) 
# Merging the regions PT1A & PT1B (2023-2025)
# due to change in NUTS2 boundaries, and name it PT17 (2003-2022+23-24)
mort <- mort %>%
  filter(!NUTS_mort %in% c("PT1A", "PT1B")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("PT1A", "PT1B")) %>%
              group_by(week) %>%
              summarise(
                NUTS_mort = "PT17",
                deaths = sum(deaths)))
# Merging the regions PT16 & PT18 (2003-2022) and PT19, PT1C & PT1D (2023-2025)
# due to change in NUTS2 boundaries, and name it PT16PT18
mort <- mort %>%
  filter(!NUTS_mort %in% c("PT16", "PT18")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("PT16", "PT18")) %>%
              group_by(week) %>%
              summarise(
                NUTS_mort = "PT16PT18",
                deaths = sum(deaths))) %>%
  filter(!NUTS_mort %in% c("PT19", "PT1C", "PT1D")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("PT19", "PT1C", "PT1D")) %>%
              group_by(week) %>%
              summarise(
                NUTS_mort = "PT16PT18",
                deaths = sum(deaths))) #%>% filter(NUTS_mort%in%c("PT16", "PT18","PT16PT18","PT19", "PT1C", "PT1D")) %>% View
# Order by NUTS unit and week
mort <- arrange(mort, NUTS_mort, week)


# Check that all mortality codes have an available geometry
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
if(any(mort$NUTS_mort[!mort$NUTS_mort %in% pointnuts$NUTS_mort])){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# to detect the difference
# setdiff(mort$NUTS_mort, pointnuts$NUTS_mort)

# Check that only North Macedonia has exposure but not mortality data
if(any(pointnuts$NUTS_mort[!pointnuts$NUTS_mort %in% mort$NUTS_mort] != "MK00")){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# ####################### to delete
# 
# # Join the data to check for matching geometries
# 
# # Convert SpatVector to sf object
# pointnuts_sf <- st_as_sf(pointnuts)
# 
# # Perform the join
# check_mort <- mort %>%
#   left_join(pointnuts_sf, by = "NUTS_mort")
# 
# # Identify missing geometries
# missing_geometries <- check_mort %>%
#   filter(is.na(geometry)) %>%
#   select(NUTS_mort)
# 
# # Output results
# if (nrow(missing_geometries) > 0) {
#   print("These mortality codes do not have a geometry:")
#   print(missing_geometries)
# } else {
#   print("All mortality codes have at least one geometry.")
# }
# 
# # Check if any of these values are not "MK00" (North Macedonia)
# # Identify NUTS_mort values that are in pointnuts but not in mort
# exposure_but_no_mortality <- pointnuts$NUTS_mort[!pointnuts$NUTS_mort %in% mort$NUTS_mort] 
# 
# # Display all such NUTS_mort codes
# if (length(exposure_but_no_mortality) > 0) {
#   print("NUTS_mort codes with exposure data but no corresponding mortality data:") # something wrong with DE and IE -> nuts2 codes here, although we should have nuts1 level for these countries. Where is the mistake?
#   print(unique(exposure_but_no_mortality))
# } else {
#   print("All NUTS_mort codes with exposure data have corresponding mortality data.")
# }
# 
# #######################


# 2. Weekly to daily counts (ISO 8601) ----
# https://ec.europa.eu/eurostat/data/database?node_code=demomwk

# Expand weeks to days by dividing all death counts by 7 and stacking 7 times
########################## to delete, not needed anymore
# mort_day <- mutate(mort,
#                    year_week = paste0(substr(week, 1, 4), "-", substr(week, 5, 8))) 
#########################
mort_day <- mutate(mort, deaths = deaths/7)
mort_day <- bind_rows(mutate(mort_day, year_week_day = paste0(week, "-1")),
                       mutate(mort_day, year_week_day = paste0(week, "-2")),
                       mutate(mort_day, year_week_day = paste0(week, "-3")),
                       mutate(mort_day, year_week_day = paste0(week, "-4")),
                       mutate(mort_day, year_week_day = paste0(week, "-5")),
                       mutate(mort_day, year_week_day = paste0(week, "-6")),
                       mutate(mort_day, year_week_day = paste0(week, "-7"))) 
# mort_day <- mort_day %>%
#   mutate(year_week_day = str_replace(year_week_day, "^(\\d{4})-W(\\d)-", "\\1-W0\\2-"),
#          iso_date = ISOweek2date(paste0(substr(year_week_day, 1, 8), "-",
#                                         substr(year_week_day, 10, 10))),
#          date = as.Date(iso_date)) %>%
#   select(-year_week_day, -iso_date)

# mort_day <- mort_day %>%
#   mutate(year_week_day = str_replace(year_week_day, "^(\\d{4})-W(\\d)-", "\\1-W0\\2-"),
#          year = as.integer(substr(year_week_day, 1, 4)),   
#          week = as.integer(substr(year_week_day, 7, 8)),  
#          day = as.integer(substr(year_week_day, 10, 10)),
#          date = as.Date(paste0(year, "-01-01")) + weeks(week - 1) + days(day - 1)) %>% 
#   select(-day, -week, -year)

# Trim extra spaces
# mort_day_t1 <- mort_day %>% mutate(year_week_day = str_trim(year_week_day))

# Add leading zero to single-digit week numbers
mort_day <- mort_day %>% 
  mutate(year_week_day = str_replace(year_week_day, "-W(\\d)-", "-W0\\1-"))
# ISOweek2date(gsub("-([0-9]+)$", paste0("+\\1"), mort_day$year_week_day))

mort_day <- mutate(mort_day, date = ISOweek::ISOweek2date(year_week_day))

mort_day <- arrange(mort_day, NUTS_mort, date)
# Check that the counts before vs. after processing match
if(sum(mort$deaths) != sum(mort_day$deaths)){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# Filter by days and write
mort_day <- filter(mort_day, between(date, as.Date("2003-01-01"), as.Date("2025-12-31"))) %>% 
  mutate(year = year(date))
# save
write_csv(mort_day, paste0(pathroot, "data/processed/mortality.csv"))

# Clean
rm(list=ls())
