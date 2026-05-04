#-----------------------------------------------------------------------------#
#               2. Clean NUTS data and create rasters with codes              #
#-----------------------------------------------------------------------------#


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2027_wildfires/"


# 1. Read and split by NUTS level ----

nuts <- read_sf(paste0(pathroot, "data/raw/boundaries/NUTS_RG_01M_2021_3035.geojson")) |>
  dplyr::select(NUTS_ID, LEVL_CODE) |>
  st_transform(crs = 4326)
nuts_0 <- dplyr::filter(nuts, LEVL_CODE == 0) |>
  rename(NUTS_0 = NUTS_ID)|>
  dplyr::select(NUTS_0)
nuts_1 <- dplyr::filter(nuts, LEVL_CODE == 1) |>
  rename(NUTS_1 = NUTS_ID)|>
  dplyr::select(NUTS_1)
nuts_2 <- dplyr::filter(nuts, LEVL_CODE == 2) |>
  rename(NUTS_2 = NUTS_ID) |>
  dplyr::select(NUTS_2)
rm("nuts")

# save adapted NUTS2 shapefile for figures
st_write(nuts_0, "data/processed/nuts_0.shp", delete_layer = T)

# Merging the regions NL31 & NL33 (2015-2023), NL35 & NL36 due to 
# change in NUT2 boundaries, and name it NL31NL33 (2015-2025)
NL_merged <- st_union(nuts_2 %>% filter(NUTS_2 %in% c("NL31","NL33")), is_coverage = T)
NL_merged <- st_sf(NUTS_2 = "NL31NL33", geometry = NL_merged)
nuts_2 <- dplyr::bind_rows(nuts_2 %>% filter(!NUTS_2 %in% c("NL31", "NL33")), NL_merged)  

# Merging the regions PT16 & PT18 (2003-2022)
# due to change in NUTS2 boundaries, and name it PT16PT18
PT_merged <- st_union(nuts_2 %>% filter(NUTS_2 %in% c("PT16","PT18")), is_coverage = T)
PT_merged <- st_sf(NUTS_2 = "PT16PT18", geometry = PT_merged)
nuts_2 <- dplyr::bind_rows(nuts_2 %>% filter(!NUTS_2 %in% c("PT16", "PT18")), PT_merged)  

# save adapted NUTS2 shapefile for figures
st_write(nuts_2, "data/processed/nuts_2.shp", delete_layer = T)


# 2. Merge with grid ----

# Exposure boundaries
gridgeom <- rast("data/raw/SILAM/europeFirePM25-MODIS-2003to2025DayMean.nc4", lyrs = 1)
expo_polys <- st_as_sf(as.polygons(gridgeom, trunc = F, dissolve = F)) |>
  mutate(GRD_ID = 1:n()) |>
  dplyr::select(GRD_ID)
expo_points <- st_centroid(expo_polys)

# Extract NUTS codes at the exposure grid geometries by largest overlap
add_nuts <- function(expolys, nutspolys){
  # st_join(largest = T) is not efficient, we can substantially speed it up by
  # applying it only when necessary (more than one intersection)
  expolys_nuts <- st_join(expolys, nutspolys)
  expolys_1 <- expolys_nuts[!expolys_nuts$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- expolys[expolys$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- st_join(expolys_2, nutspolys, largest = T)
  expolys_nuts <- rbind(expolys_1, expolys_2) |>
    arrange(GRD_ID)
  expolys_nuts
}
expo_points$NUTS_2 <- add_nuts(expo_polys, nuts_2)$NUTS_2
expo_points <- expo_points[!is.na(expo_points$NUTS_2),]
expo_points <- group_by(expo_points, NUTS_2) |>
  mutate(NUTS_0 = substr(NUTS_2, 1, 2),
         NUTS_1 = substr(NUTS_2, 1, 3)) |>
  ungroup()


# 3. Add region IDs ---- # in 2024: United Kindom (UK) is removed and Kosovo (XB) is new in NUTS_0

euroregions <- import(paste0(pathroot, "data/raw/regions/2027 Country names and groupings -2.xlsx"), skip=1) |>
    dplyr::select(1,4,6,8,9,10,11)
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "NUTS_0", "EEA", "EEA_subregion", "region_UNgeo", "WB_income", "HDI_index")
# Change UK to Northern
euroregions$EEA_subregion[euroregions$NUTS_0=="UK"] <- "Northern"
expo_points <- left_join(expo_points, euroregions, by = "NUTS_0")


# 4. Filter regions and mortality ID ----

# Turkey (TR) and Jan Mayen and Svalbard (NO0B) - no population and mortality data
# Madeira (PT30) - no Atlantic islands
expo_points <- expo_points[expo_points$NUTS_0 != "TR" & !expo_points$NUTS_2 %in% c("NO0B", "PT30"),]

# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) - mortality at the NUTS1 level 
expo_points$NUTS_mort <- ifelse(expo_points$NUTS_0 %in% c("DE", "IE", "HR", "SI"),
                                expo_points$NUTS_1, expo_points$NUTS_2)

# We don't need NUTS_1 anymore
expo_points$NUTS_1 <- NULL


# 5. Write ----

write_sf(expo_points, paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))

# Clean
rm(list=ls())
