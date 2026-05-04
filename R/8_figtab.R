#-----------------------------------------------------------------------------#
#                    8. Generation of figures and tables                      #
#-----------------------------------------------------------------------------#

# Packages
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(data.table)
library(terra)
library(sf)
library(rio)
# library(cowplot)
library(patchwork)
library(here)
library(ggpattern)


# 0. Utils for computing trends ----

linear_trend <- function(df, outcome, spunit){
  mod <- lm(as.formula(paste0(outcome, "~year")), data = df)
  data.frame(spunit = as.character(df[1,spunit]),
             int = coef(mod)[1],
             coef = coef(mod)[2],
             lower = confint(mod)[2,1],
             upper = confint(mod)[2,2],
             pval = summary(mod)$coefficients[2,4])
}


# 1.1 Trends by European regions ----

FWI_region <- read_csv("data/processed/FWI_region.csv")
pm25_region <- read_csv("data/processed/pm25_region.csv")
att_region <- read_csv("data/processed/attributable_region.csv") %>% 
  rename(attr_stand = attr_100K)
FWI_trend <- split(FWI_region, f = FWI_region$region) %>% 
  map_df(linear_trend, outcome = "FWI_pop", spunit = "region")
pm25_trend <- split(pm25_region, f = pm25_region$region) %>% 
  map_df(linear_trend, outcome = "pm25_pop", spunit = "region")
att_trend <- split(att_region, f = att_region$region) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "region")

p1 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = FWI_region, aes(x=year, y=FWI_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(1980, 2025, 5), minor_breaks = seq(1982, 2025, 5)) +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")
 
p2 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = pm25_region, aes(x=year, y=pm25_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2004, 2025, 3), minor_breaks = seq(2003, 2025, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme_classic() +
  theme()

p3 <- ggplot() + 
  geom_abline(data = att_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = att_region, aes(x = year, y = attr_stand, col = region, group = region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_x_continuous(breaks = seq(2004, 2025, 3), minor_breaks = seq(2003, 2025, 1)) +
  xlab("") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  labs(colour = "") + 
  theme_classic() +
  theme(legend.position = "none")
 
p4 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/regiontrend_FWI_pm2.5_attrdeaths.png", pall, width = 8, height = 8, dpi = 300)

# Export trends
trendtab <- rbind(mutate(FWI_trend, metric = "FWI"),
                  mutate(pm25_trend, metric = "PM2.5"),
                  mutate(att_trend, metric = "attributable deaths/100K")) %>% 
  rename(region = spunit, pvalue = pval) %>% 
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) %>% 
  select(metric, region, trend, pvalue) %>% 
  arrange(desc(metric), region)

write_csv(trendtab, "figures/figures_regiontrend.csv")
rm("FWI_region", "FWI_trend", "att_region", "att_trend",
   "p1", "p2", "pall", "trendtab")


# 1.2 (%-)change by European regions ----
# FWI_region <- read_csv("data/processed/FWI_region.csv")
# pm25_region <- read_csv("data/processed/pm25_region.csv")
# att_region <- read_csv("data/processed/attributable_region.csv") 
# 
# # calculate difference and %-change between 2003-2012 and 2016-2025
# FWI_change <- FWI_region %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   # Define periods
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(region, period) %>%
#   summarise(mean_FWI_pop = mean(FWI_pop, na.rm = T), .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = mean_FWI_pop) %>%
#   rename(
#     fwi_2003_2012 = `2003_2012`,
#     fwi_2016_2025 = `2016_2025`) %>%
#   mutate(
#     diff_FWI_pop = fwi_2016_2025 - fwi_2003_2012,
#     pct_change_FWI_pop = diff_FWI_pop / fwi_2003_2012 * 100)
# 
# pm25_change <- pm25_region %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(region, period) %>%
#   summarise(mean_pm25_pop = mean(pm25_pop, na.rm = T), .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = mean_pm25_pop) %>%
#   rename(
#     pm25_2003_2012 = `2003_2012`,
#     pm25_2016_2025 = `2016_2025`) %>%
#   mutate(
#     diff_pm25_pop = pm25_2016_2025 - pm25_2003_2012,
#     pct_change_pm25_pop = diff_pm25_pop / pm25_2003_2012 * 100)
# 
# att_change <- att_region %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(region, period) %>%
#   summarise(
#     mean_attr = mean(attr, na.rm = T),
#     mean_attr_100K = mean(attr_100K, na.rm = T),
#     .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = c(mean_attr, mean_attr_100K)) %>%
#   mutate(
#     diff_attr = mean_attr_2016_2025 - mean_attr_2003_2012,
#     pct_change_attr = diff_attr / mean_attr_2003_2012 * 100,
#     diff_attr_100K = mean_attr_100K_2016_2025 - mean_attr_100K_2003_2012,
#     pct_change_attr_100K = diff_attr_100K / mean_attr_100K_2003_2012 * 100)
# 
# # merge the changes of FWI, pm25 and deaths
# all_change <- FWI_change %>%
#   left_join(pm25_change, by = "region") %>%
#   left_join(att_change, by = "region")
# 
# # save
# write.csv(all_change,"figures/fwi_pm25_attr_region_change03-12_16-25.csv")
# 

# 3.1 Trends by country ----

# Compute trends
FWI_country <- read_csv("data/processed/FWI_country.csv")
pm25_country <- read_csv("data/processed/pm25_country.csv")
attr_country <- read_csv("data/processed/attributable_country.csv") %>% 
  rename(attr_stand = attr_100K)
FWI_trendpop <- split(FWI_country, f = FWI_country$NUTS_0) %>% 
  map_df(linear_trend, outcome = "FWI_spatial", spunit = "NUTS_0")
pm25_trendpop <- split(pm25_country, f = pm25_country$NUTS_0) %>% 
  map_df(linear_trend, outcome = "pm25_spatial", spunit = "NUTS_0")
attr_trendpop <- attr_country %>%
  filter(!is.na(attr_stand)) %>% # Remove rows with NA in 'attr_stand'
  split(.$NUTS_0) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "NUTS_0")

# Add complete names
euroregions <- import("data/raw/regions/2027 Country names and groupings -2.xlsx", skip=1) %>% 
  dplyr::select(1,4,6,8,9,10,11)
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "NUTS_0", "EEA", "EEA_subregion", "region_UNgeo", "WB_income", "HDI_index")
euroregions <- euroregions %>% 
  rename(country = country_name,
         region = EEA_subregion) %>% 
  mutate(region = ifelse(region=="Not EEA", "Northern", region)) # the UK
  
# euroregions$region[euroregions$NUTS_0=="CY"] <- "Southern Europe"

FWI_trendpop <- left_join(FWI_trendpop, euroregions, by = c("spunit" = "NUTS_0")) 
pm25_trendpop <- left_join(pm25_trendpop, euroregions, by = c("spunit" = "NUTS_0"))
attr_trendpop <- left_join(attr_trendpop, euroregions, by = c("spunit" = "NUTS_0"))

# Prepare for plotting
FWI_trendpop$Pvalue <- case_when(
  FWI_trendpop$pval > 0.2 ~ "> 0.2",
  FWI_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  FWI_trendpop$pval <= 0.05 ~ "< 0.05")
FWI_trendpop$Pvalue <- fct_relevel(FWI_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

pm25_trendpop$Pvalue <- case_when(
  pm25_trendpop$pval > 0.2 ~ "> 0.2",
  pm25_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  pm25_trendpop$pval <= 0.05 ~ "< 0.05")
pm25_trendpop$Pvalue <- fct_relevel(pm25_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

attr_trendpop$Pvalue <- case_when(
  attr_trendpop$pval > 0.2 ~ "> 0.2",
  attr_trendpop$pval > 0.05 ~ "0.05 to 0.2",
  attr_trendpop$pval <= 0.05 ~ "< 0.05")
attr_trendpop$Pvalue <- fct_relevel(attr_trendpop$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

# Plotting
p1 <- ggpubr::ggdotchart(FWI_trendpop, x = "country", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend in Fire Weather Index") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p2 <- ggpubr::ggdotchart(pm25_trendpop, x = "country", y = "coef",
                 color = "region",
                 dot.size = "Pvalue",
                 sorting = "descending",
                 add = "segments",
                 add.params = list(color = "lightgray", size = 2),
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~`wildfire-PM`[2.5])) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p3 <- ggpubr::ggdotchart(attr_trendpop, x = "country", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

pall <- ggpubr::ggarrange(p1, p2, p3,
                  nrow = 3, common.legend = T, legend = "bottom")

ggsave("figures/countrytrend_popw.png", pall,  width = 8, height = 11.5)
rm("FWI_country", "FWI_trendpop", "attr_country", "attr_trendpop", "p1", "p2", "p3", "pall",
   "pm25_country", "pm25_trendpop", "euroregions")

# 3.2 (%-)change by country ----
# 
# FWI_country <- read_csv("data/processed/FWI_country.csv")
# pm25_country <- read_csv("data/processed/pm25_country.csv")
# att_country <- read_csv("data/processed/attributable_country.csv") 
# 
# # calculate difference and %-change between 2003-2012 and 2016-2025
# FWI_change <- FWI_country %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   # Define periods
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(NUTS_0, period) %>%
#   summarise(mean_FWI_pop = mean(FWI_pop, na.rm = T), .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = mean_FWI_pop) %>%
#   rename(
#     fwi_2003_2012 = `2003_2012`,
#     fwi_2016_2025 = `2016_2025`) %>%
#   mutate(
#     diff_FWI_pop = fwi_2016_2025 - fwi_2003_2012,
#     pct_change_FWI_pop = diff_FWI_pop / fwi_2003_2012 * 100)
# 
# pm25_change <- pm25_country %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(NUTS_0, period) %>%
#   summarise(mean_pm25_pop = mean(pm25_pop, na.rm = T), .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = mean_pm25_pop) %>%
#   rename(
#     pm25_2003_2012 = `2003_2012`,
#     pm25_2016_2025 = `2016_2025`) %>%
#   mutate(
#     diff_pm25_pop = pm25_2016_2025 - pm25_2003_2012,
#     pct_change_pm25_pop = diff_pm25_pop / pm25_2003_2012 * 100)
# 
# att_change <- att_country %>%
#   filter(year %in% c(2003:2012, 2016:2025)) %>%
#   mutate(period = case_when(
#     year >= 2003 & year <= 2012 ~ "2003_2012",
#     year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
#   group_by(NUTS_0, period) %>%
#   summarise(
#     mean_attr = mean(attr, na.rm = T),
#     mean_attr_100K = mean(attr_100K, na.rm = T),
#     .groups = "drop") %>%
#   pivot_wider(
#     names_from = period,
#     values_from = c(mean_attr, mean_attr_100K)) %>%
#   mutate(
#     diff_attr = mean_attr_2016_2025 - mean_attr_2003_2012,
#     pct_change_attr = diff_attr / mean_attr_2003_2012 * 100,
#     diff_attr_100K = mean_attr_100K_2016_2025 - mean_attr_100K_2003_2012,
#     pct_change_attr_100K = diff_attr_100K / mean_attr_100K_2003_2012 * 100)
# 
# # merge the changes of FWI, pm25 and deaths
# all_change <- FWI_change %>%
#   left_join(pm25_change, by = "NUTS_0") %>%
#   left_join(att_change, by = "NUTS_0")
# 
# # Add complete names
# euroregions <- import("data/raw/regions/2027 Country names and groupings -2.xlsx", skip=1) %>% 
#   dplyr::select(1,4,6,8,9,10,11)
# euroregions <- euroregions[!duplicated(euroregions),]
# names(euroregions) <- c("country_name", "NUTS_0", "EEA", "EEA_subregion", "region_UNgeo", "WB_income", "HDI_index")
# euroregions <- euroregions %>% 
#   rename(country = country_name,
#          region = EEA_subregion) %>% 
#   mutate(region = ifelse(region=="Not EEA", "Northern", region)) # the UK
# 
# all_change <- euroregions %>% 
#   select(country,NUTS_0,region) %>% 
#   right_join(all_change, by = "NUTS_0")
# 
# # save
# write.csv(all_change,"figures/fwi_pm25_attr_country_change03-12_16-25.csv")



# 4.1 Trends by NUTS2 2003-2025, maps ----
# Compute trends
FWI_nuts <- read_csv("data/processed/FWI_nuts2.csv") %>% 
  filter(year>2002)
pm25_nuts <- read_csv("data/processed/pm25_nuts2.csv")
attr_nuts <- read_csv("data/processed/attributable_nuts.csv") %>% 
  rename(attr_stand = attr_100K) %>% 
  rename(NUTS_2 = NUTS_mort)
FWI_trendpop <- split(FWI_nuts, f = FWI_nuts$NUTS_2) %>% 
  map_df(linear_trend, outcome = "FWI_spatial", spunit = "NUTS_2") %>% 
  mutate(pval_cat = factor(if_else(pval < 0.05, "<0.05", "≥0.05"),
                           levels = c("<0.05", "≥0.05")))
pm25_trendpop <- split(pm25_nuts, f = pm25_nuts$NUTS_2) %>% 
  map_df(linear_trend, outcome = "pm25_spatial", spunit = "NUTS_2") %>% 
  mutate(pval_cat = factor(if_else(pval < 0.05, "<0.05", "≥0.05"),
                           levels = c("<0.05", "≥0.05")))
attr_trendpop <- attr_nuts %>%
  filter(!is.na(attr_stand)) %>% # Remove rows with NA in 'attr_stand'
  split(.$NUTS_2) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "NUTS_2") %>% 
  mutate(pval_cat = factor(if_else(pval < 0.05, "<0.05", "≥0.05"),
                           levels = c("<0.05", "≥0.05")))

# Read and prepare original NUTS2 polygons
nutspoly <- st_read("data/processed/nuts_2.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(!grepl("^TR", NUTS_2), # no mortality data
         !grepl("NO0B", NUTS_2)) %>%  # no population data
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Read and prepare original NUTS0 polygons
nuts0poly <- st_read("data/processed/nuts_0.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(!grepl("^TR", NUTS_0)) %>%  # no mortality data
         # !grepl("NO0B", NUTS_2)) %>%  # no population data
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Read and prepare original country polygons
europe_ids <- c("AD", "AL", "AT", "BA", "BE", "BG", "BY", "CH", "CY", "CZ",
  "DE", "DK", "EE", "EL", "ES", "FI", "FO", "FR", "HR", "HU",
  "IE", "IM", "IS", "IT", "JE", "LI", "LT", "LU", "LV", "MC",
  "MD", "ME", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "RS",
  "RU", "SE", "SI", "SJ", "SK", "SM", "UA", "UK", "VA",
  "TR")

cntrpoly <- st_read("data/raw/boundaries/CNTR_RG_20M_2024_3035.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(grepl(paste(europe_ids, collapse = "|"), CNTR_ID)) %>%
  select(CNTR_ID) %>% 
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Countries to aggregate from NUTS2 -> NUTS1
countries_agg <- c("DE", "IE", "HR", "SI")

# Aggregate selected countries to NUTS1
nutspoly_sel_NUTS1 <- nutspoly %>%
  filter(grepl(paste(countries_agg, collapse = "|"), NUTS_2)) %>% 
  mutate(NUTS_1 = substr(NUTS_2, 1, 3)) %>%
  group_by(NUTS_1) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop") %>%
  rename(NUTS_2 = NUTS_1)   # harmonise column name

# Remove original NUTS2 polygons for those countries
nutspoly_no_sel <- nutspoly %>% 
  filter(!grepl(paste(countries_agg, collapse = "|"), NUTS_2))

# Bind aggregated and remaining polygons
common_cols <- intersect(
  names(nutspoly_no_sel),
  names(nutspoly_sel_NUTS1))

nutspoly_attr <- rbind(
  nutspoly_no_sel[, common_cols],
  nutspoly_sel_NUTS1[, common_cols])

# Join trends to spatial data
FWI_map  <- nutspoly %>% left_join(FWI_trendpop,  by = c("NUTS_2"="spunit"))
pm25_map <- nutspoly %>% left_join(pm25_trendpop, by = c("NUTS_2"="spunit"))
attr_map <- nutspoly_attr %>% left_join(attr_trendpop, by = c("NUTS_2"="spunit"))

# calculating global limits for a shared legend

# global_limits <- range(
#   FWI_map$coef,
#   pm25_map$coef,
#   attr_map$coef,
#   na.rm = T)
# 
# lim <- max(abs(global_limits))
# global_limits <- c(-lim, lim)

# mid point for colour gradient
fixed_midpoint <- 0

# Plotting function with hashes
plot_nuts_trend <- function(sfdata, cntrpoly, nuts0poly, value, title, legend_title,
                            midpoint = 0, limits = NULL) {
  
  ggplot() +
    # Background: NUTS0 polygons (grey fill, no outline)
  geom_sf(
    data = cntrpoly,
    fill = "grey70",
    color = NA) +
    # Main layer: NUTS2 polygons (transparent if NA)
    geom_sf(
      data = sfdata,
      aes(fill = {{ value }}),
      color = NA) +
    
    # Hashing over non-significant trend areas (p ≥ 0.05)
    geom_sf_pattern(
      data        = sfdata[sfdata$pval_cat == "≥0.05", ],
      pattern     = "stripe",
      fill        = NA,
      color       = NA,
      pattern_fill    = NA,          # no fill between lines — colour beneath shows through
      pattern_colour  = "grey30",    # line colour
      pattern_size = 0.1,       # keep lines fine
      pattern_angle   = 45,          # diagonal stripes
      pattern_spacing = 0.01,       # tight spacing for small polygons
      pattern_density = 0.08) +         # low density = thin lines relative to gap
   
    # NUTS0 outlines on top
  geom_sf(
    data = nuts0poly,
    fill = NA,
    color = "grey30",
    linewidth = 0.3) +
    
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = midpoint,
      oob = scales::squish,
      na.value = NA,  
      name = legend_title) +
    
    labs(title = title) +
    coord_sf(expand = F) +
    
    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(
        face = "plain",
        hjust = 0.5,
        size = 14),
      legend.position = "right")
}


# Create maps
map_FWI_1 <- plot_nuts_trend(
  sfdata = FWI_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = "Fire danger (FWI)",
  legend_title = "Trend")

map_pm25_1 <- plot_nuts_trend(
  sfdata = pm25_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = expression(Wildfire - PM[2.5]~exposure),
  legend_title = "Trend")

map_attr_1 <- plot_nuts_trend(
  sfdata = attr_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = expression(Attributable~mortality~to~wildfire - PM[2.5]),
  legend_title = "Trend")

final_plot_1 <- (map_FWI_1 + map_pm25_1 + map_attr_1) +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Trends 2003–2025",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5)))

# save
ggsave("figures/NUTS2_trend_maps_pval.png", final_plot_1, width = 16, height = 5, dpi = 600)

#################
# Plotting function with stars where significant trends
plot_nuts_trend <- function(sfdata, cntrpoly, nuts0poly, value, title, legend_title,
                            midpoint = 0, limits = NULL) {
  
  ggplot() +
    # Background: NUTS0 polygons (grey fill, no outline)
    geom_sf(
      data = cntrpoly,
      fill = "grey70",
      color = NA) +
    # Main layer: NUTS2 polygons (transparent if NA)
    geom_sf(
      data = sfdata,
      aes(fill = {{ value }}),
      color = NA) +
    
    
    # Black star for non-significant trends
    geom_sf(
      data = sfdata[sfdata$pval_cat == "<0.05", ] |>
        sf::st_point_on_surface() |>
        sf::st_cast("POINT"),
      shape = 42,        # star
      colour = "black",
      size = 5,
      stroke = 0.6) +
  
    
    # # Hashing over non-significant trend areas (p ≥ 0.05)
    # geom_sf_pattern(
    #   data        = sfdata[sfdata$pval_cat == "≥0.05", ],
    #   pattern     = "stripe",
    #   fill        = NA,
    #   color       = NA,
    #   pattern_fill    = NA,          # no fill between lines — colour beneath shows through
    #   pattern_colour  = "grey30",    # line colour
    #   pattern_size = 0.1,       # keep lines fine
    #   pattern_angle   = 45,          # diagonal stripes
    #   pattern_spacing = 0.01,       # tight spacing for small polygons
    #   pattern_density = 0.08) +         # low density = thin lines relative to gap
    
    # NUTS0 outlines on top
    geom_sf(
      data = nuts0poly,
      fill = NA,
      color = "grey30",
      linewidth = 0.3) +
    
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = midpoint,
      oob = scales::squish,
      na.value = NA,  
      name = legend_title) +
    
    labs(title = title) +
    coord_sf(expand = F) +
    
    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(
        face = "plain",
        hjust = 0.5,
        size = 14),
      legend.position = "right")
}


# Create maps
map_FWI_1 <- plot_nuts_trend(
  sfdata = FWI_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = "Fire danger (FWI)",
  legend_title = "Trend")

map_pm25_1 <- plot_nuts_trend(
  sfdata = pm25_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = expression(Wildfire - PM[2.5]~exposure),
  legend_title = "Trend")

map_attr_1 <- plot_nuts_trend(
  sfdata = attr_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = coef,
  title = expression(Attributable~mortality~to~wildfire - PM[2.5]),
  legend_title = "Trend")

final_plot_1 <- (map_FWI_1 + map_pm25_1 + map_attr_1) +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Trends 2003–2025",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5)))

# save
ggsave("figures/NUTS2_trend_maps_pval_stars.png", final_plot_1, width = 16, height = 5, dpi = 600)
######################




# 4.2 (%-)change by NUTS2 2003-2012 - 2016-2025, maps ----
# Compute averages across periods
FWI_nuts <- read_csv("data/processed/FWI_nuts2.csv") %>%
  filter(year>2002)
pm25_nuts <- read_csv("data/processed/pm25_nuts2.csv")
att_nuts <- read_csv("data/processed/attributable_nuts.csv") %>%
  rename(NUTS_2 = NUTS_mort)

# calculate difference and %-change between 2003-2012 and 2016-2025
FWI_change <- FWI_nuts %>%
  filter(year %in% c(2003:2012, 2016:2025)) %>%
  # Define periods
  mutate(period = case_when(
    year >= 2003 & year <= 2012 ~ "2003_2012",
    year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
  group_by(NUTS_2, period) %>%
  summarise(mean_FWI_pop = mean(FWI_pop, na.rm = T), .groups = "drop") %>%
  pivot_wider(
    names_from = period,
    values_from = mean_FWI_pop) %>%
  rename(
    fwi_2003_2012 = `2003_2012`,
    fwi_2016_2025 = `2016_2025`) %>%
  mutate(
    diff_FWI_pop = fwi_2016_2025 - fwi_2003_2012,
    pct_change_FWI_pop = diff_FWI_pop / fwi_2003_2012 * 100)

pm25_change <- pm25_nuts %>%
  filter(year %in% c(2003:2012, 2016:2025)) %>%
  mutate(period = case_when(
    year >= 2003 & year <= 2012 ~ "2003_2012",
    year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
  group_by(NUTS_2, period) %>%
  summarise(mean_pm25_pop = mean(pm25_pop, na.rm = T), .groups = "drop") %>%
  pivot_wider(
    names_from = period,
    values_from = mean_pm25_pop) %>%
  rename(
    pm25_2003_2012 = `2003_2012`,
    pm25_2016_2025 = `2016_2025`) %>%
  mutate(
    diff_pm25_pop = pm25_2016_2025 - pm25_2003_2012,
    pct_change_pm25_pop = diff_pm25_pop / pm25_2003_2012 * 100)

att_change <- att_nuts %>%
  filter(year %in% c(2003:2012, 2016:2025)) %>%
  mutate(period = case_when(
    year >= 2003 & year <= 2012 ~ "2003_2012",
    year >= 2016 & year <= 2025 ~ "2016_2025")) %>%
  group_by(NUTS_2, period) %>%
  summarise(
    mean_attr = mean(attr, na.rm = T),
    mean_attr_100K = mean(attr_100K, na.rm = T),
    .groups = "drop") %>%
  pivot_wider(
    names_from = period,
    values_from = c(mean_attr, mean_attr_100K)) %>%
  mutate(
    diff_attr = mean_attr_2016_2025 - mean_attr_2003_2012,
    pct_change_attr = diff_attr / mean_attr_2003_2012 * 100,
    diff_attr_100K = mean_attr_100K_2016_2025 - mean_attr_100K_2003_2012,
    pct_change_attr_100K = diff_attr_100K / mean_attr_100K_2003_2012 * 100)

# # merge the changes of FWI, pm25 and deaths
# all_change <- FWI_change %>%
#   left_join(pm25_change, by = "NUTS_2") %>%
#   left_join(att_change, by = "NUTS_2")




# Read and prepare original NUTS2 polygons
nutspoly <- st_read("data/processed/nuts_2.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(!grepl("^TR", NUTS_2), # no mortality data
         !grepl("NO0B", NUTS_2)) %>%  # no population data
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Read and prepare original NUTS0 polygons
nuts0poly <- st_read("data/processed/nuts_0.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(!grepl("^TR", NUTS_0)) %>%  # no mortality data
  # !grepl("NO0B", NUTS_2)) %>%  # no population data
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Read and prepare original country polygons
europe_ids <- c("AD", "AL", "AT", "BA", "BE", "BG", "BY", "CH", "CY", "CZ",
                "DE", "DK", "EE", "EL", "ES", "FI", "FO", "FR", "HR", "HU",
                "IE", "IM", "IS", "IT", "JE", "LI", "LT", "LU", "LV", "MC",
                "MD", "ME", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "RS",
                "RU", "SE", "SI", "SJ", "SK", "SM", "UA", "UK", "VA",
                "TR")

cntrpoly <- st_read("data/raw/boundaries/CNTR_RG_20M_2024_3035.shp") %>%
  st_transform(3035) %>%   # ETRS89 / LAEA Europe
  # remove all Turkey NUTS2 polygons
  filter(grepl(paste(europe_ids, collapse = "|"), CNTR_ID)) %>%
  select(CNTR_ID) %>%
  st_crop(
    xmin = 2500000, ymin = 1200000,
    xmax = 7500000, ymax = 5500000)

# Countries to aggregate from NUTS2 -> NUTS1
countries_agg <- c("DE", "IE", "HR", "SI")

# Aggregate selected countries to NUTS1
nutspoly_sel_NUTS1 <- nutspoly %>%
  filter(grepl(paste(countries_agg, collapse = "|"), NUTS_2)) %>%
  mutate(NUTS_1 = substr(NUTS_2, 1, 3)) %>%
  group_by(NUTS_1) %>%
  summarise(
    geometry = st_union(geometry),
    .groups = "drop") %>%
  rename(NUTS_2 = NUTS_1)   # harmonise column name

# Remove original NUTS2 polygons for those countries
nutspoly_no_sel <- nutspoly %>%
  filter(!grepl(paste(countries_agg, collapse = "|"), NUTS_2))

# Bind aggregated and remaining polygons
common_cols <- intersect(
  names(nutspoly_no_sel),
  names(nutspoly_sel_NUTS1))

nutspoly_attr <- rbind(
  nutspoly_no_sel[, common_cols],
  nutspoly_sel_NUTS1[, common_cols])

# Join trends to spatial data
FWI_map  <- nutspoly %>% left_join(FWI_change,  by = "NUTS_2")
pm25_map <- nutspoly %>% left_join(pm25_change, by = "NUTS_2")
attr_map <- nutspoly_attr %>% left_join(att_change, by = "NUTS_2")

# calculating global limits for a shared legend

# global_limits <- range(
#   FWI_map$coef,
#   pm25_map$coef,
#   attr_map$coef,
#   na.rm = T)
#
# lim <- max(abs(global_limits))
# global_limits <- c(-lim, lim)

# mid point for colour gradient
fixed_midpoint <- 0

# Plotting function
plot_nuts_trend <- function(sfdata, cntrpoly, nuts0poly, value, title, legend_title,
                            midpoint = 0, limits = NULL) {

  ggplot() +
    # Background: NUTS0 polygons (grey fill, no outline)
    geom_sf(
      data = cntrpoly,
      fill = "grey70",
      color = NA) +
    # Main layer: NUTS2 polygons (transparent if NA)
    geom_sf(
      data = sfdata,
      aes(fill = {{ value }}),
      color = NA) +
    # NUTS0 outlines on top
    geom_sf(
      data = nuts0poly,
      fill = NA,
      color = "grey30",
      linewidth = 0.3) +

    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = midpoint,
      oob = scales::squish,
      na.value = NA,
      name = legend_title) +

    labs(title = title) +
    coord_sf(expand = F) +

    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),

      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(
        face = "plain",
        hjust = 0.5,
        size = 14),
      legend.position = "right")
}


# # Create maps
# map_FWI <- plot_nuts_trend(
#   sfdata = FWI_map,
#   cntrpoly = cntrpoly,
#   nuts0poly = nuts0poly,
#   value = pct_change_FWI_pop,
#   title = "Fire danger (FWI)",
#   legend_title = "%-change")
# 
# map_pm25 <- plot_nuts_trend(
#   sfdata = pm25_map,
#   cntrpoly = cntrpoly,
#   nuts0poly = nuts0poly,
#   value = pct_change_pm25_pop,
#   title = expression(Wildfire - PM[2.5]~exposure),
#   legend_title = "%-change")
# 
# map_attr <- plot_nuts_trend(
#   sfdata = attr_map,
#   cntrpoly = cntrpoly,
#   nuts0poly = nuts0poly,
#   value = pct_change_attr,
#   title = expression(Attributable~mortality~to~wildfire - PM[2.5]),
#   legend_title = "%-change")
# 
# final_plot_2 <- (map_FWI + map_pm25 + map_attr) +
#   plot_layout(ncol = 3) +
#   plot_annotation(
#     title = "%-change comparing 2003–2012 with 2016-2025",
#     theme = theme(
#       plot.title = element_text(
#         face = "bold",
#         size = 18,
#         hjust = 0.5)))
# 
# # save
# ggsave("figures/NUTS2_pctchange_maps.png", final_plot_2, width = 16, height = 5, dpi = 300)

# 4.3 Average FWI, PM2.5 and deaths by NUTS2 2016-2025, maps ----

# Create maps
map_FWI_3 <- plot_nuts_trend(
  sfdata = FWI_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = fwi_2016_2025,
  title = expression(Fire~danger~"(FWI)"),
  legend_title = expression(FWI))

map_pm25_3 <- plot_nuts_trend(
  sfdata = pm25_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = pm25_2016_2025,
  title = expression(Wildfire - PM[2.5]~exposure),
  legend_title = expression(PM[2.5]~"["*mu*g/m^3*"]"))

map_attr_3 <- plot_nuts_trend(
  sfdata = attr_map,
  cntrpoly = cntrpoly,
  nuts0poly = nuts0poly,
  value = mean_attr_100K_2016_2025,
  title = expression(Attributable~mortality~to~wildfire - PM[2.5]),
  legend_title = expression(N~deaths~"/100,000"))


final_plot_3 <- (map_FWI_3 + map_pm25_3 + map_attr_3) +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Average across 2016–2025",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5)))

# save
ggsave("figures/NUTS2_average_2016_2025_maps.png", final_plot_3, width = 16, height = 5, dpi = 600)

# save 2 maps together
legend_bottom_theme <- theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "horizontal",
  legend.box.just = "center",
  legend.title.position = "bottom",
  legend.title.align = 0.5,
  legend.key.width  = unit(1.2, "cm"),
  legend.key.height = unit(0.4, "cm"),
  legend.text  = element_text(size = 10),
  legend.title = element_text(size = 11),
  plot.margin = margin(6, 6, 6, 6))


apply_legend <- function(p) {
  p +
    legend_bottom_theme +
    guides(
      fill = guide_colorbar(
        title.position = "bottom",
        title.hjust = 0.5))
}



map_FWI_3  <- apply_legend(map_FWI_3)
map_pm25_3 <- apply_legend(map_pm25_3)
map_attr_3 <- apply_legend(map_attr_3)

map_FWI_1  <- apply_legend(map_FWI_1)
map_pm25_1 <- apply_legend(map_pm25_1)
map_attr_1 <- apply_legend(map_attr_1)


# final_plot_3 <- (map_FWI_3 + map_pm25_3 + map_attr_3) +
#   plot_layout(ncol = 3, guides = "collect") +
#   plot_annotation(
#     title = "Average across 2016–2025",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 18, hjust = 0.5)))
# 
# final_plot_1 <- (map_FWI_1 + map_pm25_1 + map_attr_1) +
#   plot_layout(ncol = 3, guides = "collect") +
#   plot_annotation(
#     title = "Trends 2003–2025",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 18, hjust = 0.5)))



# map_FWI_3  <- map_FWI_3  + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))
# map_pm25_3 <- map_pm25_3 + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))
# map_attr_3 <- map_attr_3 + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))
# 
# map_FWI_1  <- map_FWI_1  + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))
# map_pm25_1 <- map_pm25_1 + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))
# map_attr_1 <- map_attr_1 + legend_bottom_theme +
#   guides(fill=guide_colorbar(title.position="bottom",title.hjust = 0.5))



final_plot_3 <- (
  (map_FWI_3 + map_pm25_3 + map_attr_3) &
    theme(legend.position = "bottom")) +
  # plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Average across 2016–2025",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5)))


final_plot_1 <- (
  (map_FWI_1 + map_pm25_1 + map_attr_1) &
    theme(legend.position = "bottom")) +
  # plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Trends 2003–2025",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5)))

# final_plot_3 <- (map_FWI_3 + map_pm25_3 + map_attr_3) +
#   # plot_layout(ncol = 3, guides = "collect") +
#   plot_annotation(
#     title = "Average across 2016–2025",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 18, hjust = 0.5))) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")

# final_plot_1 <- (map_FWI_1 + map_pm25_1 + map_attr_1) +
#   # plot_layout(ncol = 3, guides = "collect") +
#   plot_annotation(
#     title = "Trends 2003–2025",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 18, hjust = 0.5))) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")


final_plot <- 
  wrap_elements(full = final_plot_3) /
  wrap_elements(full = final_plot_1) +
  plot_layout(heights = c(1, 1))

ggsave("figures/NUTS2_average_2016_2025_trends_maps_pval.png", final_plot, width = 16, height = 15, dpi = 600)



# 5. Inequalities ----

# Data prep
FWI_nuts2 <- read_csv("data/processed/FWI_nuts2.csv") %>% 
  filter(year %in% 2003:2025)
pm25_nuts2 <- read_csv("data/processed/pm25_nuts2.csv")
attr_nuts2 <- read_csv("data/processed/attributable_nuts.csv") %>% 
  rename(attr_stand = attr_100K)
pm25_nuts2 <- full_join(FWI_nuts2, pm25_nuts2, by = c("NUTS_2", "year")) %>%
  full_join(attr_nuts2, by = c("NUTS_2" = "NUTS_mort", "year" = "year"))
pm25_nuts2 <- group_by(pm25_nuts2, NUTS_2) %>% 
  summarise(pm25 = mean(pm25_pop),
            FWI = mean(FWI_pop),
            attr_stand = mean(attr_stand, na.rm=T)) %>% 
  mutate(attr_stand = ifelse(is.nan(attr_stand), NA, attr_stand))


ineq <- import("data/raw/deprivation/ilc_mddd21$defaultview_spreadsheet.xlsx", sheet="data for R")
ineq <- ineq %>% 
  mutate(across(3:7, as.numeric)) %>% 
  mutate(deprivation = rowSums(select(., 3:7), na.rm = T) /
           rowSums(!is.na(select(., 3:7)))) %>% 
  select(-c(2:7)) 
ineq <- ineq %>% 
  add_row(NUTS_2 = "NL31NL33", 
          deprivation = sum(ineq %>% filter(NUTS_2=="NL31") %>% 
                              pull(deprivation), 
                            ineq %>% filter(NUTS_2=="NL33") %>% 
                              pull(deprivation))/2) %>% 
  add_row(NUTS_2 = "PT16PT18",
          deprivation = sum(ineq %>% filter(NUTS_2=="PT16") %>% 
                              pull(deprivation),
                            ineq %>% filter(NUTS_2=="PT18") %>% 
                              pull(deprivation))/2) %>% 
  mutate(tertile = ntile(deprivation, 3),
         category = factor(tertile, levels = c(1, 2, 3),
                           labels = c("low", "medium", "high"))) %>%
  filter(NUTS_2!="NL31") %>% 
  filter(NUTS_2!="NL33") %>% 
  filter(NUTS_2!="PT16") %>% 
  filter(NUTS_2!="PT18")

pm25_nuts2 <- inner_join(pm25_nuts2, ineq, by = c("NUTS_2" = "NUTS_2"))
# FWImeans <- data.frame(category = c("low", "medium", "high"),
#                         mean = tapply(pm25_nuts2$FWI, pm25_nuts2$category, mean),
#                         sd = tapply(pm25_nuts2$FWI, pm25_nuts2$category, sd)) |>
#   mutate(lab = paste0("Mean (SD): \n", round(mean, 2), " (", round(sd, 2), ")"))
# pm25means <- data.frame(category = c("low", "medium", "high"),
#                         mean = tapply(pm25_nuts2$pm25, pm25_nuts2$category, mean),
#                         sd = tapply(pm25_nuts2$pm25, pm25_nuts2$category, sd)) |>
#   mutate(lab = paste0("Mean (SD): \n",round(mean, 2), " (", round(sd, 2), ")")) 
# attrmeans <- data.frame(category = c("low", "medium", "high"),
#                         mean = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, mean),
#                         sd = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, sd)) |>
#   mutate(lab = paste0("Mean (SD): \n", round(mean, 2), " (", round(sd, 2), ")")) 


FWImeans <- data.frame(
  category = c("low", "medium", "high"),
  mean = tapply(pm25_nuts2$FWI, pm25_nuts2$category, mean, na.rm = T),
  sd   = tapply(pm25_nuts2$FWI, pm25_nuts2$category, sd,   na.rm = T)) %>% 
  mutate(lab = paste0("Mean (SD): \n", round(mean, 2), " (", round(sd, 2), ")"))

pm25means <- data.frame(
  category = c("low", "medium", "high"),
  mean = tapply(pm25_nuts2$pm25, pm25_nuts2$category, mean, na.rm = T),
  sd   = tapply(pm25_nuts2$pm25, pm25_nuts2$category, sd,   na.rm = T)) %>% 
  mutate(lab = paste0("Mean (SD): \n", round(mean, 2), " (", round(sd, 2), ")"))

attrmeans <- data.frame(
  category = c("low", "medium", "high"),
  mean = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, mean, na.rm = T),
  sd   = tapply(pm25_nuts2$attr_stand, pm25_nuts2$category, sd,   na.rm = T)) %>% 
  mutate(lab = paste0("Mean (SD): \n", round(mean, 2), " (", round(sd, 2), ")"))




# Plot
pm25_nuts2_fwi <- pm25_nuts2 %>%
  filter(!is.na(FWI)) %>% 
  filter(!is.na(category))
p1 <- ggboxplot(pm25_nuts2_fwi, x = "category", y = "FWI",
                color = "category", 
                fill = "category",
                palette = c("#5FA2D9", "#FF9F3A", "#FF6B63"),# c("#4E79A7", "#F28E2B", "#E15759"),
                alpha = 0.4, 
                add = "jitter",
                add.params = list(shape = 22, size = 1)) +
  scale_y_continuous(limits = c(1.5, 38)) +
  geom_text(data = FWImeans, aes(x = category, y = 2, label = lab), size = 3) +
  theme_bw() + 
  xlab("Deprivation") +
  ylab("Average FWI") +
  theme(legend.title = element_blank())

pm25_nuts2_pm25 <- pm25_nuts2 %>%
  filter(!is.na(pm25)) %>% 
  filter(!is.na(category))
p2 <- ggboxplot(pm25_nuts2_pm25, x = "category", y = "pm25",
                color = "category", 
                fill = "category",
                palette = c("#5FA2D9", "#FF9F3A", "#FF6B63"),
                alpha = 0.4,
                add = "jitter",
                add.params = list(shape = 22, size = 1)) +
  scale_y_continuous(limits = c(-0.1, 1.5)) +
  geom_text(data = pm25means, aes(x = category,  y = -0.08, label = lab), size = 3) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab(expression(Average~`wildfire-PM`[2.5]~(mu*g/m^3)))

pm25_nuts2_attr <- pm25_nuts2 %>%
  filter(!is.na(attr_stand)) %>% 
  filter(!is.na(category))
p3 <- ggboxplot(pm25_nuts2_attr, x = "category", y = "attr_stand",
                color = "category", 
                fill = "category",
                palette =c("#5FA2D9", "#FF9F3A", "#FF6B63"),
                alpha = 0.4,
                add = "jitter",
                add.params = list(shape = 22, size = 1)) +
  scale_y_continuous(limits = c(-2, 23)) +
  geom_text(data = attrmeans, aes(x = category,  y = -1.5, label = lab), size = 3) +
  theme_bw() + theme(legend.position = "none") +
  xlab("Deprivation") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K"))

p4 <- as_ggplot(get_legend(p1))
p1 <- p1 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/inequalities_nuts2.png", pall,  width = 8, height = 8, dpi = 300)

rm("p1", "p2", "pall", "FWImeans",
   "pm25means", "attrmeans", "pm25_nuts2", "FWI_nuts2", "pm25_nuts2_fwi", 
   "pm25_nuts2_pm25", "pm25_nuts2_attr", "ineq")


# 6. Yearly death counts in Europe ----

attreuro_main <- read_csv("data/processed/attributable_euro.csv") %>%
  rename(attr_stand = attr_100K,
         attrlower_stand = attrlower_100K,
         attrupper_stand = attrupper_100K) %>%
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                       ", ", round(attrupper_stand,2), ")")) %>%
  select(year, Ncountries, attr_main, attr_main_stand)

# # Sensitivity analysis - lag0-1
# attreuro_lag0to1 <- read_csv("data/processed/attributable_euro_sens_lag0-1.csv") %>% 
#   left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
#   mutate(attr_stand = (attr/population_sum*100000),
#          attrlower_stand = (attrlower/population_sum*100000),
#          attrupper_stand = (attrupper/population_sum*100000)) %>% 
#   mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
#                             ", ", round(attrupper,2), ")"),
#          attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
#                                   ", ", round(attrupper_stand,2), ")")) %>% 
#   select(year, attr_main, attr_main_stand)
# 
# # Sensitivity analysis - lag 0-7
# attreuro_lag0to7 <- read_csv("data/processed/attributable_euro_sens_lag0-7.csv") %>% 
#   left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
#   mutate(attr_stand = (attr/population_sum*100000),
#          attrlower_stand = (attrlower/population_sum*100000),
#          attrupper_stand = (attrupper/population_sum*100000)) %>% 
#   mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
#                             ", ", round(attrupper,2), ")"),
#          attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
#                                   ", ", round(attrupper_stand,2), ")")) %>% 
#   select(year, attr_main, attr_main_stand)
# 
# # Sensitivity analysis - RR for total PM2.5 mass
# attreuro_totpm25 <- read_csv("data/processed/attributable_euro_sens_RR_totalpm25.csv") %>% 
#   left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
#   mutate(attr_stand = (attr/population_sum*100000),
#          attrlower_stand = (attrlower/population_sum*100000),
#          attrupper_stand = (attrupper/population_sum*100000)) %>% 
#   mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
#                             ", ", round(attrupper,2), ")"),
#          attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
#                                   ", ", round(attrupper_stand,2), ")")) %>% 
#   select(year, attr_main, attr_main_stand)
# 
# # Sensitivity analysis - weekly RR and HIA
# attreuro_weekly <- read_csv("data/processed/attributable_euro_weekly.csv") %>% 
#   left_join(read_csv("data/processed/population_euro.csv"), by = c("year"="year")) %>% 
#   mutate(attr_stand = (attr/population_sum*100000),
#          attrlower_stand = (attrlower/population_sum*100000),
#          attrupper_stand = (attrupper/population_sum*100000)) %>% 
#   mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
#                             ", ", round(attrupper,2), ")"),
#          attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
#                                   ", ", round(attrupper_stand,2), ")")) %>% 
#   select(year, attr_main, attr_main_stand)
# 
# attr <- attreuro_main %>% 
#   full_join(attreuro_weekly, by = c("year" = "year")) %>% 
#   full_join(attreuro_lag0to1, by = c("year" = "year")) %>% 
#   full_join(attreuro_lag0to7, by = c("year" = "year")) %>% 
#   full_join(attreuro_totpm25, by = c("year" = "year"))
# 
# names(attr) <- c("Year", "Number of included countries", 
#                  "Attributable deaths (95% CI): lag0", "Attributable deaths/100K (95% CI): lag0", 
#                  "Attributable deaths (95% CI): Weekly HIA lag0", "Attributable deaths/100K (95% CI): Weekly HIA lag0",
#                  "Attributable deaths (95% CI): lag0-1", "Attributable deaths/100K (95% CI): lag0-1",
#                  "Attributable deaths (95% CI): lag0-7", "Attributable deaths/100K (95% CI): lag0-7",
#                  "Attributable deaths (95% CI): Total PM2.5", "Attributable deaths/100K (95% CI): Total PM2.5")

names(attreuro_main) <- c("Year", "Number of included countries", "Attributable deaths (95% CI): lag0", "Attributable deaths/100K (95% CI)")

write_csv(attreuro_main, "figures/attributable_euro.csv")
# rm(attreuro_main, attreuro_lag0to1, attreuro_lag0to7, attreuro_totpm25, attreuro_weekly, attr)
rm(attreuro_main)

# 7. Top 20 death counts 2003-2025 by NUTS 2 ----

attnuts <- read_csv("data/processed/attributable_nuts.csv")  %>% 
  rename(attr_stand = attr_100K,
         attrlower_stand = attrlower_100K,
         attrupper_stand = attrupper_100K) %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")")) %>% 
  # filter(NUTS_0!="DE") %>% # excluded because not at NUTS2 level
  select(year, NUTS_mort, NUTS_0, attr_stand, attr_main, attr_main_stand) 

# Add complete names
euroregions <- import("data/raw/regions/2027 Country names and groupings -2.xlsx", skip=1) %>% 
  dplyr::select(1,4)
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "NUTS_0")
euroregions <- euroregions %>% 
  rename(country = country_name)

attnuts <- left_join(attnuts, euroregions, by = "NUTS_0") %>% 
  arrange(-attr_stand) 

attnuts <- attnuts[1:20,]
attr <- attnuts %>% 
    select(year, NUTS_mort, country, attr_main, attr_main_stand)

names(attr) <- c("Year", "NUTS2", "Country", "Attributable deaths (95% CI)", "Attributable deaths/100K (95% CI)")
  
write_csv(attr, "figures/attributable_nuts_20_bydeathper100K.csv")
rm(attnuts, attr)


# clean
rm(list = ls())





