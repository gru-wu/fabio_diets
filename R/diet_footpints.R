# diet footprints

library(Matrix)
library(tidyverse)
library(data.table)
library(sf)
library(fmsb)
library(tidyverse)
library(magrittr)
library(ggmosaic)
library(rworldmap)
library(wbstats)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(patchwork)
library(viridis)

# load footprint functions
source("R/footprint_functions.R")

# select fabio version
vers <- "1.1" # or "1.2"

# should results be saved to file?
write = TRUE

# load FABIO data
Y_food_aut <- readRDS(paste0("data/v",vers,"/Y_food_aut_2013.rds"))
setkey(Y_food_aut, area_code, comm_code)
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/X.rds")) # total output

# load and prepare extensions
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E.rds")) # environmental extensions
E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp.rds"))
E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh.rds"))
items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp_names.csv"))
items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh_names.csv"))

# aggregate emission categories
E_ghg_agg <- lapply(E_ghg, colSums)
E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("5 years", items_luh$Element),])})

# bind with E table
E_all <- Map(function(e, e_ghg, e_luh){cbind(e,"ghg" = e_ghg*1000, "luh" = e_luh*1000)},E, E_ghg_agg, E_luh2_agg)

# read region classification
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/regions.csv"))
# read commodity classification
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
# create index of all region-commodity combinations
index <- data.table(area_code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    area = rep(regions$name, each = nrcom),
                    continent = rep(regions$continent, each = nrcom),  # v1.1
                    # continent = rep(regions$region, each = nrcom),  # v1.2
                    comm_code = rep(items$comm_code, nrreg),
                    item_code = rep(items$item_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg),
                    comm_group = rep(items$comm_group, nrreg))

# German group names
items_ger <- as.data.table(openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = ifelse(vers == "1.1", "items_german_1.1", "items_german")))
Y_food_aut <- merge(Y_food_aut, items_ger[,.(item_code,comm_group_ger)], by = c("item_code"), all.x = TRUE, sort = TRUE)

# colors for food groups
food_cols <- openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = "colors", colNames = FALSE)
food_cols_vect <- food_cols$X2
names(food_cols_vect) <- food_cols$X1


#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

# run calculations (see function library)
fp_sq_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$food_t_pc, X = X, E = E_all, v = vers)
fp_eat_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$eat_t_pc, X = X, E = E_all, v = vers)
fp_epo_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$epo_t_pc, X = X, E = E_all, v = vers)


#######.
# NOTE:
#for future versions of this script, it will be better to set an lapply here the generates the results/visualizations for each diet, instead of repeating the code for each diet
######.

# add total emission footprints
fp_sq_2013[, ghg_all := ghg + luh]
fp_eat_2013[, ghg_all := ghg + luh]
fp_epo_2013[, ghg_all := ghg + luh]

# remove pastures from landuse
fp_sq_2013$landuse[fp_sq_2013$item_origin=="Grazing"] <- 0
fp_eat_2013$landuse[fp_eat_2013$item_origin=="Grazing"] <- 0
fp_epo_2013$landuse[fp_epo_2013$item_origin=="Grazing"] <- 0

# convert landuse from ha to m2
fp_sq_2013$landuse <- fp_sq_2013$landuse * 10000
fp_eat_2013$landuse <- fp_eat_2013$landuse * 10000
fp_epo_2013$landuse <- fp_epo_2013$landuse * 10000

# add german names
fp_sq_2013 <- merge(fp_sq_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_eat_2013 <- merge(fp_eat_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_epo_2013 <- merge(fp_epo_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)

# save results
if (write){ 
  saveRDS(fp_sq_2013, "./plots/fp_sq_2013.rds")
  saveRDS(fp_eat_2013, "./plots/fp_eat_2013.rds")
  saveRDS(fp_epo_2013, "./plots/fp_epo_2013.rds")
}

# aggregate as desired (see function library)
fp_sq_2013_agg <- fp_aggregate(fp_sq_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_2013_agg <- fp_aggregate(fp_eat_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_2013_agg <- fp_aggregate(fp_epo_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_2013_agg_crop <- fp_aggregate(fp_sq_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_2013_agg_crop <- fp_aggregate(fp_eat_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_2013_agg_crop <- fp_aggregate(fp_epo_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_2013_group  <- fp_aggregate(fp_sq_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_2013_group <- fp_aggregate(fp_eat_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_2013_group <- fp_aggregate(fp_epo_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_2013_continent_group <- fp_aggregate(fp_sq_2013, aggregate_by = c("continent_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_2013_continent_group <- fp_aggregate(fp_eat_2013, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_2013_continent_group <- fp_aggregate(fp_epo_2013, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_2013_country <- fp_aggregate(fp_sq_2013, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_2013_country <- fp_aggregate(fp_eat_2013, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_2013_country <- fp_aggregate(fp_epo_2013, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_limits <- rbind(fp_sq_2013_country, fp_eat_2013_country, fp_epo_2013_country) %>% 
                        filter(country_origin != "AUT") %>% 
                        group_by(country_consumer) %>% 
                        summarise(across(landuse:ghg_all, max))

#-------------------------------------------------------#
# ---------------- Create Visualizations ---------------
#-------------------------------------------------------#

## Footprint map: impacts across the world ---------------------------------------------------------------------


# load world map shapefile
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

# create footprint maps

### Cropland ------

# land footprint of overall food consumption in AUT
(fp_map_landuse_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                              title = "Pro-Kopf Flächenfußabruck der aktuellen Ernährung in Österreich"))
(fp_map_landuse_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "landuse",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                             title = "Pro-Kopf Flächenfußabruck der Planetary Health Diet für Österreich"))
(fp_map_landuse_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                              title = "Pro-Kopf Flächenfußabruck der österreichischen Ernährungspyramide"))

## land footprint of overall Meat consumption in AUT
#(fp_map_landuse_meat <- fp_map(fp = fp, map = world_map, indicator = "landuse",
#                               target_groups = "Meat",
#                               title = "Pro-Kopf Flächenfußabruck des aktuellen Fleischkonsums in Österreich"))

### Water ------
(fp_map_blue_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL",  limits = c(0, fp_limits$blue),
                           title = "Pro-Kopf Süßwasserfußabdruck der aktuellen Ernährung in Österreich"))

(fp_map_blue_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "blue",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                          title = "Pro-Kopf Süßwasserfußabdruck der Planetary Health Diet für Österreich"))

(fp_map_blue_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                           title = "Pro-Kopf Süßwasserfußabdruck der österreichischen Ernährungspyramide"))

# emission footprint
(fp_map_ghg_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                           title = "Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich"))

(fp_map_ghg_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                         origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                         title = "Pro-Kopf Emissionsfußabruck der Planetary Health Diet für Österreich"))

(fp_map_ghg_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                          title = "Pro-Kopf Emissionsfußabruck der österreichischen Ernährungspyramide"))

### Biodiversity ------
(fp_map_biodiv_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                         origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                         title = "Pro-Kopf Biodiversitätsfußabdruck der aktuellen Ernährung in Österreich"))
(fp_map_biodiv_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                          title = "Pro-Kopf Biodiversitätsfußabdruck der Planetary Health Diet für Österreich"))
(fp_map_biodiv_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                          title = "Pro-Kopf Biodiversitätsfußabdruck der österreichischen Ernährungspyramide"))

### N application ------
(fp_map_n_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "n_application",
                            origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                            title = "Pro-Kopf Stickstoffeinsatz der aktuellen Ernährung in Österreich"))
(fp_map_n_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "n_application",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                             title = "Pro-Kopf Stickstoffeinsatz der Planetary Health Diet für Österreich"))
(fp_map_n_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "n_application",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                             title = "Pro-Kopf Stickstoffeinsatz der österreichischen Ernährungspyramide"))

### P application ------
(fp_map_p_sq <- fp_map(fp = fp_sq_2013[fp_sq_2013$country_origin != "AUT"], map = world_map, indicator = "p_application",
                       origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                       title = "Pro-Kopf Phosphoreinsatz der aktuellen Ernährung in Österreich"))
(fp_map_p_eat <- fp_map(fp = fp_eat_2013[fp_eat_2013$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = "Pro-Kopf Phosphoreinsatz der Planetary Health Diet für Österreich"))
(fp_map_p_epo <- fp_map(fp = fp_epo_2013[fp_epo_2013$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = "Pro-Kopf Phosphoreinsatz der österreichischen Ernährungspyramide"))

### save maps ------------

if (write) {
  ggsave("plots/map/map_land_sq.png", fp_map_landuse_sq, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_water_sq.png", fp_map_blue_sq, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_ghg_sq.png", fp_map_ghg_sq, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_biodiv_sq.png", fp_map_biodiv_sq, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_n_sq.png", fp_map_n_sq, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_p_sq.png", fp_map_p_sq, width = 15, height = 10, units = "cm")
  
  ggsave("plots/map/map_land_eat.png", fp_map_landuse_eat, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_water_eat.png", fp_map_blue_eat, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_ghg_eat.png", fp_map_ghg_eat, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_biodiv_eat.png", fp_map_biodiv_eat, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_n_eat.png", fp_map_n_eat, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_p_eat.png", fp_map_p_eat, width = 15, height = 10, units = "cm")
  
  ggsave("plots/map/map_land_epo.png", fp_map_landuse_epo, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_water_epo.png", fp_map_blue_epo, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_ghg_epo.png", fp_map_ghg_epo, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_biodiv_epo.png", fp_map_biodiv_epo, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_n_epo.png", fp_map_n_epo, width = 15, height = 10, units = "cm")
  ggsave("plots/map/map_p_epo.png", fp_map_p_epo, width = 15, height = 10, units = "cm")
} 


## Mosaic plot -------------------------------------------------------------------------------------------

# this uses the fp_mosaic function from the function library. It has the following arguments:

# fp = the footprint table (can be the raw fp_country table without any aggregation)
# indicator = the indicator to be plotted
# per_capita = should results be by capita or aggregate (TRUE/FALSE)
# target_items = optional, a vector of target items to subset results
# target_groups = optional, a vector of target commodity groups to subset results
# divide_by_cells = a factor by which the values displayed in the cells are divided (default 1000)
# divide_by_axis = a factor by which the values displayed on the x-axis are divided (default 1000000)
# display_min = a threshold defining the minimum value to be plotted in the cells to avoid overplotting
# axis_label = character string giving the x-axis label (depends on indicator chose, e.g. "Million hectares")
# plot_title = character string giving the title of the plot
# tick_offset = a vector of length 9 offsetting the tick marks on the x-axis. This has to be adapted manually for each plot

# NOTE that the function can so far not add the continent names on the top x-axis like it is done here: https://iopscience.iop.org/article/10.1088/1748-9326/ab07f5
# this is due to an issue in the used package ggmosaic and will hopefully be resolved soon

### Cropland --------------------------------
(mosaic_land_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^2)),
                             display_min = 10, round_digs = 0,
                             plot_title = "Flächenfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

(mosaic_land_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^2)),
                             display_min = 10, round_digs = 0,
                             plot_title = "Flächenfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

(mosaic_land_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^2)),
                              display_min = 10, round_digs = 0,
                              plot_title = "Flächenfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))


### Water --------------------------------
(mosaic_water_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^3)),
                             display_min = 0.5, round_digs = 2,
                             plot_title = "Wasserfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^3)),
                              display_min = 0.5, round_digs = 2,
                              plot_title = "Wasserfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                               divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste(m^3)),
                               display_min = 0.5, round_digs = 2,
                               plot_title = "Wasserfußabruck je Region und Produktgruppe",
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))


### GHG --------------------------------
(mosaic_ghg_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "kg CO2-eq.",
                              display_min = 10, round_digs = 0,
                              plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                               divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "kg CO2-eq.",
                               display_min = 10, round_digs = 0,
                               plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "kg CO2-eq.",
                             display_min = 10, round_digs = 0,
                             plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### Biodiversity --------------------------------
(mosaic_biodiv_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                            divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "10^-9 Arten",
                            display_min = 10, round_digs = 0,
                            plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                            tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_biodiv_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "10^-9 Arten",
                             display_min = 10, round_digs = 0,
                             plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_biodiv_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "10^-9 Arten",
                             display_min = 10, round_digs = 0,
                             plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001)))

### N Application --------------------------------
(mosaic_n_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                            divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g N",
                            display_min = 10, round_digs = 0,
                            plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                            tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g N",
                           display_min = 10, round_digs = 0,
                           plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g N",
                           display_min = 10, round_digs = 0,
                           plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### P Application --------------------------------
(mosaic_p_sq <- fp_mosaic(fp = fp_sq_2013, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                          divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g P",
                          display_min = 10, round_digs = 0,
                          plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                          tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_eat <- fp_mosaic(fp = fp_eat_2013, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g P",
                           display_min = 10, round_digs = 0,
                             plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_epo <- fp_mosaic(fp = fp_epo_2013, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "g P",
                           display_min = 10, round_digs = 0,
                             plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))
### save plots---------
if (write) {
  
  ggsave("plots/mosaic/mosaic_land_sq.png", mosaic_land_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_land_eat.png", mosaic_land_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_land_epo.png", mosaic_land_epo, width = 25, height = 15, units = "cm")
  
  
  ggsave("plots/mosaic/mosaic_water_sq.png", mosaic_water_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_water_eat.png", mosaic_water_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_water_epo.png", mosaic_water_epo, width = 25, height = 15, units = "cm")
  
  ggsave("plots/mosaic/mosaic_ghg_sq.png", mosaic_ghg_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_ghg_eat.png", mosaic_ghg_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_ghg_epo.png", mosaic_ghg_epo, width = 25, height = 15, units = "cm")
  
  ggsave("plots/mosaic/mosaic_biodiv_sq.png", mosaic_biodiv_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_biodiv_eat.png", mosaic_biodiv_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_biodiv_epo.png", mosaic_biodiv_epo, width = 25, height = 15, units = "cm")
  
  ggsave("plots/mosaic/mosaic_n_sq.png", mosaic_n_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_n_eat.png", mosaic_n_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_n_epo.png", mosaic_n_epo, width = 25, height = 15, units = "cm")
  
  ggsave("plots/mosaic/mosaic_p_sq.png", mosaic_p_sq, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_p_eat.png", mosaic_p_eat, width = 25, height = 15, units = "cm")
  ggsave("plots/mosaic/mosaic_p_epo.png", mosaic_p_epo, width = 25, height = 15, units = "cm")
}


##  Barchart of diet compositions --------------------------------------

#Y_agg <- Y_food_aut[,.(food_g_pc_day_net = sum(food_g_pc_day_net, na.rm = T), eat_g_pc_day_net = sum(eat_g_pc_day_net, na.rm = T)), by =  comm_group_ger]
Y_agg <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_ger, .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net")]#, "food_kcal_pc_day_net", "eat_kcal_pc_day_net")]
Y_agg_long <- Y_agg %>%
  rename_with(~gsub("_g_pc_day_net", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = "diet", cols = c(food:epo), values_to = "g_pc_day") %>%
  filter(g_pc_day > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", "Ernährungspyramide")))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg_long$comm_group_ger)]

# plot
(diet_plot <- ggplot(Y_agg_long, aes(x = diet_lab, y = g_pc_day, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
  geom_bar(stat="identity", alpha = 0.85) +
  scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
  labs(y = "Gramm pro Kopf und Tag", x = "") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
        axis.text.y = element_text(face = "bold", size = 10)))

if (write) {
  ggsave(filename = "plots/diet_plot.png", diet_plot, width = 12, height = 3.5)
}



## Stacked barcharts for footprints by consumption items ---------------------

indicators <- c("landuse", "ghg_all", "blue", "biodiv", "n_application", "p_application")
indicator_labs <- c(landuse = "Flächenverbrauch in m<sup>2</sup>",
                    ghg_all = "Emissionen in t CO<sub>2</sub>-eq.",
                    blue = "Wasserverbrauch in m<sup>3</sup>",
                    biodiv = "Biodiversitätsverlust in 10<sup>-6</sup> Arten",
                    p_application =  "Phosphoreinsatz in kg",
                    n_application = "Stickstoffeinsatz in kg")

pb_stack_list <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp_sq_2013,  "eat" = fp_eat_2013,  "epo" = fp_epo_2013), 
               indicator = ind, axis_lab = indicator_labs[ind])
}, simplify = FALSE, USE.NAMES = TRUE)

pb_stack <- wrap_plots(pb_stack_list, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom")

if(write) ggsave("plots/pb_stack.png", pb_stack, width = 30, height = 25, units = "cm")


## Comparison plot of footprints with per-capita planetary boundaries -------------------

# NOTE: this will become the spiderweb chart once we have all indicators

pbs <- c(
  #"land_ha" = 13 * 1e6 / 10e9 * 100,
  "land_m2" = 13 * 1e6 / 10e9 * 1e6,
  "water_m3" = 2500 / 10e9 * 1e9,
  "ghg_t" = 5 * 1e9 / 10e9,
  "biodiv_species" = 10 / 10e9 * 10e6,
  "n_appl_kg" = 90 * 1e9 / 10e9,
  "p_appl_kg" = 8 * 1e9 / 10e9
)

# aggregate indicators
fp_agg <- as.data.frame(rbind(fp_sq_2013_agg, fp_eat_2013_agg, fp_epo_2013_agg))
fp_agg_land <- as.data.frame(rbind(fp_sq_2013_agg_crop, fp_eat_2013_agg_crop, fp_epo_2013_agg_crop))

fp_agg$diet <- factor(c("Status \nQuo", "Planetary \nHealth", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "Planetary \nHealth", "Ernährungs- \npyramide"))
fp_agg_land$diet <- factor(c("Status \nQuo", "Planetary \nHealth", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "Planetary \nHealth", "Ernährungs- \npyramide"))


(pb_bar_land <- ggplot(fp_agg_land, aes(x = diet, y = landuse)) + #/ 10000
  # geom_bar(stat="identity", fill = "#176040") +
    geom_bar(stat="identity", fill = viridis(6)[1]) +
    geom_abline(intercept = pbs["land_m2"], slope = 0, color = "red") +
  labs(y = "Flächenverbrauch in m<sup>2</sup>", x = "") +
  theme_minimal()+
  theme(axis.title.y = element_markdown()))

(pb_bar_water <- ggplot(fp_agg, aes(x = diet, y = blue)) +
    # geom_bar(stat="identity", fill = "#293969") +
    geom_bar(stat="identity", fill = viridis(6)[2]) +
    geom_abline(intercept = pbs["water_m3"], slope = 0, color = "red") +
    labs(y = "Wasserverbrauch in m<sup>3</sup>", x = "")+
    coord_cartesian(ylim = c(0, 250))+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_ghg <- ggplot(fp_agg, aes(x = diet, y = ghg_all)) +
    # geom_bar(stat="identity", fill = "#521f11") +
    geom_bar(stat="identity", fill = viridis(6)[3]) +
    geom_abline(intercept = pbs["ghg_t"], slope = 0, color = "red") +
    labs(y = "Emissionen in t CO<sub>2</sub>-eq.", x = "")+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_biodiv <- ggplot(fp_agg, aes(x = diet, y = biodiv)) +
    geom_bar(stat="identity", fill = viridis(6)[4]) +
    geom_abline(intercept = pbs["biodiv_species"], slope = 0, color = "red") +
    labs(y = "Biodiversitätsverlust in 10^-6 Arten", x = "")+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_n <- ggplot(fp_agg, aes(x = diet, y = n_application)) +
    geom_bar(stat="identity", fill = viridis(6)[5]) +
    geom_abline(intercept = pbs["n_appl_kg"], slope = 0, color = "red") +
    labs(y = "Stickstoffeinsatz in kg", x = "")+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_p <- ggplot(fp_agg, aes(x = diet, y = p_application)) +
    geom_bar(stat="identity", fill = viridis(6)[6]) +
    geom_abline(intercept = pbs["p_appl_kg"], slope = 0, color = "red") +
    labs(y = "Phosphoreinsatz in kg", x = "")+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar <- pb_bar_land + pb_bar_water + pb_bar_ghg + pb_bar_biodiv + pb_bar_n + pb_bar_p)

if (write) {
  ggsave("plots/pb_bar.png", pb_bar, width = 15, height = 12, units = "cm", scale = 1.5)
}


## Spiderweb chart -----------------------

fp_spider <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

# rescale by pb values
fp_spider[,2:7] <- t(t(fp_spider[,2:7])/pbs)
fp_spider <- mutate(fp_spider, Biodiversität = Biodiversität/10)

fp_spider_log <- fp_spider
fp_spider_log[,2:7] <- log(fp_spider_log[,2:7])


(pb_spider <- spiderweb(fp_spider, 
          grid.max = max(fp_spider[,2:7], na.rm = TRUE), grid.min = 0, grid.mid = 1,
          grid.max.label = "", grid.min.label = "", grid.mid.label = "",
          gridline.mid.linetype = "solid",
          gridline.max.linetype = "solid",
          gridline.min.linetype = "solid",
          gridline.mid.colour = "red",
          background.circle.colour = "transparent",
          legend.title = "",
          centre.y = -0.5,
          plot.extent.x.sf = 2,
          group.line.width = 1,
          group.point.size = 2,
          group.alpha = 0.7,
          axis.line.colour="grey",
          axis.line.alpha = 0.6))

if (write) ggsave("plots/pb_spiderweb.png", pb_spider, width = 10, units = "cm", scale = 1.5)


## Circular planetary boundary chart (experimental) -------------

pb_circle <- sapply(c("sq", "eat", "epo"), circle_plot, fp_table = fp_spider, log = FALSE,
                    simplify = FALSE, USE.NAMES = TRUE)
pb_circle <- wrap_plots(pb_circle)

if (write) ggsave("plots/pb_circle.png", pb_circle, width = 30, units = "cm", scale = 1)
