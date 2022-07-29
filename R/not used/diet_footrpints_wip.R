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

# load footprint functions
source("R/footprint_functions.R")

# select fabio version
vers <- "1.1" # or "1.2"

# should results be saved to file?
write = FALSE

# load FABIO data
Y_food_aut <- readRDS(paste0("data/v",vers,"/Y_food_aut_2013.rds"))
setkey(Y_food_aut, area_code, comm_code)
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/X.rds")) # total output

# load and prepare extensions
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E.rds")) # environmental extensions
# NOTE: emission extensions so far only available for v1.1!
E_ghg <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg_value.rds")
E_luh <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2_value.rds")
items_ghg <- read.csv("/mnt/nfs_fineprint/tmp/fabio/ghg/items_ghg.csv")
items_luh <- read.csv("/mnt/nfs_fineprint/tmp/fabio/ghg/items_luh2.csv")

# aggregate emission categories
E_ghg_agg <- lapply(E_ghg, colSums)
E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("100 years", items_luh$Element),])})

# bind with E table
E_all <- Map(function(e, e_ghg, e_luh){cbind(e,"ghg" = e_ghg*1000, "luh" = e_luh*1000)},E, E_ghg_agg, E_luh2_agg)

# read region classification
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/regions.csv"))
# read commodity classification
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
# create index of all region-commodity combinations
index <- data.table(country_code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$name, each = nrcom),
                    continent = rep(regions$continent, each = nrcom),
                    comm_code = rep(items$comm_code, nrreg),
                    item_code = rep(items$item_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg),
                    comm_group = rep(items$comm_group, nrreg))


# german group names
items_ger <- as.data.table(openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = ifelse(vers == "1.1", "items_german_1.1", "items_german")))

# colors for food groups
food_cols <- openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = "colors", colNames = FALSE)
food_cols_vect <- food_cols$X2
names(food_cols_vect) <- food_cols$X1


#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

# run calculations (see function library)
fp_sq_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$food_t_pc, X = X, E = E_all)
fp_eat_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$eat_t_pc, X = X, E = E_all)
fp_epo_2013 <- footprint(country = "AUT",  allocation = "value", year = 2013, y = Y_food_aut$epo_t_pc, X = X, E = E_all)


# add total emission footprints
fp_sq_2013[, ghg_all := ghg + luh]
fp_eat_2013[, ghg_all := ghg + luh]
fp_epo_2013[, ghg_all := ghg + luh]


# add german names
fp_sq_2013 <- merge(fp_sq_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_eat_2013 <- merge(fp_eat_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_epo_2013 <- merge(fp_epo_2013, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)


# aggregate as desired (see function library)
fp_sq_2013_agg <- fp_aggregate(fp_sq_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_eat_2013_agg <- fp_aggregate(fp_eat_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_epo_2013_agg <- fp_aggregate(fp_epo_2013, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))


fp_sq_2013_agg_crop <- fp_aggregate(fp_sq_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_eat_2013_agg_crop <- fp_aggregate(fp_eat_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_epo_2013_agg_crop <- fp_aggregate(fp_epo_2013[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))

fp_sq_2013_group  <- fp_aggregate(fp_sq_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_eat_2013_group <- fp_aggregate(fp_eat_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_epo_2013_group <- fp_aggregate(fp_epo_2013, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))

fp_sq_2013_continent_group <- fp_aggregate(fp_sq_2013, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_eat_2013_continent_group <- fp_aggregate(fp_eat_2013, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))
fp_epo_2013_continent_group <- fp_aggregate(fp_epo_2013, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_all", "biomass"))



#-------------------------------------------------------#
# ---------------- Create Visualizations ---------------
#-------------------------------------------------------#

diet_names <- c("sq" = "aktuellen Ernährung in Österreich",  "eat" = "EAT-Lancet Diät für Österreich", "epo" = "österreichischen Ernährunspyramide")
fp_list <- list (fp_)

for (diet in c("sq", "eat", "epo")){

## footprint map: impacts across the world ---------------------------------------------------------------------


# load world map shapefile
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

# create footprint maps

# land footprint of overall food consumption in AUT
(fp_map_landuse <- fp_map(fp = fp_sq_2013, map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL",
                              title = paste("Pro-Kopf Flächenfußabruck der", diet_names[diet])))


# blue water footprint of overall food consumption in AUT
(fp_map_blue <- fp_map(fp = fp_sq_2013, map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL",
                           title = paste("Pro-Kopf Süßwasserfußabdruck der", diet_names[diet])))


# emission footprint of overall food consumption in AUT
(fp_map_ghg <- fp_map(fp = fp_sq_2013, map = world_map, indicator = "ghg_all",
                           origin_items = "ALL", target_items = "ALL",
                           title = "Pro-Kopf Emissionsfußabruck der", diet_names[diet])))



# TODO: standardize legend scales between diets

if (write) {
# save maps to png
  ggsave("plots_diets/fp_map_land_sq.png", fp_map_landuse_sq, width = 15, height = 10, units = "cm")
  ggsave("plots_diets/fp_map_water_sq.png", fp_map_blue_sq, width = 15, height = 10, units = "cm")
  ggsave("plots_diets/fp_map_ghg_sq.png", fp_map_ghg_sq, width = 15, height = 10, units = "cm")
  
  ggsave("plots_diets/fp_map_land_eat.png", fp_map_landuse_eat, width = 15, height = 10, units = "cm")
  ggsave("plots_diets/fp_map_water_eat.png", fp_map_blue_eat, width = 15, height = 10, units = "cm")
  ggsave("plots_diets/fp_map_ghg_eat.png", fp_map_ghg_eat, width = 15, height = 10, units = "cm")
} 

## mosaic plot -------------------------------------------------------------------------------------------

# NOTE that the function can so far not add the continent names on the top x-axis like it is done here: https://iopscience.iop.org/article/10.1088/1748-9326/ab07f5
# this is due to an issue in the used package ggmosaic and will hopefully be resolved soon

(mosaic_land_sq <- fp_mosaic(fp = fp_sq_2013[group_origin != "Grazing",], indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1e-4, divide_by_axis = 1e-4, axis_label = expression(paste(m^2)),
                              display_min = 10, round_digs = 0,
                              plot_title = "Flächenfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

(mosaic_land_eat <- fp_mosaic(fp = fp_eat_2013[group_origin != "Grazing",], indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-4, divide_by_axis = 1e-4, axis_label = expression(paste(m^2)),
                             display_min = 10, round_digs = 0,
                             plot_title = "Flächenfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

# save plot to png
if (write){
ggsave("plots_diets/mosaic_land_sq.png", mosaic_land_sq, width = 25, height = 15, units = "cm")
ggsave("plots_diets/mosaic_land_eat.png", mosaic_land_eat, width = 25, height = 15, units = "cm")
}


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

# save plot to png
if (write) {
ggsave("plots_diets/mosaic_water_sq.png", mosaic_water_sq, width = 25, height = 15, units = "cm")
ggsave("plots_diets/mosaic_water_eat.png", mosaic_water_eat, width = 25, height = 15, units = "cm")
}



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

# save plot to png
if (write) {
ggsave("plots_diets/mosaic_ghg_sq.png", mosaic_ghg_sq, width = 25, height = 15, units = "cm")
ggsave("plots_diets/mosaic_ghg_eat.png", mosaic_ghg_eat, width = 25, height = 15, units = "cm")
}



##  barchart of diet compositions --------------------------------------

Y_food_aut <- merge(Y_food_aut, items_ger[,.(item_code,comm_group_ger)], by = c("item_code"), all.x = TRUE, sort = TRUE)
#Y_agg <- Y_food_aut[,.(food_g_pc_day_net = sum(food_g_pc_day_net, na.rm = T), eat_g_pc_day_net = sum(eat_g_pc_day_net, na.rm = T)), by =  comm_group_ger]
Y_agg <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_ger, .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net")]#, "food_kcal_pc_day_net", "eat_kcal_pc_day_net")]
Y_agg_long <- Y_agg %>%
  rename_with(~gsub("_g_pc_day_net", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = "diet", cols = c(food:epo), values_to = "g_pc_day") %>%
  filter(g_pc_day > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "EAT Lancet", "Ernährungspyramide")))

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
ggsave(filename = "plots_diets/diet_plot.svg", diet_plot, width = 12, height = 3.5)
}


## comparison plot of footprints with per-capita planetary boundaries -------------------

# NOTE: this will become the spiderweb chart once we have all indicators

land_ha = 13 * 1e6 / 10e9 * 100
water_m3 = 2500 / 10e9 * 1e9
ghg_t = 5 * 1e9 / 10e9

# aggregate indicators
fp_agg <- as.data.frame(rbind(fp_sq_2013_agg, fp_eat_2013_agg, fp_epo_2013_agg))
fp_agg_land <- as.data.frame(rbind(fp_sq_2013_agg_crop, fp_eat_2013_agg_crop, fp_epo_2013_agg_crop))

fp_agg$diet <- factor(c("Status \nQuo", "EAT \nLancet", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "EAT \nLancet", "Ernährungs- \npyramide"))
fp_agg_land$diet <- factor(c("Status \nQuo", "EAT \nLancet", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "EAT \nLancet", "Ernährungs- \npyramide"))


(pb_bar_land <- ggplot(fp_agg_land, aes(x = diet, y = landuse)) +
  geom_bar(stat="identity", fill = "#176040") +
  geom_abline(intercept = land_ha, slope = 0, color = "red") +
  labs(y = "Flächenverbrauch in ha", x = "") +
  theme_minimal())

(pb_bar_water <- ggplot(fp_agg, aes(x = diet, y = blue)) +
    geom_bar(stat="identity", fill = "#293969") +
    geom_abline(intercept = water_m3, slope = 0, color = "red") +
    labs(y = "Wasserverbrauch in m<sup>3</sup>", x = "")+
    coord_cartesian(ylim = c(0, 250))+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_ghg <- ggplot(fp_agg, aes(x = diet, y = ghg_all)) +
    geom_bar(stat="identity", fill = "#521f11") +
    geom_abline(intercept = ghg_t, slope = 0, color = "red") +
    labs(y = "Emissionen in t CO<sub>2</sub>-eq.", x = "")+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar <- pb_bar_land + pb_bar_water + pb_bar_ghg)

if (write) {
ggsave("plots_diets/pb_bar.svg", pb_bar, width = 15, height = 12, units = "cm")
}

