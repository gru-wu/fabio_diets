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
library(openxlsx)

# load footprint functions
source("R/footprint_functions.R")

# select fabio version
vers <- "1.1" # or "1.1"
yr = 2013

# should results be saved to file?
write = FALSE

# load FABIO data
Y_food_aut <- readRDS(paste0("data/v",vers,"/Y_food_aut_",yr,".rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/X.rds")) # total output

# load and prepare extensions
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E.rds")) # environmental extensions
if (vers == "1.1"){
  E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp.rds"))
  E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh.rds"))
  items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp_names.csv"))
  items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh_names.csv"))
  E_biodiv <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_biodiv.rds"))
  items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/biodiv_codes.csv"))
} else if (vers == "1.2"){
  E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_gwp_value.rds"))
  E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_luh_value.rds"))
  items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp_names.csv"))
  items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh_names.csv"))
  E_biodiv <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_biodiv.rds"))
  items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/biodiv_codes.csv"))
}

# aggregate emission categories
E_ghg_agg <- lapply(E_ghg, colSums)
E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("5 years", items_luh$Element),])})
E_ghg_pb <- lapply(E_ghg, function(x){colSums(x[!grepl("Energy use", items_ghg$Element),])})

# convert potential species loss to E/MSY
#if (vers == "1.2"){
items_biodiv <- items_biodiv[items_biodiv$land %in% c("cropland", "pasture"),]
E_biodiv <- lapply(E_biodiv, function(x){
  x <- t(t(x[,8:17]) / 1000000 / (items_biodiv$number / 1000000) / 200) # in 10e-6 species! if we want it in absolute species: t(t(x[,8:27]) / 1000000 / (items_biodiv$number / 1000000) / 200)
  colnames(x) <- items_biodiv$land
  x <- agg(x)
})
#}

# bind with E table
#if(vers == "1.1"){
#  E_all <- Map(function(e, e_ghg, e_luh){
#    cbind(e, "ghg" = e_ghg*1000, "luh" = e_luh*1000)
#    }, E, E_ghg_agg, E_luh2_agg)
#  } else if(vers == 1.2){
E_all <- Map(function(e, e_biodiv, e_ghg, e_luh, e_ghg_pb){
  #cbind(e, "biodiv" = e_biodiv[,"cropland"]+e_biodiv[,"pasture"], "ghg" = e_ghg*1000, "luh" = e_luh*1000, "ghg_pb" = e_ghg_pb*1000, "ghg_all" = e_ghg*1000+e_luh*100)
  cbind(e, "biodiv" = 0, "ghg" = e_ghg*1000, "luh" = e_luh*1000, "ghg_pb" = e_ghg_pb*1000, "ghg_all" = e_ghg*1000+e_luh*1000)
  
}, E, E_biodiv, E_ghg_agg, E_luh2_agg, E_ghg_pb)
#}

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
                    continent = rep(regions[[ifelse(vers == "1.1", "continent", "continent")]], each = nrcom),
                    comm_code = rep(items$comm_code, nrreg),
                    item_code = rep(items$item_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg),
                    comm_group = rep(items$comm_group, nrreg))

# German group names
items_ger <- as.data.table(openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = ifelse(vers == "1.1", "items_german_1.1", "items_german")))
Y_food_aut <- merge(Y_food_aut, items_ger[,.(item_code,comm_group_ger)], by = c("item_code"), all.x = TRUE, sort = FALSE)
setkey(Y_food_aut, area_code, comm_code)

# colors for food groups
food_cols <- openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = "colors_alt", colNames = FALSE)
food_cols_vect <- food_cols$X2
names(food_cols_vect) <- food_cols$X1

## per-capita planetary bondaries from Willett et al
#pbs <- c(
#  #"land_ha" = 13 * 1e6 / 10e9 * 100,
#  "landuse" = 13 * 1e6 / 10e9 * 1e6, # in m2
#  "blue" = 2500 / 10e9 * 1e9, # in m3
#  "ghg_all" = 5 * 1e9 / 10e9, # in t
#  "biodiv" = 10 / 10e9 * 10e6, # in 10-6 species
#  "n_application" = 90 * 1e9 / 10e9, # in kg
#  "p_application" = 8 * 1e9 / 10e9 # in kg
#)

# global planetary boundaries (lower bound - upper bound)
pbs_global <- rbind(
  #"boundary" = c("lower", "upper"),
  "landuse" = c(11, 13, 15),
  "blue" = c(1000, 2500, 4000),
  "ghg_all" = c(4.7, 5, 5.4),
  "biodiv" = c(1, 10, 80),
  "n_application" = c(65, 90, 130),
  "p_application" = c(6, 8, 16)
)
colnames(pbs_global) <- c("lower", "boundary",  "upper")

# transform into per-capita values in appropriate units
per_cap_factor <- c(  
  "landuse" = 1e6 / 10e9 * 1e6, # from mio km2 to m2 pc
  "blue" = 1e9 / 10e9  , # from km3 to m3 pc
  "ghg_all" =  1e9 / 10e9, # from Gt to t pc
  #"biodiv" = 1e6 / 10e9, # from species to 10-6 species pc
  "biodiv" = 1 / 10e9, # from species to species pc
  "n_application" = 1e9 / 10e9, # from Tg to kg pc
  "p_application" = 1e9 / 10e9 # from Tg to kg pc
)

pbs <- pbs_global*per_cap_factor

#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

# run calculations (see function library)
fp_sq <- footprint(country = "AUT",  allocation = "value", year = yr, y = Y_food_aut$food_t_pc, X = X, E = E_all, v = vers)
fp_eat <- footprint(country = "AUT",  allocation = "value", year = yr, y = Y_food_aut$eat_t_pc, X = X, E = E_all, v = vers)
fp_epo <- footprint(country = "AUT",  allocation = "value", year = yr, y = Y_food_aut$epo_t_pc, X = X, E = E_all, v = vers)


#######.
# NOTE:
#for future versions of this script, it will be better to set an lapply here the generates the results/visualizations for each diet, instead of repeating the code for each diet
######.

# add total emission footprints (Note: not necessary if extension would be aggregated already)
#fp_sq[, ghg_all := ghg + luh]
#fp_eat[, ghg_all := ghg + luh]
#fp_epo[, ghg_all := ghg + luh]

# remove pastures from landuse
fp_sq$landuse[fp_sq$item_origin=="Grazing"] <- 0
fp_eat$landuse[fp_eat$item_origin=="Grazing"] <- 0
fp_epo$landuse[fp_epo$item_origin=="Grazing"] <- 0

# convert landuse from ha to m2
fp_sq$landuse <- fp_sq$landuse * 10000
fp_eat$landuse <- fp_eat$landuse * 10000
fp_epo$landuse <- fp_epo$landuse * 10000

# add german names
fp_sq <- merge(fp_sq, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_eat <- merge(fp_eat, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
fp_epo <- merge(fp_epo, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)

# save results
# if (write){ 
#   saveRDS(fp_sq, paste0("./plots/v",vers,"/fp_sq_",yr,".rds"))
#   saveRDS(fp_eat, paste0("./plots/v",vers,"/fp_eat_",yr,".rds"))
#   saveRDS(fp_epo, paste0("./plots/v",vers,"/fp_epo_",yr,".rds"))
# }

# aggregate as desired (see function library)
fp_sq_agg <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_agg <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_agg <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

#fp_sq_agg_crop <- fp_aggregate(fp_sq[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
#fp_eat_agg_crop <- fp_aggregate(fp_eat[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
#fp_epo_agg_crop <- fp_aggregate(fp_epo[group_origin != "Grazing",], aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_group  <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_group <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_group <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_continent_group <- fp_aggregate(fp_sq, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_continent_group <- fp_aggregate(fp_eat, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_continent_group <- fp_aggregate(fp_epo, aggregate_by = c("continent_origin", "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_continent <- fp_aggregate(fp_sq, aggregate_by = c("continent_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))


fp_sq_country <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_country <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_country <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_country_group <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "country_origin",  "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_country_group <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer", "country_origin",  "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_country_group <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer", "country_origin",  "comm_group_ger"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

fp_sq_continent_group_orig <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "continent_origin",  "item_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_sq_country_item <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "country_origin",  "item_target"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))


# determine indicator-specific limits for plots to be used across all scenarios
fp_limits <- rbind(fp_sq_country, fp_eat_country, fp_epo_country) %>% 
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
(fp_map_landuse_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "landuse",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                             title = "")) # Pro-Kopf Flächenfußabruck der aktuellen Ernährung in Österreich
(fp_map_landuse_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                              title = "")) # Pro-Kopf Flächenfußabruck der Planetary Health Diet für Österreich
(fp_map_landuse_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "landuse",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$landuse),
                              title = "")) #Pro-Kopf Flächenfußabruck der österreichischen Ernährungspyramide

## land footprint of overall Meat consumption in AUT
#(fp_map_landuse_meat <- fp_map(fp = fp, map = world_map, indicator = "landuse",
#                               target_groups = "Meat",
#                               title = "Pro-Kopf Flächenfußabruck des aktuellen Fleischkonsums in Österreich"))

### Water ------
(fp_map_blue_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "blue",
                          origin_items = "ALL", target_items = "ALL",  limits = c(0, fp_limits$blue),
                          title = "")) # Pro-Kopf Süßwasserfußabdruck der aktuellen Ernährung in Österreich

(fp_map_blue_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                           title = "")) # Pro-Kopf Süßwasserfußabdruck der Planetary Health Diet für Österreich

(fp_map_blue_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "blue",
                           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$blue),
                           title = "")) # Pro-Kopf Süßwasserfußabdruck der österreichischen Ernährungspyramide

### Emissions -----------
## emission footprint (all emissions)
(fp_map_ghg_all_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = "")) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_all_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                              title = ""))

(fp_map_ghg_all_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg_all",
                              origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                              title = ""))

# emission footprint (only emissions relevat for boundary)
(fp_map_ghg_pb_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                            origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                            title = "")) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_pb_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = ""))

(fp_map_ghg_pb_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg_pb",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                             title = ""))

# emission footprint (all emissions excluding luc)
(fp_map_ghg_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "ghg",
                         origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                         title = "")) # Pro-Kopf Emissionsfußabruck der aktuellen Ernährung in Österreich

(fp_map_ghg_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "ghg",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                          title = ""))

(fp_map_ghg_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "ghg",
                          origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$ghg_all),
                          title = ""))


### Biodiversity ------
(fp_map_biodiv_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                            origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                            title = "")) # Pro-Kopf Biodiversitätsfußabdruck der aktuellen Ernährung in Österreich
(fp_map_biodiv_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                             title = ""))
(fp_map_biodiv_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "biodiv",
                             origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$biodiv),
                             title = ""))

### N application ------
(fp_map_n_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "n_application",
                       origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                       title = "")) # Pro-Kopf Stickstoffeinsatz der aktuellen Ernährung in Österreich
(fp_map_n_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "n_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                        title = ""))
(fp_map_n_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "n_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$n_application),
                        title = ""))

### P application ------
(fp_map_p_sq <- fp_map(fp = fp_sq[fp_sq$country_origin != "AUT"], map = world_map, indicator = "p_application",
                       origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                       title = "")) #Pro-Kopf Phosphoreinsatz der aktuellen Ernährung in Österreich
(fp_map_p_eat <- fp_map(fp = fp_eat[fp_eat$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = ""))
(fp_map_p_epo <- fp_map(fp = fp_epo[fp_epo$country_origin != "AUT"], map = world_map, indicator = "p_application",
                        origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits$p_application),
                        title = ""))

### save maps ------------

if (write) {
  ggsave(paste0("plots/v",vers,"/map/map_land_sq.png"), fp_map_landuse_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_water_sq.png"), fp_map_blue_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_all_sq.png"), fp_map_ghg_all_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_pb_sq.png"), fp_map_ghg_pb_sq, width = 15, height = 10, units = "cm")
  #ggsave(paste0("plots/v",vers,"/map/map_ghg_sq.png"), fp_map_ghg_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_biodiv_sq.png"), fp_map_biodiv_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_n_sq.png"), fp_map_n_sq, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_p_sq.png"), fp_map_p_sq, width = 15, height = 10, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/map/map_land_eat.png"), fp_map_landuse_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_water_eat.png"), fp_map_blue_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_all_eat.png"), fp_map_ghg_all_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_pb_eat.png"), fp_map_ghg_pb_eat, width = 15, height = 10, units = "cm")
  #ggsave(paste0("plots/v",vers,"/map/map_ghg_eat.png"), fp_map_ghg_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_biodiv_eat.png"), fp_map_biodiv_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_n_eat.png"), fp_map_n_eat, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_p_eat.png"), fp_map_p_eat, width = 15, height = 10, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/map/map_land_epo.png"), fp_map_landuse_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_water_epo.png"), fp_map_blue_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_all_epo.png"), fp_map_ghg_all_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_ghg_pb_epo.png"), fp_map_ghg_pb_epo, width = 15, height = 10, units = "cm")
  #ggsave(paste0("plots/v",vers,"/map/map_ghg_epo.png"), fp_map_ghg_epo, width = 15, height = 10, units = "cm")  
  ggsave(paste0("plots/v",vers,"/map/map_biodiv_epo.png"), fp_map_biodiv_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_n_epo.png"), fp_map_n_epo, width = 15, height = 10, units = "cm")
  ggsave(paste0("plots/v",vers,"/map/map_p_epo.png"), fp_map_p_epo, width = 15, height = 10, units = "cm")
  
}

fp_map_data <- lapply(list("map_sq" = fp_sq, "map_epo" = fp_epo,  "map_eat" = fp_eat), fp_aggregate, aggregate_by = c("country_origin"))
#fp_map_data_df <- fp_map_data %>% reduce(full_join, by='country_origin') 
#write.xlsx(file = "plots/plot_data.xlsx", fp_map_data)
#plot_data <- fp_map_data

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
(mosaic_land_sq <- fp_mosaic(fp = fp_sq, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Anbaufläche in ", m^2)),
                             display_min = 10, round_digs = 0,
                             plot_title = "Flächenfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)*1e4))

(mosaic_land_eat <- fp_mosaic(fp = fp_eat, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Anbaufläche in ", m^2)),
                              display_min = 10, round_digs = 0,
                              plot_title = "Flächenfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))

(mosaic_land_epo <- fp_mosaic(fp = fp_epo, indicator = "landuse", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Anbaufläche in ", m^2)),
                              display_min = 10, round_digs = 0,
                              plot_title = "Flächenfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)*1e4))


### Water --------------------------------
(mosaic_water_sq <- fp_mosaic(fp = fp_sq, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                              divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Wasserinsatz in ", m^3)),
                              display_min = 0.5, round_digs = 2,
                              plot_title = "Wasserfußabruck je Region und Produktgruppe",
                              tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_eat <- fp_mosaic(fp = fp_eat, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                               divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Wasserinsatz in ", m^3)),
                               display_min = 0.5, round_digs = 2,
                               plot_title = "Wasserfußabruck je Region und Produktgruppe",
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_water_epo <- fp_mosaic(fp = fp_epo, indicator = "blue", aggregate_by = c("comm_group_ger", "continent_origin"),
                               divide_by_cells = 1, divide_by_axis = 1, axis_label = expression(paste("Wasserinsatz in ", m^3)),
                               display_min = 0.5, round_digs = 2,
                               plot_title = "Wasserfußabruck je Region und Produktgruppe",
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))


### GHG --------------------------------
(mosaic_ghg_sq <- fp_mosaic(fp = fp_sq, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                            divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")),
                            display_min = 10, round_digs = 0,
                            plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                            tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_eat <- fp_mosaic(fp = fp_eat, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")),
                             display_min = 10, round_digs = 0,
                             plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_ghg_epo <- fp_mosaic(fp = fp_epo, indicator = "ghg_all", aggregate_by = c("comm_group_ger", "continent_origin"),
                             divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")),
                             display_min = 10, round_digs = 0,
                             plot_title = "Emissionsfußabruck je Region und Produktgruppe",
                             tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### Biodiversity --------------------------------
(mosaic_biodiv_sq <- fp_mosaic(fp = fp_sq, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                               divide_by_cells = 1e-6, divide_by_axis = 1e-6, axis_label = expression(paste("Biodiversitätsverlust in ", 10^9," Arten/MSY")),
                               display_min = 1, round_digs = 0,
                               plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                               tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)))

(mosaic_biodiv_eat <- fp_mosaic(fp = fp_eat, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                                divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = expression(paste("Biodiversitätsverlust in ", 10^9," Arten/MSY")),
                                display_min = 10, round_digs = 0,
                                plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                                tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_biodiv_epo <- fp_mosaic(fp = fp_epo, indicator = "biodiv", aggregate_by = c("comm_group_ger", "continent_origin"),
                                divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = expression(paste("Biodiversitätsverlust in ", 10^9," Arten/MSY")),
                                display_min = 10, round_digs = 0,
                                plot_title = "Biodiversitätsfußabruck je Region und Produktgruppe",
                                tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001)))

### N Application --------------------------------
(mosaic_n_sq <- fp_mosaic(fp = fp_sq, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                          divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Stickstoffeinsatz in g",
                          display_min = 10, round_digs = 0,
                          plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                          tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_eat <- fp_mosaic(fp = fp_eat, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Stickstoffeinsatz in g",
                           display_min = 10, round_digs = 0,
                           plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_n_epo <- fp_mosaic(fp = fp_epo, indicator = "n_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Stickstoffeinsatz in g",
                           display_min = 10, round_digs = 0,
                           plot_title = "Stickstoffeinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### P Application --------------------------------
(mosaic_p_sq <- fp_mosaic(fp = fp_sq, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                          divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Phosphoreinsatz in g",
                          display_min = 10, round_digs = 0,
                          plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                          tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_eat <- fp_mosaic(fp = fp_eat, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Phosphoreinsatz in g",
                           display_min = 10, round_digs = 0,
                           plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

(mosaic_p_epo <- fp_mosaic(fp = fp_epo, indicator = "p_application", aggregate_by = c("comm_group_ger", "continent_origin"),
                           divide_by_cells = 1e-3, divide_by_axis = 1e-3, axis_label = "Phosphoreinsatz in g",
                           display_min = 10, round_digs = 0,
                           plot_title = "Phosphoreinsatz je Region und Produktgruppe",
                           tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001,-0.001)))

### save plots and data ---------
if (write) {
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_land_sq.png"), mosaic_land_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_land_eat.png"), mosaic_land_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_land_epo.png"), mosaic_land_epo, width = 25, height = 15, units = "cm")
  
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_water_sq.png"), mosaic_water_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_water_eat.png"), mosaic_water_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_water_epo.png"), mosaic_water_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_ghg_sq.png"), mosaic_ghg_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_ghg_eat.png"), mosaic_ghg_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_ghg_epo.png"), mosaic_ghg_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_biodiv_sq.png"), mosaic_biodiv_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_biodiv_eat.png"), mosaic_biodiv_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_biodiv_epo.png"), mosaic_biodiv_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_n_sq.png"), mosaic_n_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_n_eat.png"), mosaic_n_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_n_epo.png"), mosaic_n_epo, width = 25, height = 15, units = "cm")
  
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_p_sq.png"), mosaic_p_sq, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_p_eat.png"), mosaic_p_eat, width = 25, height = 15, units = "cm")
  ggsave(paste0("plots/v",vers,"/mosaic/mosaic_p_epo.png"), mosaic_p_epo, width = 25, height = 15, units = "cm")
  
}

## save data
fp_mosaic_data <- lapply(list("mosaic_sq" = fp_sq,  "mosaic_epo" = fp_epo,  "mosaic_eat" = fp_eat), 
                         fp_aggregate, 
                         aggregate_by =  c("group_target", "continent_origin"))
#plot_data <- c(plot_data, fp_mosaic_data)


##  Barchart of diet compositions --------------------------------------

#Y_agg <- Y_food_aut[,.(food_g_pc_day_net = sum(food_g_pc_day_net, na.rm = T), eat_g_pc_day_net = sum(eat_g_pc_day_net, na.rm = T)), by =  comm_group_ger]
Y_agg <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_ger, .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net", 
                                                                                     "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net",
                                                                                     "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net",
                                                                                     "food_fat_pc_day_net", "eat_fat_pc_day_net", "epo_fat_pc_day_net")]
Y_agg_long <- Y_agg %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  #pivot_longer(names_to = "diet", cols = c(food:epo), values_to = "g_pc_day") %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_ger, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", "Ernährungspyramide")))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg_long$comm_group_ger)]

# plot
(diet_plot_g <- ggplot(Y_agg_long, 
                       aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                           y = g, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_kcal <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = kcal, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_prot <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = prot, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_fat <- ggplot(Y_agg_long, 
                         aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                             y = fat, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0("plots/v",vers,"/diet_plot_g.png"), diet_plot_g, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/diet_plot_kcal.png"), diet_plot_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/diet_plot_prot.png"), diet_plot_prot, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/diet_plot_fat.png"), diet_plot_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}

# save data
diet_data <- Y_agg_long #rename(Y_agg, sq = food_g_pc_day_net, epo = epo_g_pc_day_net, eat = eat_g_pc_day_net, )
#plot_data <- c(plot_data, "diet_plot" = list(diet_data))

# most important items for each group
Y_agg_item <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = c("comm_group_ger", "item"), 
                         .SDcols = c("food_g_pc_day_net", "eat_g_pc_day_net", "epo_g_pc_day_net", 
                                     "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net",
                                     "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net",
                                     "food_fat_pc_day_net", "eat_fat_pc_day_net", "epo_fat_pc_day_net")]


## Stacked barcharts for footprints by consumption items ---------------------

indicators <- c("landuse", "ghg_all", "blue", "biodiv", "n_application", "p_application")
indicator_labs <- c(landuse = "Anbaufläche  in m<sup>2</sup>",
                    ghg_all = "THG-Emissionen in t CO<sub>2</sub>-Äq.",
                    blue = "Wassereinsatz in m<sup>3</sup>",
                    biodiv = "Biodiversitätsverlust in 10<sup>-6</sup> Arten / MSY",
                    p_application =  "Phosphoreinsatz in kg",
                    n_application = "Stickstoffeinsatz in kg")

pb_stack_list <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp_sq,  "epo" = fp_epo, "eat" = fp_eat), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack <- wrap_plots(pb_stack_list, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

# with reversed legend für single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")
indicator_ext_labs <- c(indicator_labs, 
                        "ghg_pb" = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                        "luh" = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")
pb_stack_list_rev <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp_sq,  "epo" = fp_epo, "eat" = fp_eat), 
               indicator = ind, axis_lab = indicator_ext_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  ggsave(paste0("plots/v",vers,"/pb_stack.png"), pb_stack, width = 30, height = 25, units = "cm")
  # also save single plots
  for (i in 1:length(pb_stack_list_rev)) {
    ggsave(filename=paste0("plots/v",vers,"/stack/pb_stack_", names(pb_stack_list_rev)[i],".png"), 
           plot=pb_stack_list_rev[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 15, height = 12, units = "cm")
  }
}

# save data
pb_stack_data <- sapply(indicators, function(ind){
  stacked_data(fp_list = list("sq" = fp_sq,  "eat" = fp_eat,  "epo" = fp_epo), 
               indicator = ind)
}, simplify = FALSE, USE.NAMES = TRUE)
pb_stack_data <- pb_stack_data %>% reduce(full_join, by=c('comm_group_ger', 'diet', 'diet_lab')) 
#plot_data <- c(plot_data, "pb_stack" = list(pb_stack_data))


# side-by-side stack for status-quo emissions
ind_labs <- c("ghg" = "Landwirtschaft", "luh" = "Landnutzungsänderung")
pb_stack_ghg_sq <- stacked_bars_single(fp = fp_sq, ind_list = c("ghg", "luh"), ind_labs = ind_labs, axis_lab = "THG-Emissionen in t CO<sub>2</sub>-Äq.")
ggsave(filename=paste0("plots/v",vers,"/stack/pb_stack_ghg_sq.png"), pb_stack_ghg_sq, width = 15, height = 15, units = "cm") 


## comparison of direct vs LUC emissions by group
stack_ghg_luc_sq <- stacked_bars_ghg(fp = fp_sq, mult_fact = 8495000/1000000, axis_lab =  "THG-Emissionen in Mio. t CO<sub>2</sub>-Äq.", ind_labs = ind_labs) + theme(legend.position=c(.85,.85))
ggsave(filename=paste0("plots/v",vers,"/stack/stack_ghg_luc_sq.png"), stack_ghg_luc_sq, width = 25, height = 12, units = "cm") 



## Comparison plot of footprints with per-capita planetary boundaries -------------------


# aggregate indicators
fp_agg <- as.data.frame(rbind(fp_sq_agg, fp_epo_agg, fp_eat_agg))
#fp_agg_land <- as.data.frame(rbind(fp_sq_agg_crop, fp_epo_agg_crop, fp_eat_agg_crop))
#fp_agg$landuse <- fp_agg_land$landuse

fp_agg$diet <- factor(c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"), levels = c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"))
#fp_agg_land$diet <- factor(c("Status \nQuo", "Planetary \nHealth Diet", "Ernährungs- \npyramide"), levels = c("Status \nQuo", "Planetary \nHealth Diet", "Ernährungs- \npyramide"))


(pb_bar_land <- ggplot(fp_agg, aes(x = diet, y = landuse)) + #/ 10000
    # geom_bar(stat="identity", fill = "#176040") +
    geom_bar(stat="identity", fill = viridis(6)[1]) +
    geom_hline(aes(yintercept = pbs["landuse", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["landuse", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["landuse", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Flächenverbrauch in m<sup>2</sup>", x = NULL, color = NULL) +
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_water <- ggplot(fp_agg, aes(x = diet, y = blue)) +
    # geom_bar(stat="identity", fill = "#293969") +
    geom_bar(stat="identity", fill = viridis(6)[2]) +
    geom_hline(aes(yintercept = pbs["blue", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["blue", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["blue", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Wasserverbrauch in m<sup>3</sup>", x = NULL, color = NULL)+
    coord_cartesian(ylim = c(0, pbs["blue", "upper"]))+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_ghg_all <- ggplot(fp_agg, aes(x = diet, y = ghg_all)) +
    # geom_bar(stat="identity", fill = "#521f11") +
    geom_bar(stat="identity", fill = viridis(6)[3]) +
    geom_hline(aes(yintercept = pbs["ghg_all", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["ghg_all", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["ghg_all", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Emissionen in t CO<sub>2</sub>-Äq.", x = NULL, color = NULL)+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_ghg_pb <- ggplot(fp_agg, aes(x = diet, y = ghg_pb)) +
    # geom_bar(stat="identity", fill = "#521f11") +
    geom_bar(stat="identity", fill = viridis(6)[3]) +
    geom_hline(aes(yintercept = pbs["ghg_all", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["ghg_all", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["ghg_all", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Emissionen (exkl. Energie und LUC) in t CO<sub>2</sub>-Äq.", x = NULL, color = NULL)+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_biodiv <- ggplot(fp_agg, aes(x = diet, y = biodiv)) +
    geom_bar(stat="identity", fill = viridis(6)[4]) +
    geom_hline(aes(yintercept = pbs["biodiv", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["biodiv", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["biodiv", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Biodiversitätsverlust in 10^-6 Arten", x = NULL, color = NULL)+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_n <- ggplot(fp_agg, aes(x = diet, y = n_application)) +
    geom_bar(stat="identity", fill = viridis(6)[5]) +
    geom_hline(aes(yintercept = pbs["n_application", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["n_application", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["n_application", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Stickstoffeinsatz in kg", x = NULL, color = NULL)+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar_p <- ggplot(fp_agg, aes(x = diet, y = p_application)) +
    geom_bar(stat="identity", fill = viridis(6)[6]) +
    geom_hline(aes(yintercept = pbs["p_application", "boundary"], color =  "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
    geom_hline(aes(yintercept = pbs["p_application", "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1) +
    geom_hline(aes(yintercept = pbs["p_application", "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1) +
    scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
    labs(y = "Phosphoreinsatz in kg", x = NULL, color = NULL)+
    theme_minimal()+
    theme(axis.title.y = element_markdown()))

(pb_bar <- pb_bar_land + pb_bar_water + pb_bar_ghg_all + pb_bar_biodiv + pb_bar_n + pb_bar_p + plot_layout(guides = "collect") 
  & theme(legend.position = "bottom"))

# save plot
if (write) {
  ggsave(paste0("plots/v",vers,"/pb_bar.png"), pb_bar, width = 15, height = 12, units = "cm", scale = 1.5)
}

# save data
#plot_data <- c(plot_data, "pb_bar" = list(fp_agg))


## Spiderweb chart -----------------------

fp_agg_sel <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

# rescale by pb values
fp_spider <- fp_agg_sel
fp_spider[,2:7] <- t(t(fp_agg_sel[,2:7])/pbs[, "lower"])
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

# save plot
if (write) ggsave(paste0("plots/v",vers,"/pb_spiderweb.png"), pb_spider, width = 10, units = "cm", scale = 1.5)

# save data
#plot_data <- c(plot_data, "pb_spider & pb_circle" = list(fp_spider))


## Circular planetary boundary chart (experimental) -------------

# rescale values
pbs <- cbind(pbs, "range" = pbs[,"upper"] - pbs[,"lower"])
fp_circle <- fp_agg_sel
fp_circle[,2:7] <- t((t(fp_agg_sel[,2:7])-pbs[, "lower"])/pbs[, "range"])+1
#####.
fp_circle$Biodiversität <- 0
#####.

pb_circle <- sapply(c("sq", "epo", "eat"), circle_plot_grad, fp_table = fp_circle, ylim.min = -0.0, ylim.max = 4, log = FALSE, legend = FALSE,
                    simplify = FALSE, USE.NAMES = TRUE)
(pb_circle <- wrap_plots(pb_circle, guides = "collect", nrow = 1) & theme(legend.position = 'bottom',
                                                                          legend.direction = 'horizontal'))

if (write) ggsave(paste0("plots/v",vers,"/pb_circle.png"), pb_circle, width = 30, units = "cm", scale = 1)




# Pyramid 2.0 -------------------------------------------------------

### compute impacts per kcal/prot -------

# add relevant items to Y_food --> mostly already done in create_diets
Y_food_aut[, `:=`(food_t_pc_net = food_g_pc_day_net * 365*1e-6,
                  eat_t_pc_net = eat_g_pc_day_net * 365*1e-6,
                  epo_t_pc_net = epo_g_pc_day_net * 365*1e-6,
                  food_kcal_pc_net = food_kcal_pc_day_net * 365,
                  eat_kcal_pc_net = eat_kcal_pc_day_net * 365,
                  epo_kcal_pc_net = epo_kcal_pc_day_net * 365,
                  food_prot_pc_net = food_prot_pc_day_net * 365,
                  eat_prot_pc_net = eat_prot_pc_day_net * 365,
                  epo_prot_pc_net = epo_prot_pc_day_net * 365,
                  food_fat_pc_net = food_fat_pc_day_net * 365,
                  eat_fat_pc_net = eat_fat_pc_day_net * 365,
                  epo_fat_pc_net = epo_fat_pc_day_net * 365
)] 

# aggregate footprints by item
fp_sq_item <- fp_aggregate(fp_sq, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_ger"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_epo_item <- fp_aggregate(fp_epo, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_ger"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_eat_item <- fp_aggregate(fp_eat, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_ger"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

# merge
Y_food_aut <- merge(Y_food_aut, regions[,.(code, area_iso = iso3c, continent)], by.x = "area_code", by.y = "code")
fp_sq_item <- merge(fp_sq_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_ger 
                                              t_consumed = food_t_pc, 
                                              t_net_consumed = food_t_pc_net, 
                                              kcal_net_consumed = food_kcal_pc_net,
                                              proteins_net_consumed = food_prot_pc_net,
                                              fat_net_consumed = food_fat_pc_net)], 
                    by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_sq_item$kcal_net_consumed)/365, sum(Y_food_aut$food_kcal_pc_day_net))

fp_epo_item <- merge(fp_epo_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_ger, 
                                                t_consumed = epo_t_pc, 
                                                t_net_consumed = epo_t_pc_net, 
                                                kcal_net_consumed = epo_kcal_pc_net,
                                                proteins_net_consumed = epo_prot_pc_net,
                                                fat_net_consumed = epo_fat_pc_net)], 
                     by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_epo_item$kcal_net_consumed)/365, sum(Y_food_aut$epo_kcal_pc_day_net))

fp_eat_item <- merge(fp_eat_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_ger, 
                                                t_consumed = eat_t_pc, 
                                                t_net_consumed = eat_t_pc_net, 
                                                kcal_net_consumed = eat_kcal_pc_net,
                                                proteins_net_consumed = eat_prot_pc_net,
                                                fat_net_consumed = eat_fat_pc_net)], 
                     by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_eat_item$kcal_net_consumed)/365, sum(Y_food_aut$eat_kcal_pc_day_net))

# compute footprint by kcal
inds <-  c("production", "landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application")
fp_sq_item_by <- fp_sq_item[, (paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
fp_sq_item_by <- fp_sq_item[, (paste0(inds,"_per_gprot")) := lapply(.SD, '/', proteins_net_consumed), .SDcols = inds]
#fp_epo_item_by <- fp_epo_item[, (paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
#fp_eat_item_by <- fp_eat_item[, (paste0(inds,"_per_g_protein")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]

## same on aggregate item level
fp_sq_item_agg <- fp_aggregate(fp_sq_item, aggregate_by = c("country_consumer", "item_target", "comm_group_ger"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed"))
fp_sq_item_agg_by <- fp_sq_item_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
fp_sq_item_agg_by <- fp_sq_item_agg_by[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', proteins_net_consumed), .SDcols = inds]
write.csv(fp_sq_item_agg_by, "tables/fp_sq_item_agg_by.csv", fileEncoding="UTF-16LE")

## and on group level
fp_sq_group_agg <- fp_aggregate(fp_sq_item, aggregate_by = c("country_consumer", "comm_group_ger"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed"))
fp_sq_group_agg_by <- fp_sq_group_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
fp_sq_group_agg_by <- fp_sq_group_agg_by[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', proteins_net_consumed), .SDcols = inds]
write.csv(fp_sq_group_agg_by, "tables/fp_sq_group_agg_by.csv", fileEncoding="UTF-16LE")



### define EPO 2.0 --------------

epo_groups_all <- unique(Y_food_aut$epo_group)
epo_groups_keep <- c("All sugars", "Fruits", "Coffee, tea and cocoa", "Spices", "Alcohol", NA)

Y_food_aut[epo_group %in% epo_groups_keep , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net, epo2_prot_pc_day_net = epo_prot_pc_day_net, 
                                                 epo2_g_pc_day_net = epo_g_pc_day_net, epo2_fat_pc_day_net = epo_fat_pc_day_net)]


Y_food_aut[epo_group %in% c("Meat, red",  "Meat, low-fat", "Fish", "Butter, lard or tallow", "Eggs") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net/2, epo2_prot_pc_day_net = epo_prot_pc_day_net/2, epo2_g_pc_day_net = epo_g_pc_day_net/2, epo2_fat_pc_day_net = epo_fat_pc_day_net/2)]
Y_food_aut[epo_group %in% c("Cereals, roots and tubers") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*5/4, epo2_prot_pc_day_net = epo_prot_pc_day_net*5/4, epo2_g_pc_day_net = epo_g_pc_day_net*5/4, epo2_fat_pc_day_net = epo_fat_pc_day_net*5/4)]
Y_food_aut[epo_group %in% c("Milk and products") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*1/3, epo2_prot_pc_day_net = epo_prot_pc_day_net*1/3, epo2_g_pc_day_net = epo_g_pc_day_net*1/3, epo2_fat_pc_day_net = epo_fat_pc_day_net*1/3)]

Y_food_aut[epo_subgroup %in% c("Vegetables") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*2/2.7231774, epo2_prot_pc_day_net = epo_prot_pc_day_net*2/2.7231774, epo2_g_pc_day_net = epo_g_pc_day_net*2/2.7231774, epo2_fat_pc_day_net = epo_fat_pc_day_net*2/2.7231774)]
Y_food_aut[epo_subgroup %in% c("Legumes") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*1/0.2629964, epo2_prot_pc_day_net = epo_prot_pc_day_net*1/0.2629964, epo2_g_pc_day_net = epo_g_pc_day_net*1/0.2629964, epo2_fat_pc_day_net = epo_fat_pc_day_net*1/0.2629964)]
#Y_food_aut[comm_group_ger %in% "Pflanzenöle" , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*2, epo2_prot_pc_day_net = epo_prot_pc_day_net*2, epo2_g_pc_day_net = epo_g_pc_day_net*2, epo2_fat_pc_day_net = epo_fat_pc_day_net*2)]
#Y_food_aut[epo_subgroup == c("Vegetable oils, nuts and seeds") &  comm_group_ger != "Pflanzenöle", `:=`(epo2_kcal_pc_day_net = food_kcal_pc_day_net, epo2_prot_pc_day_net = food_prot_pc_day_net, epo2_g_pc_day_net = food_g_pc_day_net, epo2_fat_pc_day_net = food_fat_pc_day_net)]
Y_food_aut[epo_subgroup == c("Vegetable oils, nuts and seeds") , `:=`(epo2_kcal_pc_day_net = epo_kcal_pc_day_net*2, epo2_prot_pc_day_net = epo_prot_pc_day_net*2, epo2_g_pc_day_net = epo_g_pc_day_net*2, epo2_fat_pc_day_net = epo_fat_pc_day_net*2)]


# transform into annual totals
Y_food_aut[, `:=`(epo2_g_pc = epo2_g_pc_day_net * (1/(1-waste_fin-loss)) * 365,
                  epo2_t_pc = epo2_g_pc_day_net * (1/(1-waste_fin-loss)) * 365 * 1e-6,
                  epo2_port_day = epo2_g_pc_day_net/g_port)]


# check nutritional values
sum(Y_food_aut$epo2_kcal_pc_day_net)
sum(Y_food_aut[comm_group_ger %in% c("Getreide","Wurzeln und Knollen")]$epo2_kcal_pc_day_net)
sum(Y_food_aut$epo2_prot_pc_day_net)# - sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut[comm_group_ger %in% c("Eier")]$epo2_prot_pc_day_net) 
sum(Y_food_aut$epo2_g_pc_day_net)

sum(Y_food_aut$epo_kcal_pc_day_net)
sum(Y_food_aut[comm_group_ger %in% c("Getreide","Wurzeln und Knollen")]$epo_kcal_pc_day_net)
sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut$epo_g_pc_day_net)

# calculate portions per epo group
Y_food_aut_epo2 <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = epo_group]
Y_food_aut_epo2_sub <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = .(epo_subgroup, epo_group)]
Y_food_aut_epo2_sub <- Y_food_aut_epo2_sub[, `:=`(epo_port_day_group = sum(epo_port_day), epo2_port_day_group = sum(epo2_port_day)), by = epo_group]
Y_food_aut_epo2_item <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = c("epo_subgroup", "epo_group", "item", "g_port")]
write.csv(Y_food_aut_epo_sub, "Y_food_aut_epo2_sub.csv")
write.csv(Y_food_aut_epo_item, "Y_food_aut_epo2_item.csv")

fp_epo2 <- footprint(country = "AUT",  allocation = "value", year = yr, y = Y_food_aut$epo2_t_pc, X = X, E = E_all, v = vers)
fp_epo2$landuse[fp_epo2$item_origin=="Grazing"] <- 0
fp_epo2$landuse <- fp_epo2$landuse * 10000
fp_epo2 <- merge(fp_epo2, items_ger[,.(item, comm_group_ger)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)


### plots for EPO 2.0 ----------------

## stack plots 

pb_stack_list2 <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp_sq,  "epo" = fp_epo, "eat" = fp_eat, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack2 <- wrap_plots(pb_stack_list2, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))
ggsave(filename=paste0("plots/v",vers,"/epo/pb_stack_epo2_full.png"), pb_stack2, width = 30, height = 25, units = "cm") 

pb_stack_list2_comp <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("epo" = fp_epo, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)
(pb_stack2_comp <- wrap_plots(pb_stack_list2_comp, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

ggsave(filename=paste0("plots/v",vers,"/epo/pb_stack_epo2.png"), pb_stack2_comp, width = 25, height = 25, units = "cm") 

# with reversed legend for single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")
indicator_ext_labs <- c(indicator_labs, 
                        "ghg_pb" = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                        "luh" = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")

pb_stack_list_rev_epo <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp_sq,  "epo" = fp_epo, "eat" = fp_eat, "epo2" = fp_epo2), 
               indicator = ind, axis_lab = indicator_ext_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  # also save single plots
  for (i in 1:length(pb_stack_list_rev_epo)) {
    ggsave(filename=paste0("plots/v",vers,"/epo/stack/pb_stack_", names(pb_stack_list_rev_epo)[i],".png"), 
           plot=pb_stack_list_rev_epo[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 20, height = 12, units = "cm")
  }
}


## diet plots 

Y_agg2 <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_ger, .SDcols = c("food_g_pc_day_net",    "eat_g_pc_day_net",    "epo_g_pc_day_net",    "epo2_g_pc_day_net",
                                                                                      "food_kcal_pc_day_net", "eat_kcal_pc_day_net", "epo_kcal_pc_day_net", "epo2_kcal_pc_day_net",
                                                                                      "food_prot_pc_day_net", "eat_prot_pc_day_net", "epo_prot_pc_day_net", "epo2_prot_pc_day_net",
                                                                                      "food_fat_pc_day_net",  "eat_fat_pc_day_net",  "epo_fat_pc_day_net",  "epo2_fat_pc_day_net")]
Y_agg2_long <- Y_agg2 %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_ger, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", ifelse(diet == "epo", "Ernährungspyramide", "Ernährungspyramide 2.0"))))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg2_long$comm_group_ger)]

Y_agg2_long <- Y_agg2_long %>% filter(diet %in% c("epo", "epo2"))


# plot
(diet_plot2_g <- ggplot(Y_agg2_long, 
                        aes(x = factor(diet_lab, levels = rev(c("Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                            y = g, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_kcal <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = kcal, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_prot <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = prot, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_fat <- ggplot(Y_agg2_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                              y = fat, fill = factor(comm_group_ger, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0("plots/v",vers,"/epo/diet_plot_g.png"), diet_plot2_g, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/epo/diet_plot_kcal.png"), diet_plot2_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/epo/diet_plot_prot.png"), diet_plot2_prot, width = 12, height = 3.5)
  ggsave(filename = paste0("plots/v",vers,"/epo/diet_plot_fat.png"), diet_plot2_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}



## save data ---------------
plot_data <- c(fp_map_data, fp_mosaic_data, 
               "diet_plot" = list(diet_data),
               "diets_detail" = list(Y_agg_item),
               "pb_stack" = list(pb_stack_data),
               "pb_bar" = list(fp_agg),
               "pb_circle" = list(fp_spider),
               "fp_by_item_sq" = list(fp_sq_item_agg),
               "fp_by_item_epo" = list(fp_epo_item_agg),
               "fp_by_item_eat" = list(fp_eat_item_agg))

write.xlsx(plot_data, file = paste0("plots/v",vers,"/plot_data.xlsx"), overwrite = TRUE)
