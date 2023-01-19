# diet footprints under conditional diet shifts

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
vers <- "1.2" # or "1.1"
yr = 2020

# should results be saved to file?
write = TRUE
plot_dir = "plots" # set plot directory

# set language for plots
lang = "de" # "de

fp_sq <- readRDS(paste0("./data/v",vers,"/fp_sq_",yr,".rds"))
Y_food_aut <- readRDS(paste0("./data/v",vers,"/Y_food_aut_full_",yr,".rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/X.rds"))
E_all <- readRDS(file=paste0("./data/v",vers,"/E_all.rds"))
items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/biodiv_codes.csv"))
fabio_index <- readRDS(file=paste0("./data/v",vers,"/fabio_index.rds"))
cbs_pop <- readRDS(paste0("./data/v",vers,"/cbs_pop.rds"))[area == "Austria" & year == yr, value]*1000

# group names for plots
items_group <- as.data.table(openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = ifelse(vers == "1.1", "items_german_1.1", "items_german")))
if (lang == "de") {
  items_group[, comm_group_plot := comm_group_ger]
} else {
  items_group[, comm_group_plot := comm_group_en]
}
groups_cons <- unique(Y_food_aut[food_t>0, comm_group_plot])

# colors for food groups
food_cols <- openxlsx::read.xlsx("inst/items_conc.xlsx", sheet = "colors_alt", colNames = FALSE)
food_cols_vect <- food_cols$X3
if (lang == "de") names(food_cols_vect) <- food_cols$X1
if (lang == "en") names(food_cols_vect) <- food_cols$X2

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
  #"biodiv" = 1 / 10e9, # from species to species pc
  "biodiv" = 1 * (sum(unique(items_biodiv$number))/1e6) / 10e9, # from species per MSY to species pc
  "n_application" = 1e9 / 10e9, # from Tg to kg pc
  "p_application" = 1e9 / 10e9 # from Tg to kg pc
)

pbs <- pbs_global*per_cap_factor


# should production footprints be taken from file? --> Faster, but take only if nothing in the Y and L data changed
take_prod_result = FALSE

# adapt diets -------------------------------------
Y_food_aut <- dietshift_cond(Y_food_aut, cond.var = "landuse", cond.reg = "AUT", diet.name = "eat", diet.suffix = "cond",
                             rescale.var = "eat_rescaler", rescale.group = "eat_group_fin", skip.groups = c("Peanuts", "Tree nuts & seeds"),
                             fp_sq = fp_sq, x = X[,as.character(yr)],
                             add.newcols = TRUE, tol = 1e-3)
Y_food_aut <- dietshift_cond(Y_food_aut, cond.var = "landuse", cond.reg = "AUT", diet.name = "epo", diet.suffix = "cond",
                             rescale.var = "epo_rescaler", rescale.group = "epo_subgroup", skip.groups = c("Fish"),
                             fp_sq = fp_sq, x = X[,as.character(yr)],
                             add.newcols = TRUE, tol = 1e-2)
Y_food_aut <- dietshift_cond(Y_food_aut, cond.var = "landuse", cond.reg = "AUT", diet.name = "epo2", diet.suffix = "cond",
                             rescale.var = "epo2_rescaler_sq", rescale.group = "epo_subgroup", 
                             fp_sq = fp_sq, x = X[,as.character(yr)], skip.groups = c("Fish"),
                             add.newcols = TRUE, tol = 1e-2)




#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

cat("\n Calculate diet footprints under conditional diet shifts for version", vers, "and year", yr, "\n")


diets = c("eat_cond", "epo_cond", "epo2_cond")
names(diets) <- c("eat_cond", "epo_cond", "epo2_cond")

fp_cond_list <- lapply(diets, function(diet){
  fp <- footprint(country = "AUT", allocation = "value", year = yr, y = Y_food_aut[[paste0(diet,"_t_pc")]], X = X, E = E_all, index = fabio_index, v = vers, take.result = take_prod_result, result.dir = "data", result.suffix = diet)
  fp$landuse[fp$item_origin=="Grazing"] <- 0
  fp$landuse <- fp$landuse * 10000
  fp <- merge(fp, items_group[,.(item, comm_group_plot)], by.x = c("item_target"), by.y = c("item"), all.x = TRUE, sort = FALSE)
  fp <- fp[comm_group_plot %in% groups_cons,]
})

# add sq
fp_cond_list <- c("sq" = list(fp_sq), fp_cond_list)

fp_cond_agg_list <- lapply(fp_cond_list, fp_aggregate, aggregate_by = c("country_consumer"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))
fp_cond_country_list <- lapply(fp_cond_list, fp_aggregate, aggregate_by = c("country_consumer", "country_origin"), indicators = c("landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

# determine indicator-specific limits for plots to be used across all scenarios
fp_limits <- rbindlist(fp_cond_country_list) %>% 
  filter(country_origin != "AUT") %>% 
  group_by(country_consumer) %>% 
  summarise(across(landuse:last_col(), max))
  #rename(ghg = ghg_all)

#-------------------------------------------------------#
# ---------------- Create Visualizations ---------------
#-------------------------------------------------------#

## Footprint map: impacts across the world ---------------------------------------------------------------------


# load world map shapefile
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

inds <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
names(inds) <- inds

fp_cond_maps <- sapply(names(diets), function(diet){
  cat("\n", diet, ": \n")
  
  maps <- lapply(inds, function(ind){
    cat(ind, "\n")
    map <- fp_map(fp = fp_cond_list[[diet]][country_origin != "AUT",], map = world_map, indicator = ind,
           origin_items = "ALL", target_items = "ALL", limits = c(0, fp_limits[[ind]]),
           title = "", lang = lang)
    if (write) ggsave(paste0(plot_dir,"/v",vers,"/cond/map/map_", ind, "_",diet,".png"), map, width = 15, height = 10, units = "cm")
    return(fp_map)  
  })
}, USE.NAMES = TRUE, simplify = FALSE)

fp_map_data <- lapply(fp_cond_list, fp_aggregate, aggregate_by = c("country_origin"))
names(fp_map_data) <- paste0("map_", gsub("_cond", "", names(fp_map_data)))


## Mosaic plot -------------------------------------------------------------------------------------------

mosaic_pars <- data.frame(inds = inds, 
                          divide_by_cells = c(1,1, 1e-3, 1e-14, 1e-3, 1e-3),
                          divide_by_axis = c(1,1, 1e-3, 1e-14, 1e-3, 1e-3),
                          display_min = c(10,0.5, 10, 0.5, 50, 10),
                          round_digs = c(0,2,0,2,0,0))

fp_cond_maps <- sapply(names(diets), function(diet){
  cat("\n", diet, ": \n")

  mosaics <- lapply(inds, function(ind){
    cat("\n", ind, "\n")
    pars <- mosaic_pars[inds == ind,]
    mosaic <- fp_mosaic(fp = fp_cond_list[[diet]], indicator = ind, aggregate_by = c("comm_group_plot", "continent_origin"),
                        divide_by_cells = pars$divide_by_cells, divide_by_axis = pars$divide_by_axis, 
                        display_min = pars$display_min, round_digs = pars$round_digs,
                        tick_offset = c(0,-0.0033,-0.006,-0.006,-0.004,-0.004,-0.002,-0.001, -0.001))
  
    if (write) ggsave(paste0(plot_dir,"/v",vers,"/cond/mosaic/mosaic_", ind, "_",diet,".png"), mosaic, width = 25, height = 15, units = "cm")
    return(mosaic)  
  })
}, USE.NAMES = TRUE, simplify = FALSE)


fp_mosaic_data <- lapply(#list("mosaic_sq" = fp_cond_list$sq,  "mosaic_epo" = fp_cond_list$epo_cond,  "mosaic_eat" = fp_cond_list$eat_cond), 
                         fp_cond_list, fp_aggregate, aggregate_by =  c("comm_group_plot", "continent_origin"))
names(fp_mosaic_data) <- paste0("mosaic_", gsub("_cond", "", names(fp_mosaic_data)))


##  Barchart of diet compositions --------------------------------------

Y_agg <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_plot, .SDcols = c("food_g_pc_day_net",    "eat_cond_g_pc_day_net",    "epo_cond_g_pc_day_net",    "epo2_cond_g_pc_day_net",   
                                                                                      "food_kcal_pc_day_net", "eat_cond_kcal_pc_day_net", "epo_cond_kcal_pc_day_net", "epo2_cond_kcal_pc_day_net",
                                                                                      "food_prot_pc_day_net", "eat_cond_prot_pc_day_net", "epo_cond_prot_pc_day_net", "epo2_cond_prot_pc_day_net",
                                                                                      "food_fat_pc_day_net",  "eat_cond_fat_pc_day_net",  "epo_cond_fat_pc_day_net",  "epo2_cond_fat_pc_day_net")]
Y_agg_long_full <- Y_agg %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  rename_with(~gsub("_cond", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_plot, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", ifelse(diet == "epo", "Ernährungspyramide", "Ernährungspyramide 2.0"))))

Y_agg_long <- filter(Y_agg_long_full, diet != "epo2")

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg_long$comm_group_plot)]

# plot
(diet_plot_g <- ggplot(Y_agg_long, 
                       aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                           y = g, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_kcal <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = kcal, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_prot <- ggplot(Y_agg_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                              y = prot, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot_fat <- ggplot(Y_agg_long, 
                         aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Ernährungspyramide", "Planetary Health Diet"))), 
                             y = fat, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/diet_plot_g.png"), diet_plot_g, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/diet_plot_kcal.png"), diet_plot_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/diet_plot_prot.png"), diet_plot_prot, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/diet_plot_fat.png"), diet_plot_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}

# save data
diet_data <- Y_agg_long_full #

# same data on item level
Y_agg_item <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = c("comm_group_plot", "item"), 
                         .SDcols = c("food_g_pc_day_net",    "eat_cond_g_pc_day_net",    "epo_cond_g_pc_day_net",    "epo2_cond_g_pc_day_net", 
                                     "food_kcal_pc_day_net", "eat_cond_kcal_pc_day_net", "epo_cond_kcal_pc_day_net", "epo2_cond_kcal_pc_day_net",
                                     "food_prot_pc_day_net", "eat_cond_prot_pc_day_net", "epo_cond_prot_pc_day_net", "epo2_cond_prot_pc_day_net",
                                     "food_fat_pc_day_net",  "eat_cond_fat_pc_day_net",  "epo_cond_fat_pc_day_net",  "epo2_cond_fat_pc_day_net")]
names(Y_agg_item) <- gsub("_cond", "",names(Y_agg_item))
Y_agg_item <- merge(Y_agg_item, items_group[, .(item, item_ger)], by = "item")
Y_agg_item  <- relocate(Y_agg_item, item_ger, .after = item)


## Stacked barcharts for footprints by consumption items ---------------------

indicators <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
if (lang == "de"){
  indicator_labs <- c(landuse = "Anbaufläche  in m<sup>2</sup>",
                      ghg_all = "THG-Emissionen in t CO<sub>2</sub>-Äq.",
                      blue = "Wassereinsatz in m<sup>3</sup>",
                      biodiv = "Biodiversitätsverlust in Arten / Jahr",
                      p_application =  "Phosphoreinsatz in kg",
                      n_application = "Stickstoffeinsatz in kg",
                      ghg_pb = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                      luh = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")
  
} else {
  indicator_labs <- c(landuse = "Cropland  in m<sup>2</sup>",
                      ghg_all = "GHG emissions in t CO<sub>2</sub>-eq.",
                      blue = "Water use in m<sup>3</sup>",
                      biodiv = "Biodiversity loss in species / year",
                      p_application =  "Phosphorous use in kg",
                      n_application = "Nitrogen use in kg",
                      ghg_pb = "GHG emissions (excl. energy & LUC) in t CO<sub>2</sub>-eq.", 
                      luh = "GHG emissions from land use change in t CO<sub>2</sub>-eq.")
}

pb_stack_list <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp_cond_list$sq,  "epo" = fp_cond_list$epo_cond, "eat" = fp_cond_list$eat_cond), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack <- wrap_plots(pb_stack_list, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

# with reversed legend für single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")


pb_stack_list_rev <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp_cond_list$sq,  "epo" = fp_cond_list$epo_cond, "eat" = fp_cond_list$eat_cond), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  ggsave(paste0(plot_dir,"/v",vers,"/cond/pb_stack.png"), pb_stack, width = 30, height = 25, units = "cm")
  # also save single plots
  for (i in 1:length(pb_stack_list_rev)) {
    ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/stack/pb_stack_", names(pb_stack_list_rev)[i],".png"), 
           plot=pb_stack_list_rev[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 15, height = 12, units = "cm")
  }
}

# save data
pb_stack_data <- sapply(indicators, function(ind){
  stacked_data(fp_list = list("sq" = fp_cond_list$sq,  "epo" = fp_cond_list$epo_cond, "eat" = fp_cond_list$eat_cond, "epo2" = fp_cond_list$epo2_cond), 
               indicator = ind)
}, simplify = FALSE, USE.NAMES = TRUE)
pb_stack_data <- pb_stack_data %>% reduce(full_join, by=c('comm_group_plot', 'diet', 'diet_lab')) 
#plot_data <- c(plot_data, "pb_stack" = list(pb_stack_data))


# side-by-side stack for status-quo emissions
if(lang == "de") ind_labs <- c("ghg" = "Landwirtschaft", "luh" = "Landnutzungsänderung")
if(lang == "en") ind_labs <- c("ghg" = "Agriculture", "luh" = "Land use change")

pb_stack_ghg_sq <- stacked_bars_single(fp = fp_cond_list$sq, ind_list = c("ghg", "luh"), ind_labs = ind_labs, axis_lab = "THG-Emissionen in t CO<sub>2</sub>-Äq.")
if(write) ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/stack/pb_stack_ghg_sq.png"), pb_stack_ghg_sq, width = 15, height = 15, units = "cm") 


## comparison of direct vs LUC emissions by group
ind_list = c("ghg_live", "ghg_energy", "luh", "ghg_other")
stack_ghg_sq <- stacked_bars_ghg(fp = fp_cond_list$sq, ind_list = ind_list,
                                     mult_fact = cbs_pop/1000000, 
                                     axis_lab =  "THG-Emissionen in Mio. t CO<sub>2</sub>-Äq.") + 
  theme(legend.position=c(.85,.85))
ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/stack/stack_ghg_detail_sq.png"), stack_ghg_sq, width = 25, height = 12, units = "cm") 

stack_ghg_sq_data <- fp_aggregate(fp_cond_list$sq, aggregate_by = "comm_group_plot", indicators = ind_list) %>% 
  #pivot_longer(cols = all_of(c(ind_list)), names_to = "indicator", values_to = "value") %>%
  #mutate(value = value * (cbs_pop/1000000))
  mutate(across(all_of(ind_list), function(x){x * (cbs_pop/1000000)}))
 


## Comparison plot of footprints with per-capita planetary boundaries -------------------


# aggregate indicators
fp_agg <- as.data.frame(rbind(fp_cond_agg_list$sq, fp_cond_agg_list$epo_cond, fp_cond_agg_list$eat_cond))

fp_agg$diet <- factor(c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"), levels = c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet"))

pb_bar <- sapply(indicators, pb_bars, fp_agg = fp_agg, lang = "de", USE.NAMES = TRUE, simplify = FALSE)

(pb_bar <- wrap_plots(pb_bar, ncol = 3, nrow = 2) + plot_layout(guides = "collect") 
  & theme(legend.position = "bottom"))

# save plot
if (write) {
  ggsave(paste0(plot_dir,"/v",vers,"/cond/pb_bar.png"), pb_bar, width = 15, height = 12, units = "cm", scale = 1.5)
}

# save data
#plot_data <- c(plot_data, "pb_bar" = list(fp_agg))


## Spiderweb chart -----------------------

fp_agg_sel <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

# rescale by pb values
fp_spider <- fp_agg_sel
fp_spider[,2:7] <- t(t(fp_agg_sel[,2:7])/pbs[, "boundary"])
#fp_spider <- mutate(fp_spider, Biodiversität = Biodiversität/10)

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
if (write) ggsave(paste0(plot_dir,"/v",vers,"/cond/pb_spiderweb.png"), pb_spider, width = 10, units = "cm", scale = 1.5)

# save data
#plot_data <- c(plot_data, "pb_spider & pb_circle" = list(fp_spider))


## Circular planetary boundary chart (experimental) -------------

# rescale values
pbs <- cbind(pbs, "range" = pbs[,"upper"] - pbs[,"lower"])
fp_circle <- fp_agg_sel
fp_circle[,2:7] <- t((t(fp_agg_sel[,2:7])-pbs[, "lower"])/pbs[, "range"])+1

pb_circle <- sapply(c("sq", "epo", "eat"), circle_plot_grad, fp_table = fp_circle, ylim.min = -0.0, ylim.max = 4, log = FALSE, legend = FALSE,
                    simplify = FALSE, USE.NAMES = TRUE)
(pb_circle <- wrap_plots(pb_circle, guides = "collect", nrow = 1) & theme(legend.position = 'bottom',
                                                                          legend.direction = 'horizontal'))

if (write) ggsave(paste0(plot_dir,"/v",vers,"/cond/pb_circle.png"), pb_circle, width = 30, units = "cm", scale = 1)



# Pyramid 2.0 -------------------------------------------------------

fp_sq_item <- fp_aggregate(fp_cond_list$sq, aggregate_by = c("country_consumer", "item_target", "country_target", "comm_group_plot"), indicators = c("production", "landuse", "blue", "ghg", "luh", "ghg_eat", "ghg_all", "biomass", "biodiv", "n_application", "p_application"))

# merge
fp_sq_item <- merge(fp_sq_item, Y_food_aut[,.(item_code, item, area_iso, #comm_group_plot 
                                              t_consumed = food_t_pc, 
                                              t_net_consumed = food_t_pc_net, 
                                              kcal_net_consumed = food_kcal_pc_net,
                                              proteins_net_consumed = food_prot_pc_net,
                                              fat_net_consumed = food_fat_pc_net)], 
                    by.x = c("item_target", "country_target"), by.y = c("item", "area_iso"))

all.equal(sum(fp_sq_item$kcal_net_consumed)/365, sum(Y_food_aut$food_kcal_pc_day_net))


# compute footprint by kcal
inds <-  c("production", "landuse", "blue", "ghg", "luh", "ghg_pb", "ghg_all", "biomass", "biodiv", "n_application", "p_application")
fp_sq_item[, (paste0(inds,"_per_kcal")) := lapply(.SD, '/', (kcal_net_consumed)), .SDcols = inds]
fp_sq_item[, (paste0(inds,"_per_gprot")) := lapply(.SD, '/', (proteins_net_consumed)), .SDcols = inds]
fp_sq_item[, (paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

# same on aggregate item level
fp_sq_item_agg <- fp_aggregate(fp_sq_item, aggregate_by = c("country_consumer", "item_target", "comm_group_plot"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed", "proteins_net_consumed"))
fp_sq_item_agg <- fp_sq_item_agg[, -grep("_per_", names(fp_sq_item_agg)), , with = FALSE]
fp_sq_item_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', (kcal_net_consumed)), .SDcols = inds]
fp_sq_item_agg[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', (proteins_net_consumed)), .SDcols = inds]
fp_sq_item_agg[,(paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

if (write) write.csv(fp_sq_item_agg, paste0(plot_dir,"/v",vers,"/cond/tables/fp_sq_item_agg.csv"), fileEncoding="UTF-16LE")

# and on group level
fp_sq_group_agg <- fp_aggregate(fp_sq_item, aggregate_by = c("country_consumer", "comm_group_plot"), indicators = c(inds, "t_consumed", "t_net_consumed", "kcal_net_consumed", "proteins_net_consumed"))
fp_sq_group_agg <- fp_sq_group_agg[, -grep("_per_", names(fp_sq_group_agg)), , with = FALSE]
fp_sq_group_agg[,(paste0(inds,"_per_kcal")) := lapply(.SD, '/', kcal_net_consumed), .SDcols = inds]
fp_sq_group_agg[,(paste0(inds,"_per_g_protein")) := lapply(.SD, '/', proteins_net_consumed), .SDcols = inds]
fp_sq_group_agg[,(paste0(inds,"_per_g")) := lapply(.SD, '/', (t_net_consumed*1e6)), .SDcols = inds]

if (write) write.csv(fp_sq_group_agg, paste0(plot_dir,"/v",vers,"/cond/tables/fp_sq_group_agg_by.csv"), fileEncoding="UTF-16LE")


# check nutritional values
sum(Y_food_aut$epo2_cond_kcal_pc_day_net)
sum(Y_food_aut$epo2_cond_prot_pc_day_net)# - sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut$epo2_cond_g_pc_day_net)

sum(Y_food_aut$epo2_kcal_pc_day_net)
sum(Y_food_aut$epo2_prot_pc_day_net)# - sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut$epo2_g_pc_day_net)

sum(Y_food_aut$epo_kcal_pc_day_net)
sum(Y_food_aut$epo_prot_pc_day_net)
sum(Y_food_aut$epo_g_pc_day_net)

# calculate portions per epo group
Y_food_aut_epo_sub <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = .(epo_subgroup, epo_group)]
Y_food_aut_epo_sub <- Y_food_aut_epo_sub[, `:=`(epo_port_day_group = sum(epo_port_day), epo2_port_day_group = sum(epo2_port_day)), by = epo_group]
Y_food_aut_epo_item <- Y_food_aut[, .(epo_port_day = sum(epo_g_pc_day_net/g_port, na.rm = TRUE), epo2_port_day = sum(epo2_g_pc_day_net/ g_port, na.rm = TRUE)), by = c("epo_subgroup", "epo_group", "item", "g_port")]
if(write) write.csv(Y_food_aut_epo_sub, paste0(plot_dir,"/v",vers,"/cond/tables/Y_food_aut_epo_sub.csv"))
if(write) write.csv(Y_food_aut_epo_item,  paste0(plot_dir,"/v",vers,"/cond/tables/Y_food_aut_epo_item.csv"))


### calculate footprint of EPO 2.0 --------

### create plots for EPO 2.0 ----------------

#### stack plots ----
indicators <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
pb_stack_list2 <- sapply(indicators, function(ind){
  stacked_bars(fp_list = list("sq" = fp_cond_list$sq,  "epo" = fp_cond_list$epo_cond, "eat" = fp_cond_list$eat_cond, "epo2" = fp_cond_list$epo2_cond), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack2 <- wrap_plots(pb_stack_list2, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))
if(write) ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/epo/pb_stack_epo2_full.png"), pb_stack2, width = 30, height = 25, units = "cm") 

pb_stack_list2_comp <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("epo" = fp_cond_list$epo_cond, "epo2" = fp_cond_list$epo2_cond), 
               indicator = ind, axis_lab = indicator_labs[ind], bound = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)
(pb_stack2_comp <- wrap_plots(pb_stack_list2_comp, nrow = 2, nocl = 3, guides = "collect") & theme(legend.position = "bottom"))

if(write) ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/epo/pb_stack_epo2.png"), pb_stack2_comp, width = 25, height = 25, units = "cm") 

# with reversed legend for single plots
indicators_ext <- c(indicators, "ghg_pb", "luh")
indicator_ext_labs <- c(indicator_labs, 
                        "ghg_pb" = "THG-Emissionen (exkl. Energie & LUC) in t CO<sub>2</sub>-Äq.", 
                        "luh" = "THG-Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.")

pb_stack_list_rev_epo <- sapply(indicators_ext, function(ind){
  stacked_bars(fp_list = list("sq" = fp_cond_list$sq,  "epo" = fp_cond_list$epo_cond, "eat" = fp_cond_list$eat_cond, "epo2" = fp_cond_list$epo2_cond), 
               indicator = ind, axis_lab = indicator_ext_labs[ind], bound = TRUE, reverse_legend = TRUE)
}, simplify = FALSE, USE.NAMES = TRUE)

# save plot
if(write){
  # also save single plots
  for (i in 1:length(pb_stack_list_rev_epo)) {
    ggsave(filename=paste0(plot_dir,"/v",vers,"/cond/epo/stack/pb_stack_", names(pb_stack_list_rev_epo)[i],".png"), 
           plot=pb_stack_list_rev_epo[[i]] + theme(legend.direction="vertical", legend.box = "certical", legend.position = "right"), width = 20, height = 12, units = "cm")
  }
}


#### diet plots -------

Y_agg2 <- Y_food_aut[, lapply(.SD, sum, na.rm=TRUE), by = comm_group_plot, .SDcols = c("food_g_pc_day_net",    "eat_cond_g_pc_day_net",    "epo_cond_g_pc_day_net",    "epo2_cond_g_pc_day_net",
                                                                                       "food_kcal_pc_day_net", "eat_cond_kcal_pc_day_net", "epo_cond_kcal_pc_day_net", "epo2_cond_kcal_pc_day_net",
                                                                                       "food_prot_pc_day_net", "eat_cond_prot_pc_day_net", "epo_cond_prot_pc_day_net", "epo2_cond_prot_pc_day_net",
                                                                                       "food_fat_pc_day_net",  "eat_cond_fat_pc_day_net",  "epo_cond_fat_pc_day_net",  "epo2_cond_fat_pc_day_net")]
Y_agg2_long <- Y_agg2 %>%
  rename_with(~gsub("_pc_day_net", "", .x, fixed = TRUE)) %>%
  rename_with(~gsub("_cond", "", .x, fixed = TRUE)) %>%
  pivot_longer(names_to = c("diet", ".value"), cols = -comm_group_plot, names_sep = "\\_") %>%
  filter(g > 0) %>%
  mutate(diet_lab = ifelse(diet == "food", "Status Quo", ifelse(diet == "eat", "Planetary Health Diet", ifelse(diet == "epo", "Ernährungspyramide", "Ernährungspyramide 2.0"))))

food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(Y_agg2_long$comm_group_plot)]


# plot
(diet_plot2_g <- ggplot(Y_agg2_long, 
                        aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                            y = g, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_kcal <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = kcal, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Kcal pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_prot <- ggplot(Y_agg2_long, 
                           aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                               y = prot, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Proteine in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

(diet_plot2_fat <- ggplot(Y_agg2_long, 
                          aes(x = factor(diet_lab, levels = rev(c("Status Quo", "Planetary Health Diet", "Ernährungspyramide", "Ernährungspyramide 2.0"))), 
                              y = fat, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend()) +
    labs(y = "Fette in Gramm pro Kopf und Tag", x = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
          axis.text.y = element_text(face = "bold", size = 10)))

#save plot
if (write) {
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/epo/diet_plot_g.png"), diet_plot2_g, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/epo/diet_plot_kcal.png"), diet_plot2_kcal, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/epo/diet_plot_prot.png"), diet_plot2_prot, width = 12, height = 3.5)
  ggsave(filename = paste0(plot_dir,"/v",vers,"/cond/epo/diet_plot_fat.png"), diet_plot2_fat, width = 12, height = 3.5)
  # write.xlsx(Y_agg)
}


#### circle plot --------

fp_agg <- as.data.frame(rbind(fp_cond_agg_list$sq, fp_cond_agg_list$epo_cond, fp_cond_agg_list$eat_cond, fp_cond_agg_list$epo2_cond))
fp_agg$diet <- factor(c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet", "Ernährungs- \npyramide \n2.0"), 
                      levels = c("Status \nQuo", "Ernährungs- \npyramide", "Planetary \nHealth Diet","Ernährungs- \npyramide \n2.0"))
fp_agg_sel <- fp_agg %>% rename(group = diet) %>%
  select(c(group, landuse, blue, ghg_all, biodiv, n_application, p_application)) %>%
  rename(Flächenverbrauch = landuse, Wasserverbrauch = blue, Emissionen = ghg_all, Biodiversität = biodiv, Stickstoff = n_application, Phosphor = p_application)

fp_circle <- fp_agg_sel
fp_circle[,2:7] <- t((t(fp_agg_sel[,2:7])-pbs[, "lower"])/pbs[, "range"])+1
# rough fix in case there are negatives in the rescaeled values
# TODO: discuss and resolve
fp_circle[,2:7][fp_circle[,2:7] < 0] <- 0.5

pb_circle2 <- sapply(c("sq", "epo", "eat", "epo2"), circle_plot_grad, fp_table = fp_circle, ylim.min = -0.0, ylim.max = 4, log = FALSE, legend = FALSE,
                     simplify = FALSE, USE.NAMES = TRUE)
(pb_circle2 <- wrap_plots(pb_circle2, guides = "collect", nrow = 1) & theme(legend.position = 'bottom',
                                                                            legend.direction = 'horizontal'))

if (write) ggsave(paste0(plot_dir,"/v",vers,"/cond/epo/pb_circle.png"), pb_circle2, width = 40, units = "cm", scale = 1)



## save data ---------------

plot_data <- c(fp_map_data, fp_mosaic_data, 
               "diet_plot" = list(diet_data),
               "diets_detail" = list(Y_agg_item),
               "pb_stack" = list(pb_stack_data),
               "pb_bar" = list(fp_agg),
               #"pb_circle" = list(fp_spider),
               "fp_by_item" = list(fp_sq_item_agg),
               "ghg_detail" = list(stack_ghg_sq_data)
               #"fp_by_item_epo" = list(fp_epo_item_agg),
               #"fp_by_item_eat" = list(fp_eat_item_agg)
)

write.xlsx(plot_data, file = paste0(plot_dir,"/v",vers,"/cond/plot_data.xlsx"), overwrite = TRUE)
