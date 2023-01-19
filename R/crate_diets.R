# create diets #

library(data.table)
library(tidyr)
library(dplyr)
library(Matrix)

# load data ---------------------------------------

# select fabio version
vers <- "1.2" # or "1.2"
yr <- 2020

if (vers == "1.1"){
  eat_conc <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "concordance_1.1", na = "NA"))
  eat_diet <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "eat_diet", skip = 1, na = "NA"))
  epo_diet <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "epo_diet", na = "NA"))
  epo_portions <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "epo_portionsize", na = "NA"))
  fao_comp <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "fao_composition_1.1", skip = 1, na = "NA"))
  waste_shares <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "waste", na = "NA"))
  loss_shares <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "loss_1.1", na = "NA"))
  cbs <- readRDS("data/v1.1/cbs_tidy_food.rds")
  cbs_pop <- readRDS("data/v1.1/cbs_pop.rds")
  Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/Y.rds")
  io_codes <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.1/io_codes.csv")

} else {
  eat_conc <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "concordance", na = "NA"))
  eat_diet <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "eat_diet", skip = 1, na = "NA"))
  epo_diet <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "epo_diet", na = "NA"))
  epo_portions <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "epo_portionsize", na = "NA"))
  fao_comp <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "fao_composition", skip = 1, na = "NA"))
  waste_shares <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "waste", na = "NA"))
  loss_shares <- as.data.table(readxl::read_excel("inst/items_conc.xlsx", sheet = "loss", na = "NA"))
  cbs <- readRDS("data/v1.2/cbs_tidy_food.rds")
  cbs_pop <- readRDS("data/v1.2/cbs_pop.rds")
  Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds") # "~/fabio/v1.2/losses/Y.rds" 
  io_codes <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv") #  "~/fabio/v1.2/io_codes.csv"
}


is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))


# prepare caloric content data from food balances ------------------------------------------

# add remaining processing use to food consumption for selected items in v1.2

if (vers == "1.2") {
  # for 
  # - milk: 2848
  # - cocoa: 2633
  # - animal fats: 2737
  # - coconut oil: 2578
  proc_items <- c(2848, 2633, 2737, 2578)
  cbs[item_code %in% proc_items,  `:=`(food = food_all, food_kg_pc_yr = food_all_kg_pc_yr)]
}

# extract relevant cbs columns
cbs_food <- cbs[,.(area_code, area, item_code, item, year, food, food_kg_pc_yr, food_kcal_pc_day, fat_g_pc_day, prot_g_pc_day)]

# rename Veg and Fruit, Other (changed in new FBS)
cbs_food[item == "Vegetables, other", item := "Vegetables, Other"]
cbs_food[item == "Fruits, other", item := "Fruits, Other"]

# for v1.1, transform Sugar Raw into Sugar refined, as Sugar Refined is no longer in the data(?)
if (vers == "1.1"){
  cbs_food[item_code == 2542, c("food", "food_kg_pc_yr") := lapply(.SD, function(x) x *  0.935), 
           .SDcols = c("food", "food_kg_pc_yr")]
  cbs_food[item_code == 2542, `:=`(item_code = 2818, item = "Sugar, Refined Equiv")]
  
}

# calculate energy/protein/fat content per gram of food for each country and year
cbs_food[,food_g_pc_day := food_kg_pc_yr/365*1000]
cbs_food[, `:=`(kcal_g = food_kcal_pc_day/food_g_pc_day,
                fat_g = fat_g_pc_day/food_g_pc_day,
                prot_g = prot_g_pc_day/food_g_pc_day)]

# for items with (some) positive consumption but zero or very low calories, replace values by FAO global average (to avoid rounding errors)
cbs_food <- merge(cbs_food, fao_comp[,.(item_code, kcal_data, prot_data, fat_data)], by = c("item_code"), sort = FALSE)
cbs_food[food > 0 & food_kcal_pc_day < 5, kcal_g := kcal_data] # threshold value for inaccuracies: 5 kcal pc/day
cbs_food[food > 0 & fat_g_pc_day < 5, prot_g := prot_data]
cbs_food[food > 0 & prot_g_pc_day < 5, fat_g := fat_data]
cbs_food[, `:=` (kcal_data = NULL, prot_data=NULL, fat_data = NULL)]
# for cases with zero nutrient consumption but some positive consumption, correct nutrient consumption
cbs_food[food_g_pc_day > 0 & food_kcal_pc_day == 0, food_kcal_pc_day := food_g_pc_day*kcal_g]
cbs_food[food_g_pc_day > 0 & fat_g_pc_day == 0, fat_g_pc_day := food_g_pc_day*fat_g]
cbs_food[food_g_pc_day > 0 & prot_g_pc_day == 0, prot_g_pc_day := food_g_pc_day*prot_g]
cbs_food <- cbs_food %>% mutate(across(c(kcal_g,prot_g,fat_g),  ~replace(., !is.finite(.), 0))) # tidyr::replace_na(cbs_food, list(kcal_g=0, fat_g=0, prot_g = 0))

# add concordance
cbs_food <- merge(cbs_food, eat_conc, by = c("item_code", "item"), sort = FALSE)

cbs_food <- relocate(cbs_food, c(comm_group:comm_code), .after = item) %>%
  relocate(c(area_code:area))
cbs_food <- select(cbs_food, -unit)
setkey(cbs_food, area_code, comm_code, year)

# extract Austrian fbs for year of analysis
cbs_food_aut <- cbs_food[area == "Austria" & year == yr,]
saveRDS(cbs_food_aut, paste0("data/v",vers,"/cbs_food_aut_",yr,".rds"))

# prepare final demand: ----------------------------------------------------------

# extract Austrian food vector
Y_food_aut <- as.vector(Y[[paste(yr)]][,"11_food"])
Y_food_aut <- as.data.table(cbind(io_codes, "food_t" = Y_food_aut))

# for 1.2. add processing of relevant items to food
if(vers == "1.2"){
  Y_food_aut[,proc_t := as.vector(Y[[paste(yr)]][,"11_processing"])]
  Y_food_aut[item_code %in% proc_items, food_t := food_t + proc_t]
  Y_food_aut[, proc_t := NULL]
}

setkey(Y_food_aut, area_code, comm_code)

# compute consumption in grams per capita and per day
aut_pop <- cbs_pop[year == yr & area == "Austria", value] * 1000

Y_food_aut[, food_g_pc := food_t*1e6/aut_pop]
Y_food_aut[, food_g_pc_day := food_g_pc/365]


## compute status-quo consumption in the units relevant for the comparison diets: ----------
# in caloric units for EAT, and in "portions" for the nutrition pyramid

# merge with caloric content data
Y_food_aut <- merge(Y_food_aut, eat_conc[,.(item_code, eat_group_fin, epo_group, epo_subgroup)], by = "item_code", all.x = TRUE, sort = FALSE)
Y_food_aut <- merge(Y_food_aut, cbs_food_aut[,.(item_code, kcal_g, prot_g, fat_g)], by = "item_code", all.x = TRUE, sort = FALSE)

# add fao global caloric content data for items no longer in FBS (Sugar Refined, Meat meal, Molasses..)
Y_food_aut <- merge(Y_food_aut, fao_comp[,.(item_code,kcal_data, prot_data, fat_data)], by = "item_code", all.x = TRUE, sort = FALSE)
Y_food_aut[is.na(kcal_g), kcal_g := kcal_data]
Y_food_aut[is.na(prot_g), prot_g := prot_data]
Y_food_aut[is.na(fat_g),  fat_g  := fat_data]
Y_food_aut[, `:=` (kcal_data = NULL, prot_data=NULL, fat_data = NULL)]
#Y_food_aut[,c("kcal_g", "prot_g", "fat_g")][is.na(Y_food_aut[,c("kcal_g", "prot_g", "fat_g")])] <-  0 #Y_food_aut <- Y_food_aut %>% mutate(across(kcal_g:fat_g,  ~replace(., !is.finite(.), 0)))
Y_food_aut <- setnafill(Y_food_aut, fill = 0, cols = c("kcal_g", "prot_g", "fat_g"))

# compute gross consumption in kcal/protein/fat per capita and day 
Y_food_aut[, food_kcal_pc_day := food_g_pc_day*kcal_g]
Y_food_aut[, food_prot_pc_day := food_g_pc_day*prot_g]
Y_food_aut[, food_fat_pc_day := food_g_pc_day*fat_g]


# merge with nutrition pyramid portion size data --> for nutrition pyramid diet
Y_food_aut <- merge(Y_food_aut, epo_portions[,.(epo_subgroup, g_port = `g/portion`)], by = "epo_subgroup", all.x = TRUE, sort = FALSE)

# compute gross consumption in portions per capita and day
Y_food_aut[, food_port_pc_day := food_g_pc_day/g_port]

# compute daily per capita consumption net of waste and losses
waste_shares[, waste_fin := distribution_fin + final_consumption_fin - distribution_fin*final_consumption_fin] # last term to take sequential loss into account (= 1 - (1-distribution_fin)*(1-final_consumption_fin))
Y_food_aut <- merge(Y_food_aut, eat_conc[,.(item_code, waste_group)], by = c("item_code"), all.x = TRUE, sort = FALSE)
Y_food_aut <- merge(Y_food_aut, as.data.table(waste_shares)[,.(waste_group, waste_fin)], by = c("waste_group"), all.x = TRUE, sort = FALSE)
#Y_food_aut$waste_fin[is.na(Y_food_aut$waste_fin)] <- 0
Y_food_aut <- merge(Y_food_aut, loss_shares[,.(item_code, loss)], by = c("item_code"), all.x = TRUE, sort = FALSE)
#Y_food_aut$loss[is.na(Y_food_aut$loss)] <- 0
Y_food_aut <- setnafill(Y_food_aut, fill = 0, cols = c("waste_fin", "loss"))

# NOTE: consumption in kcal/protein/fat is already net of losses (inedible parts). Thus only waste shares need to be subtracted here.
# consumption in grams is including losses, thus also needs to deduct loss shares. 

Y_food_aut[, `:=` (food_g_pc_day_net = food_g_pc_day * (1-waste_fin) * (1-loss),  # (1-waste_fin-loss) # NOTE: distribution & final comnsumption auch sequenziell!
                   food_kcal_pc_day_net = food_kcal_pc_day * (1-waste_fin),
                   food_port_pc_day_net = food_port_pc_day * (1-waste_fin) *(1-loss), # portions are calcualted from grams, thus also need to deduct losses
                   food_prot_pc_day_net = food_prot_pc_day * (1-waste_fin),
                   food_fat_pc_day_net = food_fat_pc_day * (1-waste_fin))]

setkey(Y_food_aut, area_code, comm_code)


## compute consumption of comparison diets ------------------------------------------

### EAT: calculate consumption sums by EAT group (net kcal consumption) -------
Y_food_aut[, eat_group_sum := sum(food_kcal_pc_day_net), by = eat_group_fin]
# Y_food_aut[, eat_group_share := ifelse(is.finite(food_kcal_pc_day_net/eat_group_sum), food_kcal_pc_day_net/eat_group_sum, 0)]

# add EAT net consumption suggestions
eat_diet <-  as.data.table(eat_diet)
eat_diet <- eat_diet[, .(eat_g_day = sum(`g/day`), eat_kcal_day = sum(`kcal/day`)), by = "eat_group_fin"]
Y_food_aut <- merge(Y_food_aut, eat_diet, by = "eat_group_fin", all.x = TRUE, sort = FALSE)

# calculate net EAT consumption pc and day (using kcal because eat gram values are in dry matter for some items)
# items not covered by EAT  (group "Other") are assumed to stay constant in their consumption (coffee, alcohol...)
Y_food_aut[, eat_rescaler := ifelse(is.finite(eat_kcal_day), (eat_kcal_day/eat_group_sum), 1)]
#Y_food_aut[, eat_kcal_pc_day_net := ifelse(is.finite(eat_kcal_day), food_kcal_pc_day_net*(eat_kcal_day/eat_group_sum), food_kcal_pc_day_net)]
Y_food_aut[, eat_kcal_pc_day_net := food_kcal_pc_day_net*eat_rescaler]
#Y_food_aut[, eat_g_pc_day_net := eat_kcal_pc_day_net*(1/kcal_g)*(1-loss)]
Y_food_aut[, eat_g_pc_day_net := food_g_pc_day_net*eat_rescaler]
#Y_food_aut[, eat_prot_pc_day_net := eat_g_pc_day_net*prot_g*(1/(1-loss))]
Y_food_aut[, eat_prot_pc_day_net := food_prot_pc_day_net*eat_rescaler]
#Y_food_aut[, eat_fat_pc_day_net := eat_g_pc_day_net*fat_g*(1/(1-loss))]
Y_food_aut[, eat_fat_pc_day_net := food_fat_pc_day_net*eat_rescaler]
Y_food_aut[, eat_port_pc_day_net := food_port_pc_day_net*eat_rescaler]

#Y_food_aut[, (c("eat_kcal_pc_day_net","eat_g_pc_day_net","eat_prot_pc_day_net","eat_fat_pc_day_net")) := replace(.SD, is.na(.SD), 0), .SDcols = c("eat_kcal_pc_day_net","eat_g_pc_day_net","eat_prot_pc_day_net","eat_fat_pc_day_net")]
Y_food_aut <- setnafill(Y_food_aut, fill = 0, cols = c("eat_kcal_pc_day_net", "eat_g_pc_day_net", "eat_prot_pc_day_net", "eat_fat_pc_day_net"))

# note that we work with kcal values directly, avoiding any conversions from dry weight to fresh weight
# the eat diet is by definition normalized to 2500 kcal per day (excluding "Other" food items)


### Ernährungspyramide Österreich (EPO): compute portions per pyramid group and day ---------
Y_food_aut[, epo_group_port_sum := sum(food_port_pc_day_net), by = epo_group]
# add portion suggestions by group 
Y_food_aut <- merge(Y_food_aut, epo_diet[,.(epo_group, epo_port_day = `portions/day`)], by = "epo_group", all.x = TRUE, sort = FALSE)
#Y_food_aut[, (c("epo_group_port_sum","portions_day")) := replace(.SD, is.na(.SD), 0), .SDcols = c("epo_group_port_sum","portions_day")]
Y_food_aut[, epo_rescaler := ifelse(is.finite(epo_port_day), (epo_port_day/epo_group_port_sum), 1)]
#Y_food_aut[, `:=`(epo_g_pc_day_net = ifelse(is.finite(epo_port_day), food_g_pc_day_net*epo_port_day/epo_group_port_sum, food_g_pc_day_net),
#                  epo_kcal_pc_day_net = ifelse(is.finite(epo_port_day), food_kcal_pc_day_net*epo_port_day/epo_group_port_sum, food_kcal_pc_day_net))
#           ]
Y_food_aut[, `:=`(epo_g_pc_day_net = food_g_pc_day_net*epo_rescaler,
                  epo_kcal_pc_day_net = food_kcal_pc_day_net*epo_rescaler,
                  epo_prot_pc_day_net = food_prot_pc_day_net*epo_rescaler,
                  epo_fat_pc_day_net = food_fat_pc_day_net*epo_rescaler,
                  epo_port_pc_day_net = food_port_pc_day_net*epo_rescaler)
           ]

#Y_food_aut[, epo_kcal_pc_day_net := epo_g_pc_day_net*(1/(1-loss))*kcal_g]
#Y_food_aut[, epo_prot_pc_day_net := epo_g_pc_day_net*(1/(1-loss))*prot_g]
#Y_food_aut[, epo_fat_pc_day_net := epo_g_pc_day_net*(1/(1-loss))*fat_g]

# NOTE: items without portion recommendation (alcohol, coffee & tea) are kept at their status quo level here as well!


# transform net consumption values per day and capita into loss- and waste-inclusive per-capita values per year
Y_food_aut[, eat_g_pc := eat_g_pc_day_net * (1/(1-waste_fin)) * (1/(1-loss)) * 365]
Y_food_aut[, epo_g_pc := epo_g_pc_day_net * (1/(1-waste_fin)) * (1/(1-loss)) * 365]

Y_food_aut[, food_t_pc := food_g_pc * 1e-6]
Y_food_aut[, eat_t_pc := eat_g_pc * 1e-6]
Y_food_aut[, epo_t_pc := epo_g_pc * 1e-6]

Y_food_aut <- relocate(Y_food_aut, c(eat_group_fin, epo_group, epo_subgroup, waste_group), .after = group)
Y_food_aut <- relocate(Y_food_aut, c(area_code, area, comm_code, item_code, item, comm_group, group))



# some consistency checks ------------------------------------------

# check net consumption per capita and day in kalories
sum(Y_food_aut$food_kcal_pc_day_net)
sum(Y_food_aut$eat_kcal_pc_day_net)
sum(Y_food_aut$epo_kcal_pc_day_net)

# without Others
sum(Y_food_aut[eat_group_fin != "Others",]$food_kcal_pc_day_net)
sum(Y_food_aut[eat_group_fin != "Others",]$eat_kcal_pc_day_net)
sum(Y_food_aut[eat_group_fin != "Others",]$epo_kcal_pc_day_net)

# compare to total food consumption according to cbs
sum(cbs_food_aut$food_kcal_pc_day)
cbs_food_tot <- cbs_food[, .(food_kcal_pc_day = sum(food_kcal_pc_day)), by = c("area", "year") ]

# standardize the nutrition pyramid intake to 2500 kcal as well?
# epo intake excluding "Others" group of eat diet
# epo_food_sum <- sum(Y_food_aut[eat_group_fin != "Others",]$epo_kcal_pc_day_net)
# Y_food_aut[eat_group_fin != "Others", `:=`(
#   epo_kcal_pc_day_net = epo_kcal_pc_day_net * 2500/epo_food_sum,
#   epo_g_pc = epo_g_pc * 2500/epo_food_sum,
#   epo_t_pc = epo_t_pc * 2500/epo_food_sum
# )]
# 
# sum(Y_food_aut[eat_group_fin != "Others",]$epo_kcal_pc_day_net)
# sum(Y_food_aut$epo_kcal_pc_day_net)
# sum(Y_food_aut$eat_kcal_pc_day_net)


# save final demand table ----------------------------------------------
#Y_food_aut <- select(Y_food_aut, -c(kcal_g:prot_g, g_port,waste_fin, loss))
saveRDS(Y_food_aut, file = ifelse(vers == "1.1", paste0("data/v1.1/Y_food_aut_",yr,".rds"), paste0("data/v1.2/Y_food_aut_",yr,".rds")))
