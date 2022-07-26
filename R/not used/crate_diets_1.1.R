# create diets #

library(data.table)
library(tidyr)
library(dplyr)
library(Matrix)

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
#eat_conc <- as.data.table(openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "concordance"))
eat_conc <- as.data.table(openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "concordance_1.1"))
eat_diet <- openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "diets", startRow = 2)
#fao_comp <- as.data.table(openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "fao_composition", startRow = 2))
fao_comp <- as.data.table(openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "fao_composition_1.1", startRow = 2))
waste_shares <- as.data.table(openxlsx::read.xlsx("input_diets/items_full_eat.xlsx", sheet = "waste"))
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))


#cbs <- readRDS("data/tidy/cbs_tidy_food.rds")
cbs <- readRDS("data/tidy/cbs_tidy_food_v1.1.rds")
cbs_food <- cbs[,.(area_code, area, item_code, item, year, food, food_kg_pc_yr, food_kcal_pc_day, fat_g_pc_day, prot_g_pc_day)]
#cbs_pop <- readRDS("data/tidy/cbs_pop.rds")
cbs_pop <- readRDS("data/tidy/cbs_pop_v.1.1.rds")

# rename Veg and Fruit, Other
cbs_food[item == "Vegetables, other", item := "Vegetables, Other"]
cbs_food[item == "Fruits, other", item := "Fruits, Other"]


# calculate energy/protein/fat content per gram of food for each country and year
cbs_food[,food_g_pc_day := food_kg_pc_yr/365*1000]
cbs_food[, `:=`(kcal_g = food_kcal_pc_day/food_g_pc_day,
                fat_g = fat_g_pc_day/food_g_pc_day,
                prot_g = prot_g_pc_day/food_g_pc_day)]
# for items with (some) positive consumption but zero calories, replace values by FAO global average
cbs_food <- merge(cbs_food, fao_comp[,.(item_code, kcal_data, prot_data, fat_data)], by = c("item_code"), sort = FALSE)
cbs_food[food > 0 & food_kcal_pc_day < 5, kcal_g := kcal_data] # threshhold value for inaccuracies: 5 kcal pc/day
cbs_food[food > 0 & fat_g_pc_day < 5, prot_g := prot_data]
cbs_food[food > 0 & prot_g_pc_day < 5, fat_g := fat_data]
cbs_food[, `:=` (kcal_data = NULL, prot_data=NULL, fat_data = NULL)]
cbs_food <- cbs_food %>% mutate(across(c(kcal_g,prot_g,fat_g),  ~replace(., !is.finite(.), 0))) # tidyr::replace_na(cbs_food, list(kcal_g=0, fat_g=0, prot_g = 0))
cbs_food <- merge(cbs_food, eat_conc, by = c("item_code", "item"), sort = FALSE)

cbs_food <- relocate(cbs_food, c(comm_group:comm_code), .after = item) %>%
  relocate(c(area_code:area))
cbs_food <- select(cbs_food, -unit)
setkey(cbs_food, area_code, comm_code, year)

# cbs_food_aut <- cbs_food[area == "Austria" & year == 2019,]
cbs_food_aut <- cbs_food[area == "Austria" & year == 2013,]


## load final Y matrix of FABIO 1.2.
#Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds")
#io_codes <- read.csv("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")

# load final Y matrix of FABIO 1.1.
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/Y.rds")
io_codes <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.1/io_codes.csv")

# extract Austrian food vector
#Y_food_aut <- as.vector(Y$`2019`[,"11_food"])
Y_food_aut <- as.vector(Y$`2013`[,"11_food"])

Y_food_aut <- as.data.table(cbind(io_codes, "food_t" = Y_food_aut))
setkey(Y_food_aut, area_code, comm_code)

# compute consumption in grams per capita and per day
#aut_pop <- cbs_pop[year == 2019 & area == "Austria", value] * 1000
aut_pop <- cbs_pop[year == 2013 & area == "Austria", value] * 1000

Y_food_aut[, food_g_pc := food_t*1e6/aut_pop]
Y_food_aut[, food_g_pc_day := food_g_pc/365]

# merge with content data
Y_food_aut <- merge(Y_food_aut, eat_conc[,.(item_code, eat_group_fin)], by = "item_code", all.x = TRUE, sort = FALSE)
Y_food_aut <- merge(Y_food_aut, cbs_food_aut[,.(item_code, kcal_g, prot_g, fat_g)], by = "item_code", all.x = TRUE, sort = FALSE)

# add fao global content data for items no longer in FBS (Sugar Refined, Meat meal, molasses..)
Y_food_aut <- merge(Y_food_aut, fao_comp[,.(item_code,kcal_data, prot_data, fat_data)], by = "item_code", all.x = TRUE, sort = FALSE)
Y_food_aut[is.na(kcal_g), kcal_g := kcal_data]
Y_food_aut[is.na(prot_g), prot_g := prot_data]
Y_food_aut[is.na(fat_g),  fat_g  := fat_data]
Y_food_aut[, `:=` (kcal_data = NULL, prot_data=NULL, fat_data = NULL)]

#Y_food_aut <- Y_food_aut %>% mutate(across(kcal_g:fat_g,  ~replace(., !is.finite(.), 0)))
Y_food_aut[,c("kcal_g", "prot_g", "fat_g")][is.na(Y_food_aut[,c("kcal_g", "prot_g", "fat_g")])] <-  0

# compute consumption in kcal per capita and day
Y_food_aut[, food_kcal_pc_day := food_g_pc_day*kcal_g]

# compute daily per capita consumption net of losses
waste_shares[, waste_fin := distribution_fin + final_consumption_fin]
Y_food_aut <- merge(Y_food_aut, eat_conc[,.(item_code, waste_group)], by = c("item_code"), all.x = TRUE, sort = FALSE)
Y_food_aut <- merge(Y_food_aut, as.data.table(waste_shares)[,.(waste_group, waste_fin)], by = c("waste_group"), all.x = TRUE, sort = FALSE)
Y_food_aut$waste_fin[is.na(Y_food_aut$waste_fin)] <- 0
Y_food_aut[, `:=` (food_g_pc_day_net = food_g_pc_day * (100-waste_fin)/100, food_kcal_pc_day_net = food_kcal_pc_day * (100-waste_fin)/100)]
setkey(Y_food_aut, area_code, comm_code)


## calculate import supply shares per country-product (net kcal consumption)
#Y_food_aut[, food_item_sum := sum(food_kcal_pc_day_net), by = comm_code]
#setkey(Y_food_aut, area_code, comm_code)
#Y_food_aut[, sup_share := ifelse(is.finite(food_kcal_pc_day_net/food_item_sum), food_kcal_pc_day_net/food_item_sum, 0)]

# calculate shares by EAT group (net kcal consumption)
#Y_food_aut[, eat_group_share := ifelse(is.finite(food/sum(food)), food/sum(food), 0), by = eat_group_fin]
Y_food_aut[, eat_group_sum := sum(food_kcal_pc_day_net), by = eat_group_fin]
Y_food_aut[, eat_group_share := ifelse(is.finite(food_kcal_pc_day_net/eat_group_sum), food_kcal_pc_day_net/eat_group_sum, 0)]

# add EAT net consumption suggestions
eat_diet <-  as.data.table(eat_diet)
eat_diet <- eat_diet[, .(eat_g_day = sum(`g/day`), eat_kcal_day = sum(`kcal/day`)), by = "eat_group_fin"]
Y_food_aut <- merge(Y_food_aut, eat_diet, by = "eat_group_fin", all.x = TRUE, sort = FALSE)


# calculate net EAT consumption pc and day (starting with kcal because eat gram values are in dry matter for some items)
# items not covered by EAT are assumed to stay constant in their consumption (coffee, alcohol...)
#Y_food_aut[, eat_kcal_pc_day_net1 := ifelse(is.finite(eat_kcal_day),eat_kcal_day * eat_group_share, food_kcal_pc_day_net) ]
#Y_food_aut[, eat_kcal_pc_day_net2 := ifelse(is.finite(eat_kcal_day),eat_kcal_day * sup_share * food_item_sum/eat_group_sum, food_kcal_pc_day_net) ]
Y_food_aut[, eat_kcal_pc_day_net := ifelse(is.finite(eat_kcal_day), food_kcal_pc_day_net*eat_kcal_day/eat_group_sum, food_kcal_pc_day_net)]
Y_food_aut[, eat_g_pc_day_net := eat_kcal_pc_day_net*(1/kcal_g)]
#Y_food_aut <- Y_food_aut %>% mutate(across(c(eat_kcal_pc_day_net,eat_g_pc_day_net),  ~replace(., !is.finite(.), 0)))
Y_food_aut[, (c("eat_kcal_pc_day_net","eat_g_pc_day_net")) := replace(.SD, is.na(.SD), 0), .SDcols = c("eat_kcal_pc_day_net","eat_g_pc_day_net")]

# transform dry values of EAT to fresh weight --> not necessary when we work with kcal values directly
# normalize to 2500 kcal per day --> already the case

# transform eat net values per day and capita into waste-inclusive per-capita values per year
Y_food_aut[, eat_g_pc := eat_g_pc_day_net * (100/(100-waste_fin)) * 365]
Y_food_aut[, food_t_pc := food_g_pc * 1e-6]
Y_food_aut[, eat_t_pc := eat_g_pc * 1e-6]


# save Y
saveRDS(Y_food_aut, file = "data_diets/Y_food_aut_2013.rds")

sum(Y_food_aut$food_kcal_pc_day)
sum(Y_food_aut$eat_kcal_pc_day)
sum(Y_food_aut$food_kcal_pc_day_net)
sum(Y_food_aut$eat_kcal_pc_day_net)
sum(Y_food_aut[eat_group_fin != "Others",]$food_kcal_pc_day_net)
sum(Y_food_aut[eat_group_fin != "Others",]$eat_kcal_pc_day_net)
sum(cbs_food_aut$food_kcal_pc_day)


cbs_food_tot <- cbs_food[, .(food_kcal_pc_day = sum(food_kcal_pc_day)), by = c("area", "year") ]
