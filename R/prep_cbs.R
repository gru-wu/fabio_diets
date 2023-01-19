
library("data.table")
source("~/fabio/R/01_tidy_functions.R")

regions <- fread("~/fabio/inst/regions_full.csv")


# Colnames ----------------------------------------------------------------

rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Element" = "element",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value",
  # After casting
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  "Loss" = "losses",
  "Food supply quantity (tonnes)" = "food",
  "Food" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses (non-food)" = "other", # Other uses
  "Processing" = "processing",
  "Processed" = "processing",
  "Residuals" = "residuals",
  "Tourist consumption" = "tourist",
  "Fat supply quantity (g/capita/day)" = "fat_g_pc_day",
  "Food supply (kcal/capita/day)" = "food_kcal_pc_day",
  "Food supply quantity (kg/capita/yr)" = "food_kg_pc_yr",
  "Protein supply quantity (g/capita/day)" = "prot_g_pc_day",
  "Total Population - Both sexes" = "population"
)

# CBS ---------------------------------------------------------------------

cat("\nTidying CBS.\n")

# food: transform to tonnes and filter pre-2014 values from new fbs
cbs_food_old <- readRDS("~/fabio/input/fao/cbs_food_old.rds")[Year <= 2013,]
cbs_food_old[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]
cbs_food_new <- readRDS("~/fabio/input/fao/cbs_food_new.rds")[Year > 2013,]
cbs_food_new[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]

# nonfood: remove items contained in food balances
cbs_nonfood <- readRDS("~/fabio/input/fao/cbs_nonfood.rds")
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]
cbs_nonfood[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]

cbs_nonfood <- merge(cbs_nonfood, cbs_food_old[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food_old"))
cbs_nonfood <- merge(cbs_nonfood, cbs_food_new[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food_new"))
cbs_nonfood <- cbs_nonfood[is.na(Value.food_old) & is.na(Value.food_new) ,]
cbs_nonfood <- cbs_nonfood[,`:=` (Value.food_old = NULL, Value.food_new = NULL )]


# bind
cbs <- rbind(cbs_food_old, cbs_food_new, cbs_nonfood, fill = TRUE)
cbs <- dt_rename(cbs, rename, drop = TRUE)
rm(cbs_nonfood, cbs_food_old, cbs_food_new)


# transform items that changed from old to new FBS method
elements <- c("Domestic supply quantity", "Production", "Import Quantity", "Export Quantity", "Feed", "Food", "Losses",
              "Other uses (non-food)", "Processing", "Residuals", "Seed", "Stock Variation", "Tourist consumption")
# "Groundnuts (Shelled Eq)" in "Groundnuts"
cbs[item == "Groundnuts (Shelled Eq)" & element %in% elements, `:=` (item_code = 2552, item = "Groundnuts", value = 1/0.7 * value)]
# “Rice (milled equivalent)” into "Rice and products" via TCF
cbs[item == "Rice (Milled Equivalent)" & element %in% elements, `:=` (item_code = 2807, item = "Rice and products", value = 1/0.67 * value)]
# Note: Sugar (Raw Equivalent) was also present in old FBS


# Country / Area adjustments
cbs <- area_kick(cbs, code = 351, pattern = "China", groups = TRUE)
cbs <- area_merge(cbs, orig = 62, dest = 238, pattern = "Ethiopia")
cbs <- area_merge(cbs, orig = 206, dest = 276, pattern = "Sudan")
cbs <- area_fix(cbs, regions)

cbs_pop <- cbs[element == "Total Population - Both sexes",]
cbs <- cbs[element != "Total Population - Both sexes",]

# Widen by element
cbs <- data.table::dcast(cbs, area_code + area + item_code + item + year ~ element,
                         value.var = "value") # fun.aggregate = sum, na.rm = TRUE sum is used her because remaining duplicates only contain NAs in food balance
cbs <- dt_rename(cbs, rename, drop = FALSE)

# Replace NA values with 0
cbs <- dt_replace(cbs, is.na, value = 0)
# Make sure values are not negative
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("imports", "exports", "feed", "food", "losses",
                           "other", "processing", "production", "seed"))

cat("Recoding 'total_supply' from",
    "'production + imports - exports + stock_withdrawal'", "to",
    "'production + imports'.\n")
cbs[, total_supply := na_sum(production, imports)]

# Add more intuitive 'stock_addition'
cbs[, stock_addition := -stock_withdrawal]


# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
                          -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other, -residuals, -tourist)] #


# rename 2 items in order to have identical name throughout the FAOSTAT data domains
cbs[, item := ifelse(item_code==2605,	"Vegetables, Other",
                     ifelse(item_code==2625, "Fruits, Other", item))]


######## add unallocated processing use from finished Y matrix

use_fd <- readRDS("~/fabio/data/use_fd_final.rds")

cbs_proc_fd <- use_fd[, .(year, item_code, area_code, processing_fd = processing)]
cbs_proc_fd[is.na(processing_fd), processing_fd := 0]

cbs <- merge(cbs, cbs_proc_fd, by = c("year", "area_code", "item_code"), all.x = TRUE)
cbs <- dt_replace(cbs, is.na, value = 0)

# add new food_all column
cbs[, food_all := na_sum(food, processing_fd)]

# calculate per capita consumption in kg
cbs <- merge(cbs, cbs_pop[,.(area_code, year, population = value*1000)], by = c("area_code", "year"))
cbs[, food_all_kg_pc_yr := (food_all/population)*1000]


# save data
saveRDS(cbs, "data/v1.2/cbs_tidy_food.rds")
saveRDS(cbs_pop, "data/v1.2/cbs_pop.rds")
