
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


# Colnames ----------------------------------------------------------------

rename <- c(
  "Area Code" = "area_code",
  "AreaCode" = "area_code",
  "Area" = "area",
  "AreaName" = "area",
  "Item Code" = "item_code",
  "ItemCode" = "item_code",
  "Item" = "item",
  "ItemName" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  "ElementName" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value",
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  # After casting
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  #"Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses (non-food)" = "other", # Other uses
  "Processing" = "processing",
  # Units
  # "1000 US$" = "k_usd",
  # "1000 Head" = "k_capita",
  "Head" = "head",
  "tonnes" = "tonnes",
  "Export" = "exports",
  "Import" = "imports",
  # Fish
  "COUNTRY" = "country",
  # "AREA" = "water_area",
  "SOURCE" = "source_code",
  "SPECIES" = "species",
  "YEAR" = "year",
  "UNIT" = "unit",
  "QUANTITY" = "value",
  "Months" = "months",
  "Tourist consumption" = "tourist",
  "Food" = "food",
  "Residuals" = "residuals",
  #"Geographic.Area" = "area",
  #"measuredElement" = "element_code",
  "COUNTRY.UN_CODE" = "country",
  "SPECIES.ALPHA_3_CODE" = "species",
  #"AREA.CODE" = "water_area",
  "PRODUCTION_SOURCE_DET.CODE" = "source_code",
  "MEASURE" = "unit",
  "PERIOD" = "year",
  "VALUE" = "value",
  "code" = "area_code",
  "name" = "area",
  "Fat supply quantity (g/capita/day)" = "fat_g_pc_day",
  "Food supply (kcal/capita/day)" = "food_kcal_pc_day",
  "Food supply quantity (kg/capita/yr)" = "food_kg_pc_yr",
  "Protein supply quantity (g/capita/day)" = "prot_g_pc_day"
)

# CBS ---------------------------------------------------------------------

cat("\nTidying CBS.\n")

#cbs_v2 <- rbind(readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/cbs_crop.rds"),
#   readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/cbs_live.rds"))

# food: filter post-2010 values from old balances and transform to tonnes
cbs_food_old <- readRDS("input/fao/cbs_food_old.rds")[Year <= 2013,]
cbs_food_old[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]
#cbs_food_new <- readRDS("input/fao/cbs_food_new.rds")
#cbs_food_new[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]

# nonfood: remove items contained in food balances
cbs_nonfood <- readRDS("input/fao/cbs_nonfood.rds")[Year <= 2013,]
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]

cbs_nonfood <- merge(cbs_nonfood, cbs_food_old[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food_old"))
#cbs_nonfood <- merge(cbs_nonfood, cbs_food_new[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
#                     all.x = TRUE,
#                     by = c("Area Code", "Item Code", "Element", "Year Code"),
#                     suffixes = c("", ".food_new"))
cbs_nonfood <- cbs_nonfood[is.na(Value.food_old) ,]
cbs_nonfood <- cbs_nonfood[,`:=` (Value.food_old = NULL )]


# bind
cbs <- rbind(cbs_food_old, cbs_nonfood)
#rm(cbs_nonfood, cbs_food_old, cbs_food_new)
cbs <- dt_rename(cbs, rename, drop = TRUE)

## transform items that changed from old to new FBS method
## "Groundnuts (Shelled Eq)" in "Groundnuts"
#cbs[item == "Groundnuts (Shelled Eq)", `:=` (item_code = 2552, item = "Groundnuts", value = 1/0.7 * value)]
## “Rice (milled equivalent)” into "Rice and products" via TCF
#cbs[item == "Rice (Milled Equivalent)", `:=` (item_code = 2807, item = "Rice and products", value = 1/0.67 * value)]
## Note: Sugar (Raw Equivalent) was also present in old FBS

# aggregate tourist consumption and residuals into other uses and drop unused elements
cbs[element %in% c("Tourist consumption"), element := "Other uses (non-food)"]
cbs <- cbs[,.(value = sum(value)), by = setdiff(names(cbs), "value")]


cbs_pop <- cbs[element == "Total Population - Both sexes",]
cbs_food <- cbs[element %in% c("Food",
                               "Food supply (kcal/capita/day)",
                               "Food supply quantity (kg/capita/yr)",
                               "Fat supply quantity (g/capita/day)",
                               "Protein supply quantity (g/capita/day)")]

cbs <- cbs[(! element %in% c(#"Food supply (kcal/capita/day)",
                            #"Food supply quantity (kg/capita/yr)",
                            #"Fat supply quantity (g/capita/day)",
                            #"Protein supply quantity (g/capita/day)",
                            "Total Population - Both sexes")) & item != "Population"]

# Country / Area adjustments
cbs <- area_kick(cbs, code = 351, pattern = "China", groups = TRUE)
cbs <- area_merge(cbs, orig = 62, dest = 238, pattern = "Ethiopia")
cbs <- area_merge(cbs, orig = 206, dest = 276, pattern = "Sudan")
cbs <- area_fix(cbs, regions)

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

# Add more intuitive 'stock_addition' and fix discrepancies with 'total_supply'
cbs[, stock_addition := -stock_withdrawal]
cat("Found ", cbs[stock_addition > total_supply, .N],
    " occurences of 'stock_addition' exceeding 'total_supply'.\n",
    "Keeping values as is.\n", sep = "")
# cbs[stock_addition > total_supply, stock_addition := total_supply]

# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
                          -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other)] # tourist
# remove residuals, which are now contained in balancing
#cbs[, residuals := NULL]

# TODO: imbalances in the data need to be double-checked! there are a lot of them!

# Store
saveRDS(cbs, "data/v1.1/cbs_tidy_food.rds")
saveRDS(cbs_pop, "data/v1.1/cbs_pop.rds")

