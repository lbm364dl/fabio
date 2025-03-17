library("data.table")
source("R/01_tidy_functions.R")

rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Element" = "element",
  "Year" = "year",
  "Unit" = "unit",
  "Value" = "value",
  # After casting
  "Domestic supply quantity" = "total_supply",
  "Export Quantity" = "exports",
  "Feed" = "feed",
  "Food" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Residuals" = "residuals",
  "Tourist consumption" = "tourist",
  "area_code" = "area_code",
  "area" = "area",
  "item_code" = "item_code",
  "item" = "item",
  "year" = "year"
)

# CBS ---------------------------------------------------------------------

cat("\nTidying CBS.\n")

# food: transform to tonnes and filter pre-2014 values from new fbs
cbs_food_old <- readRDS("input/fao/cbs_food_old.rds")[Year <= 2013, ]
cbs_food_old[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value * 1000), Unit = "tonnes")]
cbs_food_new <- readRDS("input/fao/cbs_food_new.rds")[Year > 2013, ]
cbs_food_new[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value * 1000), Unit = "tonnes")]
# Stock Variation seems to be defined wrongly, sign needs to be changed
# NOTE: this is inconsistently defined (sometimes correct, sometimes wrong), so it is corrected further below in the balancing section

# nonfood: remove items contained in food balances
cbs_nonfood_old <- readRDS("input/fao/cbs_nonfood_old.rds")[Year < 2010, ]
cbs_nonfood_new <- readRDS("input/fao/cbs_nonfood_new.rds")[Year >= 2010, ]
cbs_nonfood <- rbind(cbs_nonfood_old, cbs_nonfood_new[, 1:(ncol(cbs_nonfood_new) - 1)])
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]
cbs_nonfood <- merge(cbs_nonfood, cbs_food_old[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
  all.x = TRUE,
  by = c("Area Code", "Item Code", "Element", "Year Code"),
  suffixes = c("", ".food_old")
)
cbs_nonfood <- merge(cbs_nonfood, cbs_food_new[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
  all.x = TRUE,
  by = c("Area Code", "Item Code", "Element", "Year Code"),
  suffixes = c("", ".food_new")
)
cbs_nonfood <- cbs_nonfood[is.na(Value.food_old) & is.na(Value.food_new), ]
cbs_nonfood <- cbs_nonfood[, `:=`(Value.food_old = NULL, Value.food_new = NULL)]

# bind
cbs <- rbind(cbs_food_old, cbs_food_new, cbs_nonfood, fill = TRUE)

cbs <- dt_rename(cbs, rename, drop = TRUE)
rm(cbs_nonfood_old, cbs_nonfood_new, cbs_nonfood, cbs_food_old, cbs_food_new)

cbs[element == "Processed", element := "Processing"]

# fix error: wrong units for stock changes
cbs[element == "Stock Variation" & unit == "1000 An", unit := "1000 t"]
# change units to tonnes
cbs[unit == "t", unit := "tonnes"]
cbs[unit == "1000 t", `:=`(unit = "tonnes", value = value * 1000)]

# Standardize both names to 'Quantity'
cbs[element == "Export quantity", element := "Export Quantity"]
cbs[element == "Import quantity", element := "Import Quantity"]

# Widen by element
cbs <- data.table::dcast(cbs, area_code + area + item_code + item + year ~ element,
  value.var = "value"
) # fun.aggregate = sum, na.rm = TRUE sum is used her because remaining duplicates only contain NAs in food balance
cbs <- dt_rename(cbs, rename, drop = TRUE)

# Replace NA values with 0
cbs <- dt_replace(cbs, is.na, value = 0)
# Make sure values are not negative
cbs <- dt_replace(
  cbs,
  function(x) `<`(x, 0),
  value = 0,
  cols = c(
    "imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed"
  )
)

cat(
  "Recoding 'total_supply' from",
  "'production + imports - exports + stock_withdrawal'", "to",
  "'production + imports'.\n"
)
cbs[, total_supply := na_sum(production, imports)]

# Add more intuitive 'stock_addition'
cbs[, stock_addition := -stock_withdrawal]


# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(
  total_supply,
  -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other, -residuals, -tourist
)] #

# correct mistakes in stock variation reporting: this was reported with inconsistent signs
cbs[
  ((balancing / stock_addition < -1.9) & is.finite(balancing / stock_addition)) |
    (data.table::between(-2 * stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),
  `:=`(
    stock_addition = -stock_addition,
    stock_withdrawal = -stock_withdrawal,
    balancing = balancing + 2 * stock_addition,
    corr = TRUE,
    ratio = balancing / stock_addition
  )
]

cbs[, `:=`(corr = NULL, ratio = NULL)]

# Store
data_tidy_path <- "data/"
if (!dir.exists(data_tidy_path)) {
  dir.create(data_tidy_path, recursive = TRUE)
}
saveRDS(cbs, paste0(data_tidy_path, "cbs_full.rds"))
