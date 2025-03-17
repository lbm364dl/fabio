
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

# CBS ---------------------------------------------------------------------

cat("\nBuilding full CBS.\n")

cbs <- readRDS("data/tidy/cbs_tidy.rds")

cat("Removing items from CBS that are not used in FABIO:\n\t",
  paste0(unique(cbs[!item_code %in% items$item_code, item]),
    sep = "", collapse = "; "), ".\n", sep = "") # Eggs and Milk are duplicated with different codes (2948, 2949)
# Particularly fish and aggregates
cbs <- dt_filter(cbs, item_code %in% items$item_code)


# Prepare BTD data --------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/tidy/btd_full_tidy.rds")

cat("\nGiving preference to units in the following order:\n",
  "\t 'head' > 'tonnes'\n", "Dropping 'usd'.\n", sep = "")

# Imports
imps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
  value.var = "value")
imps[, `:=`(value = ifelse(!is.na(head), head, tonnes),
  head = NULL, tonnes = NULL)]

# Exports
exps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
  value.var = "value")
exps[, `:=`(value = ifelse(!is.na(head), head, tonnes),
  head = NULL, tonnes = NULL)]


# # Forestry ----------------------------------------------------------------
#
# cat("\nAdding forestry production data.\n")
#
# fore <- readRDS("data/tidy/fore_prod_tidy.rds")
#
# fore[, `:=`(total_supply = na_sum(production, imports),
#   other = na_sum(production, imports, -exports),
#   stock_withdrawal = 0, stock_addition = 0,
#   feed = 0, food = 0, losses = 0, processing = 0,
#   seed = 0, balancing = 0)]
# fore[other < 0, `:=`(balancing = other, other = 0)]
#
# cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
# rm(fore)







# Add BTD data ------------------------------------------------------------

cat("\nAdding missing export and import data to CBS from BTD.\n")
cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
rm(imps, exps)


# Create RoW --------------------------------------------------------------

# remove regions "Unspecified" and "Others (adjustment)"
cbs <- cbs[! area %in% c("Unspecified", "Others (adjustment)")]

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[cbs == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[cbs == TRUE, code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "comm_code", "item_code", "item", "unit", "year")]

# Remove ROW-internal trade from CBS
intra <- btd[from_code==to_code & unit!="usd", sum(value), by=c("from_code","from","item_code","item","year")]
cbs <- merge(cbs, intra,
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
  imports = na_sum(imports,-V1))]
cbs[, V1 := NULL]
rm(intra)

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)


# Rebalance columns -------------------------------------------------------

# TODO: this could be improved! Decide how to use residuals here.

cat("\nRebalance CBS.\n")

# Round to 0 digits
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "balancing", "unspecified",
         "residuals", "tourist")
cbs[, (cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = cols]

# Replace negative values with '0'
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
  cols = c("imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed", "tourist"))

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
        -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified, -tourist, -residuals)]

# Note: the sum of balancing and residuals should never be <0 in the end
# as long as we do not introduce an "unknown source" region, we need to adapt the other cbs use elements accordingly

# # neutralize negative balancing via other items
# cat("\nAdjust 'residuals' for ", cbs[balancing < 0 &
#                                      !is.na(residuals) & residuals >= -balancing*0.99, .N],
#     " observations, where `balancing < 0` and `residuals >= -balancing` to ",
#     "`residuals = residuals - balancing`.\n", sep = "")
# cbs[balancing < 0 & !is.na(residuals) & residuals >= -balancing*0.99,
#     `:=`(residuals = na_sum(residuals, balancing),
#          balancing = 0)]


cat("\nAdjust 'exports' for ", cbs[na_sum(balancing, residuals) < 0 &
    !is.na(exports) & exports >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `exports >= -na_sum(balancing, residuals)` to ",
    "`exports = exports - na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(exports) & exports >= na_sum(balancing, residuals)*0.99,
    `:=`(exports = na_sum(exports, balancing, residuals),
         balancing = 0, residuals = 0)]
cbs[exports < 0, `:=`(balancing = balancing + exports,
                      exports = 0)]

cat("\nAdjust 'processing' for ", cbs[na_sum(balancing, residuals) < 0 &
    !is.na(processing) & processing >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `processing >= -na_sum(balancing, residuals)` to ",
    "`processing = processing + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(processing) & processing >= -na_sum(balancing, residuals)*0.99,
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0, residuals = 0)]
cbs[processing < 0, `:=`(balancing = balancing + processing,
                         processing = 0)]

cat("\nAdjust 'other' for ", cbs[na_sum(balancing, residuals) < 0 &
    !is.na(other) & other >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `other >= -na_sum(balancing, residuals)` to ",
    "`other = other + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(other) & other >= -na_sum(balancing, residuals)*0.99,
    `:=`(other = na_sum(other, balancing, residuals),
         balancing = 0, residuals = 0)]
cbs[other < 0, `:=`(balancing = balancing + other,
                    other = 0)]

# cat("\nAdjust 'tourist' for ", cbs[balancing < 0 &
#     !is.na(tourist) & tourist >= -balancing, .N],
#     " observations, where `balancing < 0` and `tourist >= -balancing` to ",
#     "`tourist = tourist + balancing`.\n", sep = "")
# cbs[balancing < 0 & !is.na(other) & other >= -balancing,
#     `:=`(other = na_sum(other, balancing),
#          balancing = 0)]

cat("\nAdjust 'unspecified' for ", cbs[na_sum(balancing, residuals) < 0 &
    !is.na(unspecified) & unspecified >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `unspecified >= -na_sum(balancing, residuals)` to ",
    "`unspecified = unspecified + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(unspecified) & unspecified >= -na_sum(balancing, residuals)*0.99,
    `:=`(unspecified = na_sum(unspecified, balancing, residuals),
         balancing = 0, residuals = 0)]
cbs[unspecified < 0, `:=`(balancing = balancing + unspecified,
                          unspecified = 0)]

cat("\nAdjust 'stock_addition' for ", cbs[na_sum(balancing, residuals) < 0 &
    !is.na(stock_addition) & stock_addition >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `stock_addition >= -na_sum(balancing, residuals)` to ",
    "`stock_addition = stock_addition + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(stock_addition) & stock_addition >= -na_sum(balancing, residuals)*0.99,
    `:=`(stock_addition = na_sum(stock_addition, balancing, residuals),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing, -residuals),
         balancing = 0, residuals = 0)]

cat("\nAdjust uses proportionally for ", cbs[na_sum(balancing, residuals) < 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0`", sep = "")
cbs[, divisor := na_sum(exports, other, processing, seed, food, feed, stock_addition, tourist)]
cbs[na_sum(balancing, residuals) < 0 & divisor >= -na_sum(balancing, residuals),
    `:=`(stock_addition = round(na_sum(stock_addition, (na_sum(balancing, residuals) / divisor * stock_addition))),
         processing = round(na_sum(processing, (na_sum(balancing, residuals) / divisor * processing))),
         exports = round(na_sum(exports, (na_sum(balancing, residuals) / divisor * exports))),
         other = round(na_sum(other, (na_sum(balancing, residuals) / divisor * other))),
         seed = round(na_sum(seed, (na_sum(balancing, residuals) / divisor * seed))),
         food = round(na_sum(food, (na_sum(balancing, residuals) / divisor * food))),
         feed = round(na_sum(feed, (na_sum(balancing, residuals) / divisor * feed))),
         tourist = round(na_sum(tourist, (na_sum(balancing, residuals) / divisor * tourist))),
         balancing = 0, residuals = 0)]
cbs[, `:=`(stock_withdrawal = -stock_addition,
           divisor = NULL)]

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified, -tourist, -residuals)]

# Attribute rest (resulting from rounding differences) to stock changes
cbs[na_sum(balancing, residuals) < 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0, residuals = 0)]
cbs[balancing < 0, `:=`(residuals = residuals + balancing, balancing = 0)]
cbs[residuals < 0, `:=`(balancing = residuals + balancing, residuals = 0)]

# Adjust 'total_supply' to new production values
cbs[, total_supply := na_sum(production, imports)]

cat("\nSkip capping 'exports', 'seed' and 'processing' at",
  "'total_supply + stock_withdrawal'.\n")


# # Balance CBS imports and exports -------------------------------------------------------
# # --> This balancing step was moved to script 05_balance.R
#
# # Adjust CBS to have equal export and import numbers per item per year
# # This is very helpful for the iterative proportional fitting of bilateral trade data
# cbs_bal <- cbs[, list(exp_t = sum(exports, na.rm = TRUE), imp_t = sum(imports, na.rm = TRUE)),
#                by = c("year", "item_code", "item")]
# cbs_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
#                area_code = 999, area = "RoW")]
# # Absorb the discrepancies in "RoW"
# cbs <- merge(cbs, cbs_bal,
#              by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
# cbs[area_code == 999, `:=`(
#   exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
#   imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
# cbs[, diff := NULL]
#
# rm(cbs_bal); gc()
#
# # Rebalance RoW
# cbs[, balancing := na_sum(production, imports, stock_withdrawal,
#                           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]
#
# cbs[, divisor := na_sum(other, processing, seed, food, feed, stock_addition, unspecified)]
# cbs[balancing < 0 & divisor >= -balancing,
#     `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
#          unspecified = round(na_sum(unspecified, (balancing / divisor * unspecified))),
#          processing = round(na_sum(processing, (balancing / divisor * processing))),
#          other = round(na_sum(other, (balancing / divisor * other))),
#          seed = round(na_sum(seed, (balancing / divisor * seed))),
#          food = round(na_sum(food, (balancing / divisor * food))),
#          feed = round(na_sum(feed, (balancing / divisor * feed))),
#          balancing = 0)]
# cbs[, `:=`(stock_withdrawal = -stock_addition, divisor = NULL,
#            balancing = na_sum(production, imports, stock_withdrawal,
#           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified))]
# cbs[balancing < 0,
#     `:=`(production = na_sum(production, -balancing),
#          balancing = 0)]
# cbs[, total_supply := na_sum(production, imports)]



cat("\nAllocate remaining supply from 'unspecified' and 'balancing' to uses.\n")

cat("\nHops, oil palm fruit, palm kernels, sugar crops and live animals to 'processing'.\n")
cbs[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
  1126, 1157, 1140, 1150, 1171, 2536, 2537, 2562) & na_sum(unspecified, balancing, residuals) > 0,
  `:=`(processing = na_sum(processing, unspecified, balancing, residuals),
       unspecified = 0, balancing = 0, residuals = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
  1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(unspecified, balancing, residuals, processing) > 0,
  `:=`(other = na_sum(other, unspecified, balancing, residuals, processing),
       unspecified = 0, balancing = 0, residuals = 0, processing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
  2595, 2596, 2597, 2598, 2749) & na_sum(unspecified, balancing, residuals) > 0,
  `:=`(feed = na_sum(feed, unspecified, balancing, residuals),
       unspecified = 0, balancing = 0, residuals = 0)]

cat("\nRest (mostly 'food', 'feed' and 'processing') remains in 'unspecified', 'balancing' and 'residuals'.\n")


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
