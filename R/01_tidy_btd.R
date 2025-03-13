library("data.table")
source("R/01_tidy_functions.R")

rename <- c(
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  "Item Code" = "item_code",
  "Item" = "item",
  "Element" = "element",
  "Year" = "year",
  "Unit" = "unit",
  "Value" = "value"
)

# BTD ---------------------------------------------------------------------

cat("\nTidying BTD.\n")

btd <- readRDS("input/fao/btd_prod.rds") |>
  dplyr::sample_n(10000) |>
  dt_rename(rename, drop = TRUE) |>
  dt_filter(value >= 0)

btd[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

btd[unit == "t", unit := "tonnes"]
btd[unit == "1000 An", `:=`(value = value * 1000, unit = "An")]
btd[unit == "An", `:=`(unit = "head")]
btd[unit == "1000 USD", `:=`(value = value * 1000, unit = "usd")]

# Store
saveRDS(btd, "data/tidy/btd_tidy.rds")
