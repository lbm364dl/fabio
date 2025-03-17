
library("data.table")
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv", stringsAsFactors = TRUE)

years <- 1986:2021

# BTD ---------------------------------------------------------------------

cat("\nBuilding full BTD.\n")

btd <- readRDS("data/tidy/btd_tidy.rds")

# Change from reporting & partner country to receiving & supplying country
btd[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  reporter = NULL, reporter_code = NULL,
  partner = NULL, partner_code = NULL)]

# Give preference to reported export flows over import flows
btd <- flow_pref(btd, pref = "Export")
btd[, imex := NULL]

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)


btd <- btd[item_code %in% items$item_code, ]

# Aggregate values
btd <- btd[, list(value = na_sum(value)), by = c("item_code", "item",
  "from", "from_code", "to", "to_code", "year", "unit")]

# Add commodity codes
btd[, comm_code := items$comm_code[match(btd$item_code, items$item_code)]]

# Subset to only keep head and usd for live animals
btd <- btd[!(comm_code %in% items[comm_group == "Live animals", comm_code] & unit == "tonnes")]

setorder(btd, by = year)

# Store -------------------------------------------------------------------

btd <- btd[year %in% years,]
saveRDS(btd, "data/tidy/btd_full.rds")
