cols <- c(
  "Reporter Country Code", "Reporter Countries",
  "Partner Country Code", "Partner Countries",
  "Item Code", "Item", "Element",
  "Year", "Unit", "Value"
)

chunk <- function(df, pos) {
  write_path <- "short_trade.csv"
  print(pos)
  print(df)
  if (pos == 1) {
    cols |>
      paste(collapse = ",") |>
      writeLines(write_path)
  }

  df |>
    dplyr::select(cols) |>
    # dplyr::select(-Item) |>
    readr::write_csv(write_path, append = TRUE)
}

readr::read_csv_chunked(
  "input/fao/btd_prod.csv",
  readr::DataFrameCallback$new(chunk),
  chunk_size = 1000000
)

drop_cols <- c("Reporter Country Code (M49)", "Partner Country Code (M49)", "Item Code (CPC)", "Year Code", "Element Code", "Flag")

df <- data.table::fread("input/fao/btd_prod.csv", stringsAsFactors = TRUE, drop = drop_cols)
