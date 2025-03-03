chunk <- function(df, pos) {
  print(pos)
  print(df)
  df |>
    dplyr::select(c(
      "Reporter Country Code", "Partner Country Code", "Item Code",
      "Element Code", "Year Code", "Unit", "Value", "Flag"
    )) |>
    readr::write_csv("short_trade.csv", append = TRUE)
}

readr::read_csv_chunked(
  "input/fao/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
  readr::DataFrameCallback$new(chunk),
  chunk_size = 1000000
)
