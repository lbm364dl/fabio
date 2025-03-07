test <- function(destfile, url) {
  if (!file.exists(destfile)) {
    print(dirname(destfile))
    destfile |>
      dirname() |>
      dir.create(recursive = TRUE)
    path <- download.file(url, destfile, timeout = )
    print(path)
  }
}

options(timeout = 1e6)

input_files <-
  "inst/input_files.csv" |>
  readr::read_csv() |>
  dplyr::rowwise() |>
  dplyr::mutate(df = test(destfile, url))
