download <- function(url, destdir, alias, extension) {
  destfile <- stringr::str_glue("{destdir}/{alias}.{extension}")
  print("destfile")
  print(destfile)
  if (file.exists(destfile)) {
    return(destfile)
  }
  dir.create(destdir, recursive = TRUE)

  tryCatch(
    download.file(url, destfile, mode = "wb", method = "curl"),
    error = function(cond) {
      if (file.exists(destfile)) {
        file.remove(destfile)
      }
      stop("File was not downloaded correctly. Try again.")
    }
  )
  print(destfile)
  destfile
}

# Select codes for fish (30___) and ethanol (2207__)
filter_fish_and_ethanol <- function(destfile, ...) {
  print(stringr::str_glue("Reading {destfile}..."))
  baci_sel <- data.table::fread(destfile, stringsAsFactors = TRUE)
  baci_sel[grep("^(30[1-5]..|2207..)", baci_sel$k), ]
}

save_baci_rds <- function(input_files, k_path_trade, k_baci_group) {
  baci_rds_path <- paste0(k_path_trade, "baci_sel.rds")
  if (file.exists(baci_rds_path)) {
    return(baci_rds_path)
  }
  input_files |>
    dplyr::filter(group == k_baci_group) |>
    purrr::pmap(filter_fish_and_ethanol) |>
    data.table::rbindlist() |>
    saveRDS(baci_rds_path)
}

save_rds <- function(destfile, destdir, alias, extension, drop_cols, ...) {
  if (!is.na(drop_cols)) {
    drop_cols <- stringr::str_split_1(drop_cols, ",")
  } else {
    drop_cols <- c()
  }

  rds_destfile <- stringr::str_glue("{destdir}/{alias}.rds")
  if (file.exists(rds_destfile)) {
    return(rds_destfile)
  }

  table <-
    if (extension == "xlsx") {
      destfile |>
        openxlsx::read.xlsx() |>
        data.table::as.data.table()
    } else {
      data.table::fread(
        destfile,
        stringsAsFactors = TRUE,
        drop = drop_cols,
        header = TRUE
      )
    }

  saveRDS(table, rds_destfile, compress = FALSE)
  rds_destfile
}

options(timeout = 1e6)

input_files <-
  "inst/input_files.csv" |>
  readr::read_csv() |>
  dplyr::rowwise() |>
  dplyr::mutate(destfile = download(url, destdir, alias, extension))

k_path_trade <- "input/trade/"

k_baci_group <- "baci"
save_baci_rds(input_files, k_path_trade, k_baci_group)

input_files |>
  dplyr::filter(group != k_baci_group) |>
  purrr::pmap(save_rds)
