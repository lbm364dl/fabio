na_sum <- function(...) {
  mat <- cbind(...)
  cols <- dim(mat)[2]
  if (cols == 1) {
    mat <- t(mat)
    cols <- dim(mat)[2]
  }

  is_all_row_na <-
    mat |>
    is.na() |>
    rowSums(na.rm = TRUE) |>
    (`==`)(cols)

  mat |>
    rowSums(na.rm = TRUE) |>
    ifelse(is_all_row_na, yes = NA, no = _)
}

library(testthat)

test_that("gives correct result", {
  df <- data.table::data.table(a = c(1, 2, 3, NA), b = c(4, NA, 6, NA))
  df[, s := na_sum(a, b)]
  expect_equal(df[, s], c(5, 2, 9, NA))

  c(NA, NA, NA) |>
    na_sum() |>
    expect_equal(NA)

  c(3, NA, NA) |>
    na_sum() |>
    expect_equal(3)

  c(3, 4, NA) |>
    na_sum() |>
    expect_equal(7)

  c(3, 4, 1) |>
    na_sum() |>
    expect_equal(8)
})

# Checking this na_sum is fast
# Just creating the data.table takes longer than computing na_sum
n <- 10000000
df <- data.table::data.table(
  a = rpois(n, lambda = 100), b = rpois(n, lambda = 100)
)
df[, s := na_sum(a, b)]
