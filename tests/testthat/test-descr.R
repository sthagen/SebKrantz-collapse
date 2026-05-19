context("descr")


test_that("descr returns expected structure", {
  d <- descr(wlddev, cols = c("iso3c", "POP", "LIFEEX"))
  expect_s3_class(d, "descr")
  expect_equal(attr(d, "name"), "wlddev")
  expect_equal(attr(d, "names"), c("iso3c", "POP", "LIFEEX"))
  expect_true("Table" %in% names(d$iso3c) || "Stats" %in% names(d$iso3c))
  expect_true(is.numeric(d$POP$Stats) || is.list(d$POP$Stats) || is.matrix(d$POP$Stats))
})

test_that("descr grouped by works", {
  d <- descr(wlddev, by = ~ region, cols = c("POP", "LIFEEX"))
  expect_s3_class(d, "descr")
  expect_true(!is.null(attr(d, "groups")))
  expect_true(length(d) >= 2L)
})

test_that("descr with weights", {
  wld <- transform(wlddev, POP = replace_NA(POP))
  d <- descr(wld, cols = c("POP", "LIFEEX"), w = ~ POP)
  expect_s3_class(d, "descr")
  expect_equal(length(attr(d, "weights")), nrow(wld))
})

test_that("descr flags control output", {
  d1 <- descr(wlddev, cols = "iso3c", table = FALSE)
  expect_null(d1$iso3c$Table)
  d2 <- descr(wlddev, cols = "POP", Ndistinct = FALSE, higher = FALSE)
  expect_s3_class(d2, "descr")
  d3 <- descr(wlddev, cols = "iso3c", sort.table = "value")
  expect_s3_class(d3, "descr")
  d4 <- descr(wlddev, cols = "iso3c", sort.table = "none")
  expect_s3_class(d4, "descr")
})

test_that("descr S3 methods work", {
  d <- descr(wlddev, cols = c("iso3c", "POP"))
  expect_output(print(d))
  df <- as.data.frame(d)
  expect_true(is.data.frame(df))
  expect_true(nrow(df) >= 1L)
})

test_that("descr grouped_df method works", {
  g <- fgroup_by(fsubset(wlddev, iso3c %in% c("USA", "DEU", "JPN")), region)
  d <- descr(fselect(g, POP, LIFEEX))
  expect_s3_class(d, "descr")
})

test_that("descr errors for invalid sort.table", {
  expect_error(descr(wlddev, cols = "iso3c", sort.table = "invalid"))
})
