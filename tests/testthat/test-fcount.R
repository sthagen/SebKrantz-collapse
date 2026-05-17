context("fcount and fcountv")


mt <- mtcars
fcount_sort <- function(x, ..., sort = FALSE) {
  r <- fcount(x, ..., sort = sort)
  r[do.call(order, r[, setdiff(names(r), "N"), drop = FALSE]), , drop = FALSE]
}

test_that("fcount and fcountv agree", {
  expect_equal(fcount(mt, cyl, vs, am), fcountv(mt, cols = .c(cyl, vs, am)))
  expect_equal(fcountv(mt, cols = c("cyl", "vs", "am")), fcount(mt, cyl, vs, am))
})

test_that("fcount matches dplyr::count", {
  skip_if_not_installed("dplyr")
  mtt <- dplyr::as_tibble(mt)
  d <- dplyr::count(mtt, cyl, vs, am, name = "N", sort = FALSE)
  expect_equal(unattrib(fcount_sort(mt, cyl, vs, am)), unattrib(as.data.frame(d)))
})

test_that("fcount add modes work", {
  full <- fcount(mt, cyl, vs, am)
  added <- fcount(mt, cyl, vs, am, add = TRUE)
  expect_equal(nrow(added), nrow(mt))
  expect_equal(added$N, full$N[match(interaction(added[, c("cyl", "vs", "am")], drop = TRUE, lex.order = TRUE),
                                        interaction(full[, c("cyl", "vs", "am")], drop = TRUE, lex.order = TRUE))])
  gv <- fcount(mt, cyl, vs, am, add = "group_vars")
  expect_equal(nrow(gv), nrow(mt))
  expect_equal(ncol(gv), 4L)
  expect_equal(gv[, c("cyl", "vs", "am")], mt[, c("cyl", "vs", "am")])
  expect_equal(fcount(mt, cyl, vs, am, add = "gv"), gv)
})

test_that("fcount sorting changes row order", {
  uns <- fcount(mt, cyl, vs, am, sort = FALSE)
  srt <- fcount(mt, cyl, vs, am, sort = TRUE)
  expect_false(identical(unattrib(uns), unattrib(srt)))
})

test_that("fcount with weights", {
  w <- runif(nrow(mt))
  r <- fcount(mt, cyl, w = w)
  expect_equal(sum(r$N), sum(w), tolerance = 1e-10)
})

test_that("fcount with grouped data", {
  g <- fgroup_by(mt, cyl, vs, am)
  expect_equal(unattrib(fcount_sort(g)), unattrib(fcount_sort(mt, cyl, vs, am)))
  expect_equal(nrow(fcount(g, add = TRUE)), nrow(mt))
  expect_equal(nrow(fcount(g, add = "group_vars")), nrow(mt))
})

test_that("fcount works on data.table", {
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(mt)
  expect_true(data.table::is.data.table(fcount(dt, cyl, vs, am)))
  expect_equal(fcount_sort(qDF(fcount(dt, cyl, vs, am))), fcount_sort(qDF(fcount(mt, cyl, vs, am))))
})

test_that("fcount errors for invalid input", {
  expect_error(fcount(mt, cyl, add = "invalid"))
  expect_error(fcountv(mt, w = "notacol"))
})
