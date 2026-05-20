context("unlist2d and rowbind")


test_that("unlist2d returns non-list input unchanged", {
  expect_equal(unlist2d(1:3), 1:3)
})

test_that("unlist2d stacks lists", {
  l <- list(a = mtcars[1:5, ], b = mtcars[6:10, ])
  u <- unlist2d(l, idcols = "id")
  r <- rowbind(a = l$a, b = l$b, idcol = "id")
  expect_equal(nrow(u), nrow(r))
  expect_equal(ncol(u), ncol(r))
  expect_equal(length(unique(u[[1L]])), 2L)
})

test_that("unlist2d id.factor options work", {
  l <- list(a = mtcars[1:3, ], b = mtcars[4:6, ])
  r1 <- unlist2d(l, idcols = "id", id.factor = TRUE)
  r2 <- unlist2d(l, idcols = "id", id.factor = FALSE)
  expect_true(is.factor(r1[[1L]]))
  expect_false(is.factor(r2[[1L]]))
})

test_that("unlist2d recursive option", {
  l <- list(x = list(y = 1:3, z = 4:6))
  expect_true(is.list(unlist2d(l, recursive = FALSE)))
  r <- unlist2d(l, recursive = TRUE)
  expect_true(is.data.frame(r) || inherits(r, "data.frame"))
  expect_equal(nrow(r), 2L)
})

test_that("unlist2d data.table output", {
  skip_if_not_installed("data.table")
  l <- list(mtcars[1:5, ], mtcars[6:10, ])
  expect_true(data.table::is.data.table(unlist2d(l, DT = TRUE)))
})

test_that("unlist2d without idcols matches rowbind", {
  l <- list(mtcars[1:5, ], mtcars[6:10, ])
  expect_equal(unlist2d(l, idcols = FALSE), rowbind(l[[1]], l[[2]]))
})
