context("collapse options")


test_that("get_collapse and set_collapse round-trip", {
  old <- get_collapse()
  on.exit(set_collapse(old), add = TRUE)

  set_collapse(na.rm = FALSE, sort = FALSE, digits = 5, verbose = 0, stub = FALSE)
  expect_equal(get_collapse("na.rm"), FALSE)
  expect_equal(get_collapse("sort"), FALSE)
  expect_equal(get_collapse(c("digits", "verbose", "stub")),
               stats::setNames(list(5L, 0L, FALSE), c("digits", "verbose", "stub")))

  set_collapse(list(na.rm = old$na.rm, sort = old$sort, digits = old$digits,
                    verbose = old$verbose, stub = old$stub))
})

test_that("set_collapse validates options", {
  old <- get_collapse()
  on.exit(set_collapse(old), add = TRUE)

  expect_error(set_collapse(nthreads = 0))
  expect_error(set_collapse(na.rm = NA))
  expect_error(set_collapse(sort = NA))
  expect_error(set_collapse(digits = -1L))
  expect_error(set_collapse(verbose = -1L))
  expect_error(set_collapse(unknown = TRUE))
})

test_that("set_collapse stable.algo round-trip", {
  old <- get_collapse()
  on.exit(set_collapse(old), add = TRUE)

  set_collapse(stable.algo = FALSE)
  expect_equal(get_collapse("stable.algo"), FALSE)
  set_collapse(stable.algo = old$stable.algo)
})

test_that("set_collapse returns previous options invisibly", {
  old <- get_collapse()
  prev <- set_collapse(verbose = old$verbose)
  expect_equal(prev, old)
  set_collapse(old)
})

test_that("set_collapse mask manip smoke test", {
  old <- get_collapse()
  on.exit({
    set_collapse(mask = old$mask, remove = old$remove)
  }, add = TRUE)

  if(length(old$mask)) set_collapse(mask = NULL, remove = NULL)
  set_collapse(mask = "manip")
  expect_equal(get_collapse("mask"), "manip")
  expect_true(exists("fselect", where = asNamespace("collapse"), inherits = FALSE))
})
