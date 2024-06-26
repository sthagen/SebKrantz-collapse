context("roworder, colorder, frename")



test_that("roworder works as intended", {

  expect_identical(roworder(mtcars, cyl, -hp), mtcars[with(mtcars, order(cyl, -hp)), ])
  expect_identical(roworder(airquality, Month, -Ozone), setRownames(airquality[with(airquality, order(Month, -Ozone)), ]))
  expect_identical(fnrow(roworder(airquality, Month, -Ozone, na.last = NA)), 116L)  # Removes the missing values in Ozone

  ## Same in standard evaluation
  expect_identical(roworderv(airquality, c("Month", "Ozone"), decreasing = c(FALSE, TRUE)), roworder(airquality, Month, -Ozone))

  ## Custom reordering
  expect_identical(roworderv(mtcars, neworder = 3:4), rbind(mtcars[3:4, ], mtcars[-(3:4), ]))               # Bring rows 3 and 4 to the front
  expect_identical(roworderv(mtcars, neworder = 3:4, pos = "end"), rbind(mtcars[-(3:4), ], mtcars[3:4, ]))  # Bring them to the end
  expect_identical(roworderv(mtcars, neworder = mtcars$vs == 1), rbind(mtcars[mtcars$vs == 1, ], mtcars[mtcars$vs != 1, ]))    # Bring rows with vs == 1 to the top
  expect_identical(ss(roworderv(mtcars, neworder = c(8, 2), pos = "exchange"), c(2,8)), ss(mtcars, c(8,2)))

})

if(identical(Sys.getenv("NCRAN"), "TRUE") && requireNamespace("magrittr", quietly = TRUE)) {

library(magrittr)
test_that("colorder works as intended", {
  expect_identical(colorder(mtcars, vs, cyl:hp, am),
                   fselect(mtcars, vs, cyl:hp, am, return = "indices") %>% {cbind(mtcars[.], mtcars[-.])})
  expect_identical(colorder(mtcars, vs, cyl:hp, am, pos = "end"),
                   fselect(mtcars, vs, cyl:hp, am, return = "indices") %>% {cbind(mtcars[-.], mtcars[.])})
  expect_identical(colorder(mtcars, vs, cyl:hp, am, pos = "exchange"),
                   fselect(mtcars, vs, cyl:hp, am, return = "indices") %>% {`get_vars<-`(mtcars, sort(.), value = mtcars[.])})

  ## Same in standard evaluation
  expect_identical(colorder(mtcars, vs, cyl:hp, am),
                   colorderv(mtcars, c(8, 2:4, 9)))
  expect_identical(colorder(mtcars, vs, cyl:hp, am, pos = "end"),
                   colorderv(mtcars, c(8, 2:4, 9), pos = "end"))
  expect_identical(colorder(mtcars, vs, cyl:hp, am, pos = "exchange"),
                   colorderv(mtcars, c(8, 2:4, 9), pos = "exchange"))

  expect_identical(colorder(mtcars, vs, cyl, am),
                   colorderv(mtcars, c("vs", "cyl|am"), regex = TRUE))

})

}

test_that("frename works as intended", {

  ## Using tagged expressions
  expect_equal(frename(iris, Sepal.Length = SL, Sepal.Width = SW,
                       Petal.Length = PL, Petal.Width = PW), setNames(iris, .c(SL, SW, PL, PW, Species)))

  expect_equal(frename(iris, Sepal.Length = "S L", Sepal.Width = "S W",
                       Petal.Length = "P L", Petal.Width = "P W"), setNames(iris, c("S L", "S W", "P L", "P W", "Species")))

  ## Using a function
  expect_equal(frename(iris, tolower), setNames(iris, tolower(names(iris))))
  expect_equal(frename(iris, tolower, cols = 1:2), setNames(iris, c(tolower(names(iris)[1:2]), names(iris)[-(1:2)])))
  expect_equal(frename(iris, tolower, cols = is.numeric), setNames(iris, c(tolower(names(iris)[1:4]), names(iris)[-(1:4)])))
  expect_equal(frename(iris, paste, "new", sep = "_", cols = 1:2), setNames(iris, c(paste(names(iris)[1:2], "new", sep = "_"), names(iris)[-(1:2)])))

  ## Using vectors of names and programming
  expect_equal(frename(iris, tolower), frename(iris, tolower(names(iris)), .nse = FALSE))
  newname = "sepal_length"
  expect_equal(frename(iris, Sepal.Length = newname, .nse = FALSE), setNames(iris, c(newname, names(iris)[-1L])))
  newnames = c("sepal_length", "sepal_width")
  expect_true(all_obj_equal(frename(iris, newnames, cols = 1:2),
                            frename(iris, newnames, cols = 1:2, .nse = FALSE),
                            setNames(iris, c(newnames, names(iris)[-(1:2)]))))
  newnames = c(Sepal.Length = "sepal_length", Sepal.Width = "sepal_width")
  expect_equal(frename(iris, newnames, .nse = FALSE), setNames(iris, c(newnames, names(iris)[-(1:2)])))

  if(requireNamespace("data.table", quietly = TRUE)) {

  ## Renaming by reference
  iris2 <- data.table::copy(iris)
  setrename(iris2, tolower)
  expect_equal(iris2, setNames(iris, tolower(names(iris))))
  iris2 <- data.table::copy(iris)
  setrename(iris2, tolower, cols = 1:2)
  expect_equal(iris2, setNames(iris, c(tolower(names(iris)[1:2]), names(iris)[-(1:2)])))
  iris2 <- data.table::copy(iris)
  setrename(iris2, tolower, cols = is.numeric)
  expect_equal(iris2, setNames(iris, c(tolower(names(iris)[1:4]), names(iris)[-(1:4)])))
  iris2 <- data.table::copy(iris)
  setrename(iris2, paste, "new", sep = "_", cols = 1:2)
  expect_equal(iris2, setNames(iris, c(paste(names(iris)[1:2], "new", sep = "_"), names(iris)[-(1:2)])))
  rm(iris2)

  nam <- toupper(names(iris))

  # Relabelling with functions
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, tolower)
  expect_equal(iris2, setLabels(iris, tolower(nam)))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, tolower, cols = 1:2)
  expect_equal(iris2, setLabels(iris, c(tolower(nam[1:2]), nam[-(1:2)])))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, tolower, cols = is.numeric)
  expect_equal(iris2, setLabels(iris, c(tolower(nam[1:4]), nam[5])))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, paste, "new", sep = "_", cols = 1:2)
  expect_equal(iris2, setLabels(iris, c(paste(nam[1:2], "new", sep = "_"), nam[-(1:2)])))

  # Relabelling other
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, Sepal.Length = "sepal.length", Sepal.Width = "sepal.width")
  expect_equal(iris2, setLabels(iris, c(tolower(nam[1:2]), nam[-(1:2)])))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, tolower(nam))
  expect_equal(iris2, setLabels(iris, tolower(nam)))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, tolower(nam[1:2]), cols = 1:2)
  expect_equal(iris2, setLabels(iris, c(tolower(nam[1:2]), nam[-(1:2)])))
  iris2 <- data.table::copy(setLabels(iris, nam))
  setrelabel(iris2, setNames(tolower(nam[1:2]), c("Sepal.Length", "Sepal.Width")))
  expect_equal(iris2, setLabels(iris, c(tolower(nam[1:2]), nam[-(1:2)])))

  vlabels(iris) <- NULL
  rm(iris2)

  }

})
