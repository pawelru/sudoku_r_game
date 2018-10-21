context("sudoku checks functionality")

test_that("checks against uniqness", {
  expect_true(check_unique(c(1L:9L)))
  expect_false(check_unique(c(1L, 1L:8L)))

  expect_error(check_unique())
  expect_error(check_unique(NULL))
  expect_error(check_unique(1, 1))
  expect_error(check_unique('a'))
  expect_error(check_unique(c('a', 'b')))
})


test_that("checks against range", {
  expect_true(check_range(c(1L:9L)))
  expect_false(check_range(c(2L:10L)))
  expect_false(check_range(c(-1L:7L)))
  expect_false(check_range(c(-1L:10L)))

  expect_error(check_range())
  expect_error(check_range(NULL))
  expect_error(check_range(1, 1))
  expect_error(check_range('a'))
  expect_error(check_range(c('a', 'b')))
})


test_that("checking rows of sudoku matrix", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_null(check_rows(x))

  y <- x
  y[1, 1] <- 1L
  y[1, 2] <- 1L
  expect_equal(check_rows(y), 1)
  rm(y)

  y <- x
  y[1, 1] <- 1L
  y[1, 2] <- 1L
  y[9, 1] <- 1L
  y[9, 2] <- 1L
  expect_equal(check_rows(y), c(1, 9))
  rm(y)
})


test_that("checking columns of sudoku matrix", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_null(check_cols(x))

  y <- x
  y[1, 1] <- 1L
  y[2, 1] <- 1L
  expect_equal(check_cols(y), 1)
  rm(y)

  y <- x
  y[1, 1] <- 1L
  y[2, 1] <- 1L
  y[1, 9] <- 1L
  y[2, 9] <- 1L
  expect_equal(check_cols(y), c(1, 9))
  rm(y)
})


test_that("checking small squares of sudoku matrix", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_equal(check_squares(x), list())

  y <- x
  y[1, 1] <- 1L
  y[2, 2] <- 1L
  expect_equal(check_squares(y), list(list(c(1, 2, 3), c(1, 2, 3))))
  rm(y)

  y <- x
  y[1, 1] <- 1L
  y[2, 2] <- 1L
  y[8, 8] <- 1L
  y[9, 9] <- 1L
  expect_equal(check_squares(y), list(list(c(1, 2, 3), c(1, 2, 3)), list(c(7, 8, 9), c(7, 8, 9))))
  rm(y)
})


test_that("combined checks of sudoku matrix", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_equal(length(check_matrix(x)), 4)
  expect_equal(check_matrix(x), list(TRUE, NULL, NULL, list()))

  y <- x
  for (row in 1:3) {
    for (col in 1:9) {
      y[row, col] = NA_integer_
    }
  }
  for (row in 1:9) {
    for (col in 1:3) {
      y[row, col] = NA_integer_
    }
  }
  y[1, 1] <- 1L
  y[1, 2] <- 1L
  y[2, 1] <- 1L
  expect_equal(check_matrix(y), list(FALSE, 1, 1, list(list(c(1, 2, 3), c(1, 2, 3)))))
  rm(y)

  y <- x
  for (row in c(1:3, 7:9)) {
    for (col in 1:9) {
      y[row, col] = NA_integer_
    }
  }
  for (row in 1:9) {
    for (col in c(1:3, 7:9)) {
      y[row, col] = NA_integer_
    }
  }
  y[1, 1] <- 1L
  y[1, 2] <- 1L
  y[2, 1] <- 1L
  y[9, 9] <- 1L
  y[8, 9] <- 1L
  y[9, 8] <- 1L
  expect_equal(check_matrix(y), list(FALSE, c(1, 9), c(1, 9), list(list(c(1, 2, 3), c(1, 2, 3)), list(c(7, 8, 9), c(7, 8, 9)))))
})
