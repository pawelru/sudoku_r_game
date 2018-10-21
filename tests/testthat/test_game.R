context("game functionalities")

test_that("assign value", {
  x <- generate_matrix('E')

  expect_equal(assign_value(x, 1L, 2L, 3L)[1, 2], 3)
  expect_is(assign_value(x, 1L, 2L, 3L), 'sudoku')

  expect_equal(assign_value(x, 1L, 2L, NA_integer_)[1, 2], NA_integer_)

  expect_error(assign_value(1, 1L, 2L, 3L))
  expect_error(assign_value(NA, 1L, 2L, 3L))
  expect_error(assign_value(NULL, 1L, 2L, 3L))

  expect_error(assign_value(x, 10L, 2L, 3L))
  expect_error(assign_value(x, NA, 2L, 3L))
  expect_error(assign_value(x, NULL, 2L, 3L))
  expect_error(assign_value(x, 'a', 2L, 3L))

  expect_error(assign_value(x, 1L, 10L, 3L))
  expect_error(assign_value(x, 1L, NA, 3L))
  expect_error(assign_value(x, 1L, NULL, 3L))
  expect_error(assign_value(x, 1L, 'a', 3L))

  expect_error(assign_value(x, 1L, 2L, 10L))
  expect_error(assign_value(x, 1L, 2L, integer(0)))
  expect_error(assign_value(x, 1L, 2L, NULL))
  expect_error(assign_value(x, 1L, 2L, 'a'))

})


test_that("global checking if win", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_true(check_if_win(x))

  x <- generate_matrix('H')
  expect_false(check_if_win(x))
})
