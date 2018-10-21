context("sudoku class")

test_that("check if sudoku class", {
  x <- generate_matrix('E')

  expect_is(x, 'sudoku')
  expect_is(x, 'matrix')
})


test_that("print cell", {
  x <- generate_matrix('E')

  x[1, 1] <- 1L
  x[1, 2] <- 2L
  x[2, 1] <- 3L

  check <- check_matrix(x)

  expect_output(print_matrix_cell(x, check, 1L, 1L), paste0(x[1, 1], ' '))
  expect_output(print_matrix_cell(x, check, 1L, 2L), paste0(x[1, 2], ' '))
  expect_output(print_matrix_cell(x, check, 2L, 1L), paste0(x[2, 1], ' '))

  expect_error(print_matrix_cell(1, check, 1L, 1L))
  expect_error(print_matrix_cell(NA, check, 1L, 1L))

  expect_error(print_matrix_cell(x, NULL, 1L, 1L))
  expect_error(print_matrix_cell(x, NA, 1L, 1L))
  expect_error(print_matrix_cell(x, 1, 1L, 1L))

  expect_error(print_matrix_cell(x, check, 1L, 10L))
  expect_error(print_matrix_cell(x, check, 1L, integer(0)))
  expect_error(print_matrix_cell(x, check, 1L, c(1L:2L)))
  expect_error(print_matrix_cell(x, check, 1L, NA_integer_))
  expect_error(print_matrix_cell(x, check, 1L, NULL))

  expect_error(print_matrix_cell(x, check, 10L, 1L))
  expect_error(print_matrix_cell(x, check, integer(0), 1L))
  expect_error(print_matrix_cell(x, check, c(1L:2L), 1L))
  expect_error(print_matrix_cell(x, check, NA_integer_, 1L))
  expect_error(print_matrix_cell(x, check, NULL, 1L))
})


test_that("print sudoku matrix", {
  x <- generate_raw_matrix()
  class(x) <- c(class(x), 'sudoku')

  expect_output(print(x))

  grep_pattern <- paste0(rep('\\d \\d \\d', 3), collapse = ' | ') %>%
    rep(., 3) %>%
    paste0(., collapse = '\\n') %>%
    rep(., 3) %>%
    paste0(., collapse = rep('- ', 12) %>% paste0(., collapse = ''))
  expect_output(print(x), grep_pattern)


  x <- generate_matrix('E')

  expect_output(print(x))

  x[1, 1] = 1L
  expect_output(print(x), '^1.*')

  x[1, 2] = NA
  expect_output(print(x), '^1 [.].*')

  x[1, 3] = 2L
  expect_output(print(x), '^1 [.] 2 | .*')
})
