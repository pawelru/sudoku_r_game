context("matrix generator")

test_that("correct transform from big to small index", {
  expect_equal(get_small_index(1L), c(1:3))
  expect_equal(get_small_index(2L), c(4:6))
  expect_equal(get_small_index(3L), c(7:9))
})

test_that("correct vector shift", {
  expect_equal(shift_index(c(1:9), 0L), c(1:9))
  expect_equal(shift_index(c(1:9), 1L), c(9, 1:8))
  expect_equal(shift_index(c(1:9), 2L), c(8:9, 1:7))
  expect_equal(shift_index(c(1:9), 8L), c(2:9, 1))

  # wrong second argument
  expect_error(shift_index(c(1:10)))
  expect_error(shift_index(c(1:9), 9L))
  expect_error(shift_index(c(1:9), c(1:3)))
  expect_error(shift_index(c(1:10), NA_integer_))
  expect_error(shift_index(c(1:10), integer(0)))
  expect_error(shift_index(c(1:10), 'a'))

  # wrong first argument
  expect_error(shift_index(1, 1L))
  expect_error(shift_index(NA_integer_, 1L))
  expect_error(shift_index(integer(0), 1L))
  expect_error(shift_index('a', 1))
})

test_that("correct shuffle functions - shuffle()", {
  x <- matrix(1:81, 9, 9)

  # tests on return
  # there is small chance that this test fail -> two sample function return the same vector
  expect_false(isTRUE(identical(shuffle(x), x)))
  expect_true(is.matrix(shuffle(x)))
  expect_equal(dim(shuffle(x)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(shuffle(x)), sum(x))
  expect_equal(mean(shuffle(x)), mean(x))
  expect_equal(sd(shuffle(x)), sd(x))

  # tests on incorrect inputs
  expect_error(shuffle(1, TRUE))
  expect_error(shuffle(NA, TRUE))
  expect_error(shuffle(x[1:2, 1:9], TRUE))
  expect_error(shuffle(x, NA))
  expect_error(shuffle(x, logical(0)))
  expect_error(shuffle(x, 1))

})


test_that("correct shuffle functions - shuffle_columns()", {
  x <- matrix(1:81, 9, 9)

  expect_false(isTRUE(identical(shuffle_columns(x), x)))
  expect_true(is.matrix(shuffle_columns(x)))
  expect_equal(dim(shuffle_columns(x)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(shuffle_columns(x)), sum(x))
  expect_equal(mean(shuffle_columns(x)), mean(x))
  expect_equal(sd(shuffle_columns(x)), sd(x))

  # tests on incorrect inputs
  expect_error(shuffle_columns(1))
  expect_error(shuffle_columns(NA))
})


test_that("correct shuffle functions - shuffle_rows()", {
  x <- matrix(1:81, 9, 9)

  expect_false(isTRUE(identical(shuffle_rows(x), x)))
  expect_true(is.matrix(shuffle_rows(x)))
  expect_equal(dim(shuffle_rows(x)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(shuffle_rows(x)), sum(x))
  expect_equal(mean(shuffle_rows(x)), mean(x))
  expect_equal(sd(shuffle_rows(x)), sd(x))

  # tests on incorrect inputs
  expect_error(shuffle_rows(1))
  expect_error(shuffle_rows(NA))
})


test_that("correct shuffle functions - replace()", {
  x <- matrix(1:81, 9, 9)

  expect_true(isTRUE(identical(replace(x, 1L, 1L), x)))
  expect_false(isTRUE(identical(replace(x, 1L, 2L), x)))
  expect_true(is.matrix(replace(x, 1L, 2L)))
  expect_equal(dim(replace(x, 1L, 2L)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(replace(x, 1L, 2L)), sum(x))
  expect_equal(mean(replace(x, 1L, 2L)), mean(x))
  expect_equal(sd(replace(x, 1L, 2L)), sd(x))

  # tests on incorrect inputs
  expect_error(replace(1))
  expect_error(replace(NA))
  expect_error(replace(1, 2L, 3L))
  expect_error(replace(NA, 1, 2))
  expect_error(replace(x, 1L, 10L))
  expect_error(replace(x, 10L, 1L))
  expect_error(replace(x, c(1L, 2L), 1L))
  expect_error(replace(x, 1L, c(1L, 2L)))
  expect_error(replace(x, integer(0), 1L))
  expect_error(replace(x, 1L, integer(0)))
  expect_error(replace(x, NA_integer_, 1L))
  expect_error(replace(x, 1L, NA_integer_))
  expect_error(replace(x, 1L, 'a'))
  expect_error(replace(x, 'a', 1L))
})


test_that("correct shuffle functions - flip()", {
  x <- matrix(1:81, 9, 9)

  expect_false(isTRUE(all(flip(x) == x)))
  expect_true(isTRUE(all(flip(flip(x)) == x)))
  expect_true(is.matrix(flip(x)))
  expect_equal(dim(flip(x)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(flip(x)), sum(x))
  expect_equal(mean(flip(x)), mean(x))
  expect_equal(sd(flip(x)), sd(x))

  # tests on incorrect inputs
  expect_error(flip(1))
  expect_error(flip(NA))
})


test_that("correct shuffle functions - rotate()", {
  x <- matrix(1:81, 9, 9)

  expect_false(isTRUE(all(rotate(x) == x)))
  expect_true(isTRUE(all(rotate(rotate(rotate(rotate(x)))) == x)))
  expect_true(is.matrix(rotate(x)))
  expect_equal(dim(rotate(x)), dim(x))
  # naive test if duplicated or dropped
  expect_equal(sum(rotate(x)), sum(x))
  expect_equal(mean(rotate(x)), mean(x))
  expect_equal(sd(rotate(x)), sd(x))

  # tests on incorrect inputs
  expect_error(rotate(1))
  expect_error(rotate(NA))
})


test_that("correct randomize_matrix() function", {
  x <- matrix(1:81, 9, 9)

  expect_false(isTRUE(all(randomize_matrix(x) == x)))
  expect_true(is.matrix(randomize_matrix(x)))
  expect_equal(dim(randomize_matrix(x)), dim(randomize_matrix(x)))
  # naive test if duplicated or dropped
  expect_equal(sum(randomize_matrix(x)), sum(x))
  expect_equal(mean(randomize_matrix(x)), mean(x))
  expect_equal(sd(randomize_matrix(x)), sd(x))

  # tests on incorrect inputs
  expect_error(randomize_matrix(1))
  expect_error(randomize_matrix(NA))

})


test_that("correct generate_raw_matrix() function", {

  expect_true(is.matrix(generate_raw_matrix()))
  expect_equal(dim(generate_raw_matrix()), c(9, 9))
  expect_equal(sum(generate_raw_matrix()), sum(rep(c(1:9), 9)))
  expect_equal(mean(generate_raw_matrix()), mean(rep(c(1:9), 9)))
  expect_equal(sd(generate_raw_matrix()), sd(rep(c(1:9), 9)))

})


test_that("Fill empty cells", {
  x <- matrix(1:81, 9, 9)

  for (i in c('E', 'M', 'H')) {
    expect_true(is.matrix(fill_NA(x, i)))
    expect_equal(dim(fill_NA(x, i)), c(9, 9))
    expect_gt(sum(is.na(fill_NA(x, i))), 0)
  }

  # tests on incorrect inputs
  expect_error(fill_NA(x))
  expect_error(fill_NA(x, character(0)))
  expect_error(fill_NA(x, NA_character_))
  expect_error(fill_NA(x, 'A'))
  expect_error(fill_NA(x, 1))
  expect_error(fill_NA(1, 'E'))
  expect_error(fill_NA(NA, 'E'))

})


test_that("Main generator function", {
  x <- matrix(1:81, 9, 9)

  for (i in c('E', 'M', 'H')) {
    expect_true(is.matrix(generate_matrix(i)))
    expect_equal(dim(generate_matrix(i)), c(9, 9))
    expect_gt(sum(is.na(fill_NA(x, i))), 0)
    expect_true('sudoku' %in% class(generate_matrix(i)))
    expect_true(check_matrix(generate_matrix(i))[[1]])
  }

  # tests on incorrect inputs
  expect_error(generate_matrix())
  expect_error(generate_matrix(character(0)))
  expect_error(generate_matrix(NA_character_))
  expect_error(generate_matrix('A'))
  expect_error(generate_matrix(1))


})

