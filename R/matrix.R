#' Converts big index to range of small indexes
#'
#' @param n big index (integer)
#'
#' @return range of small indexes
#'
#' @importFrom assertthat assert_that is.count
get_small_index <- function(n) {
  assert_that(is.count(n), n %in% 1L:3L)

  return(((n - 1L) * 3L + 1L):(n * 3L))
}


#' Move elements from tail to the head of vector
#'
#' @param x vecor to be shifted
#' @param n shift value
#'
#' @return shifted vector
#'
#' @importFrom utils tail head
#' @importFrom assertthat assert_that
shift_index <- function(x, n) {
  assert_that(is.vector(x), length(x) > 1)
  assert_that(is.integer(n), length(n) == 1L, n < length(x))

  res <- if (n == 0L) {
    return(x)
  } else {
    return(c(tail(x, n), head(x, -n)))
  }
}


#' Internal shuffle function
#'
#' @param mat matrix or sudoku class object
#' @param if_row boolean flag indicating if suffle rows or columns
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that is.flag
shuffle <- function(mat, if_row = TRUE) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))
  assert_that(is.flag(if_row))

  new_idx <- sample(1L:3L) %>%
    lapply(., get_small_index) %>%
    lapply(., sample) %>%
    unlist()

  if (if_row) {
    return(mat[new_idx, ])
  } else {
    return(mat[, new_idx])
  }
}


#' Change columns order randomly
#'
#' @param mat matrix or sudoku class object
#'
#' @return modified matrix
#'
#' @importFrom assertthat assert_that
shuffle_columns <- function(mat) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))

  shuffle(mat, FALSE)
}


#' Change rows order randomly
#'
#' @param mat matrix or sudoku class object
#'
#' @return modified matrix
#'
#' @importFrom assertthat assert_that
shuffle_rows <- function(mat) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))

  shuffle(mat, TRUE)
}


#' Changes two values by each other
#'
#' @param mat matrix or sudoku class object
#' @param val1 value_1 to be changed by value_2
#' @param val2 value_2 to be changed by value_1
#'
#' @return modified matrix
#'
#' @importFrom assertthat assert_that is.count
replace <- function(mat, val1, val2) {
  assert_that(is.matrix(mat))
  assert_that(is.count(val1), val1 %in% c(1L:9L))
  assert_that(is.count(val2), val2 %in% c(1L:9L))

  res <- mat

  res[mat == val1] <- val2
  res[mat == val2] <- val1

  return(res)
}


#' Flip matrix horizontally or vertically
#'
#' @param mat matrix or sudoku class object
#' @param if_row boolean flag indicating if horizontal or vertical flip
#'
#' @return modified matrix
#'
#' @importFrom assertthat assert_that is.flag
flip <- function(mat, if_row = TRUE) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))
  assert_that(is.flag(if_row))

  if (if_row) {
    mat[9L:1L, ]
  } else {
    mat[, 9L:1L]
  }
}


#' Rotate matrix by 90 degrees
#'
#' @param mat matrix or sudoku class object
#'
#' @return modified matrix
#'
#' @importFrom assertthat assert_that
rotate <- function(mat) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))

  return(t(mat[nrow(mat):1L, ]))
}

#' Modify matrix by various approaches
#'
#' @param mat sudoku matrix
#' @param n how many modification steps will be performed
#'
#' @return modified matrix
#'
#' @importFrom stats runif
#' @importFrom assertthat assert_that is.count
randomize_matrix <- function(mat, n = as.integer(runif(1, 50, 100))) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))
  assert_that(is.count(n))

  res <- mat

  for (i in n) {

    res <- shuffle_rows(res)
    res <- shuffle_columns(res)

    val1 <- sample(c(1L:9L), 1L)
    val2 <- sample(setdiff(c(1L:9L), val1), 1L)
    res <- replace(res, val1, val2)

    if_flip <- sample(c(T, F), 1L)
    if (if_flip) {
      if_row <- sample(c(T, F), 1L)
      res <- flip(res, if_row)
    }

    if_rotate <- sample(c(T, F), 1L)
    if (if_rotate) {
      res <- rotate(res)
    }

    if_transpose <- sample(c(T, F), 1)
    if (if_transpose) {
      res <- t(res)
    }

  }

  return(res)
}


#' Generate correct sudoku matrix to be modified
#'
#' @return correct sudoku matrix
generate_raw_matrix <- function() {
  numbers <- sample(1L:9L)

  res <- matrix(c(numbers, shift_index(numbers, 3L), shift_index(numbers, 6L),
                  shift_index(numbers, 1L), shift_index(numbers, 4L), shift_index(numbers, 7L),
                  shift_index(numbers, 2L), shift_index(numbers, 5L), shift_index(numbers, 8L)),
                9, 9)

  return(res)
}


#' Randomly fill empty values in matrix depending on difficulty
#'
#' @param mat matrix or sudoku class object
#' @param difficulty game difficulty level. Three levels allowed (E)asy, (M)edium, (H)ard
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that is.string
fill_NA <- function(mat, difficulty) {
  assert_that(is.matrix(mat), all(dim(mat) == c(9, 9)))
  assert_that(is.string(difficulty), difficulty %in% c('E', 'M', 'H'))

  empty_share <- switch(difficulty,
                        'E' = 0.1,
                        'M' = 0.4,
                        'H' = 0.5)

  empty_mask <- sample(c(T, F), 81L, replace = T, prob = c(empty_share, 1 - empty_share)) %>%
    matrix(., 9L, 9L)
  mat[empty_mask] <- NA_integer_

  return(mat)
}


#' Main function to generate final sudoku matrix
#'
#' @param difficulty game difficulty level. Three levels allowed (E)asy, (M)edium, (H)ard
#'
#' @importFrom assertthat assert_that is.string
generate_matrix <- function(difficulty) {
  assert_that(is.string(difficulty), difficulty %in% c('E', 'M', 'H'))

  res <- generate_raw_matrix()

  res <- randomize_matrix(res)

  res <- fill_NA(res, difficulty)

  class(res) <- c(class(res), 'sudoku')

  return(res)
}
