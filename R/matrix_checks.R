#' Check condition of values being unique
#'
#' @param values vector of values to be checked
#'
#' @return boolan flag
#'
#' @importFrom assertthat assert_that
check_unique <- function(values) {
  assert_that(is.integer(values))

  if (length(values) == 0) {
    return(TRUE)
  } else {
    return(isTRUE(length(values) == length(unique(values))))
  }

}


#' Check condition of values being within 1-9 range
#'
#' @param values vector of values to be checked
#'
#' @return boolan flag
#'
#' @importFrom assertthat assert_that
check_range <- function(values) {
  assert_that(is.integer(values))

  if (length(values) == 0) {
    return(TRUE)
  } else {
    return(isTRUE(all(values %in% c(1L:9L))))
  }

}


#' Check each row agains predefined criteria
#'
#' @param mat matrix or sudoku class object
#'
#' @return vector with erroneus rows
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
check_rows <- function(mat) {
  assert_that(is.sudoku(mat))

  res <- c()

  for (i in 1L:9L) {
    values <- mat[i, ] %>% .[!is.na(.)]

    correct_unique <- check_unique(values)
    correct_range <- check_range(values)

    if (!correct_unique || !correct_range) {
      res <- c(res, i)
    }
  }

  return(res)
}


#' Check each column agains predefined criteria
#'
#' @param mat matrix or sudoku class object
#'
#' @return vector with erroneus columns
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
check_cols <- function(mat) {
  assert_that(is.sudoku(mat))

  res <- c()

  for (i in 1L:9L) {
    values <- mat[, i] %>% .[!is.na(.)]

    correct_unique <- check_unique(values)
    correct_range <- check_range(values)

    if (!correct_unique || !correct_range) {
      res <- c(res, i)
    }
  }

  return(res)
}


#' Check each mini-square agains predefined criteria
#'
#' @param mat matrix or sudoku class object
#'
#' @return list with erroneus squares in form of big indexes
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
check_squares <- function(mat) {
  assert_that(is.sudoku(mat), all(dim(mat) == c(9, 9)))

  res <- list()

  for (big_row in 1L:3L) {
    for (big_col in 1L:3L) {
      small_row <- get_small_index(big_row)
      small_col <- get_small_index(big_col)

      values <- mat[small_row, small_col] %>% .[!is.na(.)]

      correct_unique <- check_unique(values)
      correct_range <- check_range(values)

      if (!correct_unique || !correct_range) {
        res <- c(res, list(list(small_row, small_col)))
      }
    }
  }

  return(res)
}


#' Main function to check if sudoku matrix is correct
#'
#' @param mat matrix or sudoku class object
#'
#' @return list where first element is global flag indicating if matrix is correct,
#' second element with erroneus rows,
#' third element with erroneus columns,
#' fourth element with erroneus squares
#'
#' @importFrom assertthat assert_that
check_matrix <- function(mat) {
  assert_that(is.sudoku(mat))

  res <- list(
    check_rows(mat),
    check_cols(mat),
    check_squares(mat)
  )

  res <- c(
    is.null(res[[1L]]) && is.null(res[[2L]]) && length(res[[3L]]) == 0,
    res
  )

  return(res)
}
