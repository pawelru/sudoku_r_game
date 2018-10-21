#' Print matrix cell with appropriate colour
#'
#' @param mat matrix of sudoku class object
#' @param check list with checks performed on matrix
#' @param row row index
#' @param col column index
#'
#' @importFrom magrittr %>%
#' @importFrom crayon red
#' @importFrom assertthat assert_that is.count
print_matrix_cell <- function(mat, check, row, col) {
  assert_that(is.sudoku(mat))
  assert_that(is.list(check), length(check) == 4)
  assert_that(is.count(row), row %in% 1L:9L)
  assert_that(is.count(col), col %in% 1L:9L)

  value <- mat[row, col] %>% ifelse(is.na(.), '.', .)
  if (check[[1L]]) {
    cat(value, '')
  } else {
    if (
      (!is.null(check[[2L]]) && row %in% check[[2L]]) ||
      (!is.null(check[[3L]]) && col %in% check[[2L]]) ||
      (length(check[[4L]]) > 0 && lapply(check[[4L]], function(x) {row %in% x[[1L]] && col %in% x[[2L]]}) %>% unlist %>% any)
    ) {
      cat(red(value), '')
    } else {
      cat(value, '')
    }
  }
}


#' Method for printing sudoku object in predefinied format and coulors
#'
#' @method print sudoku
#'
#' @param mat matrix or sudoku class object
#' @param ... ignored
#'
#' @return printed sudoku matrix in cat()
#'
#' @importFrom assertthat assert_that
#'
#' @export
print.sudoku <- function(mat, ...) {
  assert_that(is.sudoku(mat))

  check <- check_matrix(mat)

  for (row in 1L:9L) {
    for (col in 1L:9L) {
      print_matrix_cell(mat, check, row, col)

      if (col == 3L || col == 6L) {
        cat(' | ')
      }
    }

    cat('\n')

    if (row == 3L || row == 6L) {
      cat(paste0(rep('-', 12L)))
      cat('\n')
    }
  }
}


#' Sudoku class utils
#' Function to check if an object is sudoku class.
#'
#' @param x an R object
#'
#' @return boolean flag
#'
#' @export
is.sudoku <- function(x) {
  inherits(x, "sudoku")
}
