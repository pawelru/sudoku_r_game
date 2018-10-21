#' Action to assign value in selected cell
#'
#' @param mat sudoku class object
#' @param row row index of sudoku matrix
#' @param col column index of sudoku matrix
#' @param val value to be filled
#'
#' @return modified sudoku class object
#'
#' @importFrom assertthat assert_that is.count not_empty
assign_value <- function(mat, row, col, val) {
  assert_that(is.sudoku(mat))
  assert_that(is.count(row), row %in% 1:9)
  assert_that(is.count(col), col %in% 1:9)
  assert_that(not_empty(val), is.integer(val) || is.na(val), length(val) == 1, ifelse(is.integer(val) && !is.na(val), val %in% 1:9, T))

  mat[row, col] <- val
  return(mat)
}


#' Function to check if user wins
#'
#' @param mat sudoku class object
#'
#' @return boolean flag if win
#'
#' @importFrom assertthat assert_that
check_if_win <- function(mat) {
  assert_that(is.sudoku(mat))

  return(all(!is.na(mat)) && check_matrix(mat)[[1]])
}


#' Main function to start game
#'
#' Generate sudoku matrix and open an interactive game session.
#'
#' @param difficulty Game difficulty level. Three levels allowed (E)asy, (M)edium, (H)ard.
#'
#' @return None
#'
#' @examples
#' start_game('E")
#' start_game('M')
#' start_game('H')
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that is.string
#'
#' @export
start_game <- function(difficulty = 'E') {
  assert_that(is.string(difficulty), difficulty %in% c('E', 'M', 'H'))

  sudoku_matrix <- generate_matrix(difficulty)

  current_difficulty <- difficulty
  last_cmd <- character(0)
  overwritten_value <- integer(0)
  if_error <- FALSE

  repeat {
    cat("\014")

    print(sudoku_matrix)

    cat('\n\n')
    cat('Please assign value in form of [row], [column], [value] (e.g. "1, 2, 3" to write 3 in 1st row and 2nd column cell or "1, 2, ." to clear its value). \
        \nType "(rev)ert" to undo last change (only one) or "(res)tart" to start new game. \
        \nType "H" to change to (H)ard mode or "M" to change to (M)edium mode or "E" to go back to (E)asy mode.\n\n')

    if (if_error) {
      cat('\nUnrecognized command.\n')
    }

    cmd <- readline("Command: ")
    cmd <- gsub('\\s+', '', cmd)

    if_error <- FALSE

    if (tolower(cmd) == 'h') {

      last_cmd <- character(0)
      overwritten_value <- integer(0)
      if_error <- FALSE

      current_difficulty <- 'H'

      sudoku_matrix <- generate_matrix(current_difficulty)

      next

    } else if (tolower(cmd) == 'm') {

      last_cmd <- character(0)
      overwritten_value <- integer(0)
      if_error <- FALSE

      current_difficulty <- 'M'

      sudoku_matrix <- generate_matrix(current_difficulty)

      next

    } else if (tolower(cmd) == 'e') {

      last_cmd <- character(0)
      overwritten_value <- integer(0)
      if_error <- FALSE

      current_difficulty <- 'E'

      sudoku_matrix <- generate_matrix(current_difficulty)

      next

    } else if (tolower(cmd) %in% c('rev', 'revert')) {

      if (length(last_cmd) > 0) {
        cmd <- last_cmd %>%
          {strsplit(., ',')[[1]]} %>%
          trimws %>%
          head(-1) %>%
          c(., overwritten_value) %>%
          paste0(., collapse = ",")
      } else {
        next
      }

    } else if (tolower(cmd) %in% c('res', 'restart')) {

      last_cmd <- character(0)
      overwritten_value <- integer(0)
      if_error <- FALSE

      sudoku_matrix <- generate_matrix(current_difficulty)

      next

    }

    # try to evaluate -> activate error flag if not succeded
    tryCatch({

      last_cmd <- cmd
      cmd <- strsplit(cmd, ',')[[1]] %>% trimws %>% {suppressWarnings(as.integer(.))}
      stopifnot(length(cmd) == 3)
      overwritten_value <- sudoku_matrix[cmd[1], cmd[2]] %>% ifelse(is.na(.), NA_integer_, .)
      sudoku_matrix <- assign_value(sudoku_matrix, cmd[1], cmd[2], cmd[3])

    }, error = function(e) {

      if_error <<- TRUE
      eval(parse(text = 'next'), envir = parent.env(environment()))

    })

    # end game condition
    if (isTRUE(check_if_win(sudoku_matrix)[[1]])) {
      print('Congratulations! You won!')
      break
    }

  }

}
