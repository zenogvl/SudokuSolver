#Functions that check if the sudoku is in the correct format and change some things about them.
inputControl <- function(sudoku){
  UseMethod("inputControl")
}

inputControl.default <- function(sudoku) {
  stop("The sudoku must be a matrix or dataframe")
}

inputControl.matrix <- function(sudoku){
  if( !(ncol(sudoku) == 9) | !(nrow(sudoku) == 9) ){
    stop("The sudoku must have 9 rows and 9 collums")
  }
  if(any(sudoku == 0, na.rm = TRUE)){
    sudoku[sudoku == 0] <- NA
  }
  if(any(!(sudoku[!is.na(sudoku)] %in% 1:9))){
    stop("The non empty numbers can only contains integers from 1 to 9, empty places should be a 0 or a NA")
  }
  class(sudoku) <- "sudoku"

  return(sudoku)
}

inputControl.data.frame <- function(sudoku){
  sudoku <- as.matrix(sudoku)
  colnames(sudoku) <- NULL
  if( !(ncol(sudoku) == 9) | !(nrow(sudoku) == 9) ){
    stop("The sudoku must have 9 rows and 9 collums")
  }
  if(any(sudoku == 0, na.rm = TRUE)){
    sudoku[sudoku == 0] <- NA
  }
  if(any(!(sudoku[!is.na(sudoku)] %in% 1:9))){
    stop("the non empty numbers can only contains integers from 1 to 9, empty places should be a 0 or a NA")
  }
  class(sudoku) <- "sudoku"
  return(sudoku)
}
inputControl.sudoku <- function(sudoku){
  if( !(ncol(sudoku) == 9) | !(nrow(sudoku) == 9) ){
    stop("You changed something to the sudoku. The sudoku must have 9 rows and 9 collums")
  }
  if(any(sudoku == 0, na.rm = TRUE)){
    sudoku[sudoku == 0] <- NA
  }
  if(any(!(sudoku[!is.na(sudoku)] %in% 1:9))){
    stop("You changed something to the sudoku. The non empty numbers can only contains integers from 1 to 9, empty places should be a 0 or a NA")
  }
  class(sudoku) <- "sudoku"
  return(sudoku)
}





