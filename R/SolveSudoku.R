
sudoku <- empty.sudoku.1

SolveSudoku <- function(sudoku) {
  if(inherits(sudoku, what = "data.frame")){
    sudoku <- as.matrix(sudoku)
    colnames(sudoku) <- NULL
  }
  if(!is.matrix(sudoku)){
    stop("The input must be a data frame or a matrix")
  }
  if( !(ncol(sudoku) == 9) | !(nrow(sudoku) == 9) ){
    stop("The sudoku must have 9 rows and 9 collums")
  }
  if(any(sudoku == 0, na.rm = TRUE)){
    sudoku[sudoku == 0] <- NA
  }
  if(any(!(sudoku[!is.na(sudoku)] %in% 1:9))){
    stop("the non empty numbers can only contains integers from 1 to 9, empty places should be a zero or a NA")
  }






}
