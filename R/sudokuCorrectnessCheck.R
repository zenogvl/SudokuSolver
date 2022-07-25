
# A function to check if a solution of a sudoku is correct.
sudokuCorrectnessCheck <- function(sudoku){
  row <- apply(sudoku,1,stringCorrectnessCheck)
  col <- apply(sudoku,2,stringCorrectnessCheck)

  box <- matrix(NA, 9, 9)
  for(i in c(1,4,7)){ #Open single's boxes
    for(j in c(1,4,7)){
      box[i:(i+2),j:(j+2)] <- stringCorrectnessCheck(sudoku[i:(i+2),j:(j+2)])
    }
  }
  if(!(any(row) || any(col) || any(box))){
    return("sodoku solution correct")
  }
}


stringCorrectnessCheck <- function(x){
  if(length(unique(as.vector(x))) != 9){
    stop("sudoku solution not correct")
  } else {
    return(FALSE)
  }
}






