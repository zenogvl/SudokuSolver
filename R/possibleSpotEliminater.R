#' PossbileSpotEliminate
#' This function checks when a number is in a row, column of box.
#'
#' @param possibleSpotsList This is an list with 9 matrixes, each for every digit.
#' @param sudoku The sudoku to be analyzed
#'
#' @return An list with 9 matrixes containg TRUE of FALSE values. When a position is TRUE, that number can't be in that position.
#' @export

possibleSpotEliminater <- function(possibleSpotsList, sudoku){
  rowcolCheck <- function(x,numb){
    numb %in% x
  }
  for(number in 1:9){
    possibleSpotsList[[number]][apply(sudoku,1,rowcolCheck,numb = number),] <- TRUE #check if number is in row
    possibleSpotsList[[number]][,apply(sudoku,2,rowcolCheck,numb = number)] <- TRUE #check if number is in col
    for(i in c(1,4,7)){ #check if number is in box
      for(j in c(1,4,7)){
        if(number %in% sudoku[i:(i+2),j:(j+2)]){ #variable for number used. CHECK IF MULTIPLE NUMBERS ARE ANALYSED
          possibleSpotsList[[number]][i:(i+2),j:(j+2)] <- TRUE #TEMPERAL TRUE/FALSE MATRIX USED.
        }
      }
    }
    possibleSpotsList[[number]][sudoku != number] <- TRUE #check is space is occupied by other number

  }
  return(possibleSpotsList)
}



