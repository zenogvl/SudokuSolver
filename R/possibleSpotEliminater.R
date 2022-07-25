


#possibleSpotEliminater creates a list of matrixes for every single number. In these matrices a value is TRUE when a number can take that spot. The value is FALSE when the number cannot be in that spot because it's seen by the number in the row, column or box or it's occupied.
possibleSpotEliminater <- function(possibleSpotsList, sudoku){


  for(number in 1:9){
    possibleSpotsList[[number]][apply(sudoku,1,rowcolCheck,numb = number),] <- FALSE #check if number is in row
    possibleSpotsList[[number]][,apply(sudoku,2,rowcolCheck,numb = number)] <- FALSE #check if number is in col
    for(i in c(1,4,7)){ #check if number is in box
      for(j in c(1,4,7)){
        if(number %in% sudoku[i:(i+2),j:(j+2)]){ #variable for number used. CHECK IF MULTIPLE NUMBERS ARE ANALYSED
          possibleSpotsList[[number]][i:(i+2),j:(j+2)] <- FALSE #TEMPERAL TRUE/FALSE MATRIX USED.
        }
      }
    }
    possibleSpotsList[[number]][sudoku != number] <- FALSE #check is space is occupied by other number

  }
  return(possibleSpotsList)
}

#Helper function for the possible spot eliminater.
rowcolCheck <- function(x,numb){
  numb %in% x
}



