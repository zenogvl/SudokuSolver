# This uses the possibleSpotsList created by a finder function to update the sudoku and possibleSpotsList by looking for singles.
sudokuSolutionUpdater <- function(possibleSpotsList, sudoku){

  #First get the index of the lone singles
  indexLoneSingles <- which(Reduce('+', possibleSpotsList) == 1, arr.ind = TRUE)

  if(nrow(indexLoneSingles) > 0){
    for(cell in 1:nrow(indexLoneSingles)){
      #Find the number of the lone single
      number <- (1:9)[sapply(possibleSpotsList, "[", indexLoneSingles[cell,1], indexLoneSingles[cell,2])]
      #Place this number in the sudoku
      sudoku[indexLoneSingles[cell,, drop = FALSE]] <- number


      possibleSpotsList[[number]][indexLoneSingles[cell,, drop = FALSE]] <- FALSE

      #Set the value of all the cells this number sees to FALSE
      possibleSpotsList[[number]][cellInfluenceShower(indexLoneSingles[cell,, drop = FALSE])] <- FALSE

      #Set this cell to false for all other numbers
      for(i in setdiff(1:9, number)){
        possibleSpotsList[[number]][indexLoneSingles[cell,, drop = FALSE]] <- FALSE
      }


    }
  }

  ### Hidden Singles

  #Get the index and number of all hidden singles.
  indexHiddenSingles <- vector(mode = "list", 9)
  for(numb in 1:9){
    indexHiddenSingles[[numb]] <- which(hiddenSingleFinderOld(possibleSpotsList[[numb]]), arr.ind = TRUE)
    if(nrow(indexHiddenSingles[[numb]]) > 0){
      indexHiddenSingles[[numb]] <- cbind(indexHiddenSingles[[numb]], numb)
    }
  }
  class(indexHiddenSingles) <- c("list", "index")
  indexHiddenSingles <- listToMatrix(indexHiddenSingles, nCol = 3)

  if(class(indexHiddenSingles)[1] == "integer"){
    indexHiddenSingles <- matrix(indexHiddenSingles, ncol = 3)
  }

  if(nrow(indexHiddenSingles) > 0){
    for(cell in 1:nrow(indexHiddenSingles)){
      number <- indexHiddenSingles[cell, 3]
      sudoku[indexHiddenSingles[cell,1:2, drop = FALSE]] <- number
      possibleSpotsList[[indexHiddenSingles[cell, 3]]][cellInfluenceShower(indexHiddenSingles[cell,1:2, drop = FALSE])] <- FALSE

      possibleSpotsList[[number]][indexHiddenSingles[cell,1:2, drop = FALSE]] <- FALSE

      #Set this cell to false for all other numbers
      for(i in setdiff(1:9, number)){
        possibleSpotsList[[i]][indexHiddenSingles[cell,1:2, drop = FALSE]] <- FALSE
      }

    }
  }
  return(list(possibleSpotsList = possibleSpotsList, sudoku = sudoku))
}



