
#Function to look for open singles.
#Open singles are situations where only one spot is a row, column or box is empty.

#This finder function wraps around the onpenSingleSelecter function
openSingleFinder <- function(sudoku){
  sudoku <- t(apply(sudoku, 1, openSingleSelecter)) #open single rows
  sudoku <- apply(sudoku, 2, openSingleSelecter) #open single columns
  for(i in c(1,4,7)){ #Open single's boxes
    for(j in c(1,4,7)){
      sudoku[i:(i+2),j:(j+2)] <- openSingleSelecter(sudoku[i:(i+2),j:(j+2)])
    }
  }
  class(sudoku) <- "sudoku"
  return(sudoku)
}

openSingleSelecter <- function(x){ #x is a row, column or box of the sudoku
  if(sum(is.na(x)) == 1){ #Checks if only one spot is empty
    x[is.na(x)] <- setdiff(1:9, x) #See what number is missing.
  }
  return(x)
}

# A function to look for hidden singles that also produces hints.
# Hidden singles is the situation where a number can only go in one spot in a row, column or box.
hiddenSingleFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()
  for(numb in 1:9){
    for(row in 1:9){
      if(sum(possibleSpotsList[[numb]][row, ]) == 1){
        index <- cbind(row = row, col = (1:9)[possibleSpotsList[[numb]][row, ]])
        if(returnHint){
          hint <- list(number = numb,
                       direction = "row",
                       which = row,
                       index = createIndexClass(index))
          class(hint) <- "hiddenSingle"
          hintList <- append(hintList, list(hint))
        } else{
          #Add number to the sudoku
          sudoku[index] <- numb

          #Change the possible spots list
          possibleSpotsList[[numb]][cellInfluenceShower(index)] <- FALSE
          for(i in 1:9){
            possibleSpotsList[[i]][index] <- FALSE
          }
        }

      }
    }

    for(col in 1:9){
      if(sum(possibleSpotsList[[numb]][,col]) == 1){
        index <- cbind(row = (1:9)[possibleSpotsList[[numb]][, col]], col = col)
        if(returnHint){
          hint <- list(number = numb,
                       direction = "column",
                       which = col,
                       index = createIndexClass(index))
          class(hint) <- "hiddenSingle"
          if(!any(sapply(lapply(lapply(hintList, "[[", "index"), as.numeric) , identical, y = as.numeric(hint$index)))){
            hintList <- append(hintList, list(hint))
          }
        } else {
          #Add number to the sudoku
          sudoku[index] <- numb

          #Change the possible spots list
          possibleSpotsList[[numb]][cellInfluenceShower(index)] <- FALSE
          for(i in 1:9){
            possibleSpotsList[[i]][index] <- FALSE
          }
        }
      }
    }
    startIndexBoxes <- cbind(row = rep(c(1,4,7), each = 3), col = rep(c(1,4,7), 3) ) - 1
    for(box in 1:9){
      indexBox <- cbind(row = rep(1:3, each = 3), col = rep(1:3,3)) + rep(startIndexBoxes[box,], each = 9)

      if(sum(possibleSpotsList[[numb]][indexBox]) == 1){
        index <- indexBox[possibleSpotsList[[numb]][indexBox], , drop = FALSE]
        if(returnHint){
          hint <- list(number = numb,
                       direction = "box",
                       which = box,
                       index = createIndexClass(index))
          class(hint) <- "hiddenSingle"
          if(!any(sapply(lapply(lapply(hintList, "[[", "index"), as.numeric) , identical, y = as.numeric(hint$index)))){
            hintList <- append(hintList, list(hint))
          }
        } else {
          #print(index)
          sudoku[index] <- numb

          possibleSpotsList[[numb]][cellInfluenceShower(index)] <- FALSE
          for(i in 1:9){
            possibleSpotsList[[i]][index] <- FALSE
          }
        }
      }
    }
  }
  if(returnHint){
    return(hintList)
  } else {
    return(list(possibleSpotsList = possibleSpotsList, sudoku = sudoku))
  }
}

#A function that findes lone singles and also produces hints.
# A lone single is the situation where only one number can go in a spot.
loneSinglesFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()

  #Get a matrix where lone singles are TRUE and all the other spots are NA.
  loneSingles <- Reduce('+', possibleSpotsList) == 1
  loneSingles[!loneSingles] <- NA

  for(number in 1:9){

    #Check for every number if there is a lone single
    if(!is.na(any(possibleSpotsList[[number]] == loneSingles))){
      if(any(possibleSpotsList[[number]] == loneSingles)){
        #Get the index of that lone single
        index <- which(possibleSpotsList[[number]] == loneSingles, arr.ind = TRUE)
        for(i in nrow(index)){
          if(returnHint){
            hint <- list(number = number,
                         index = createIndexClass(index))
            class(hint) <- "loneSingle"
            hintList <- append(hintList, list(hint))
          } else {
            sudoku[index] <- number
            possibleSpotsList[[number]][cellInfluenceShower(index)] <- FALSE
            for(i in 1:9){
              #Change the possible spots
              possibleSpotsList[[i]][index] <- FALSE
            }
          }
        }
      }
    }
  }
  if(returnHint){
    return(hintList)
  } else {
    return(list(possibleSpotsList = possibleSpotsList, sudoku = sudoku))
  }
}




#Function to look for hidden singles, this is a old function but still in use for one function.
#Hidden singles are when a number can go in only position.
hiddenSingleFinderOld <- function(possibleSpots) {
  hiddenSingles <- matrix(FALSE, 9, 9) #Create an output matrix that only is TRUE when there is a hidden single
  #Check for hidden singles in every row and column

  for(i in 1:9){
    if(sum(possibleSpots[,i]) == 1){
      hiddenSingles[,i] <- possibleSpots[,i]
    }
    if(sum(possibleSpots[i,]) == 1){
      hiddenSingles[i,] <- possibleSpots[i,]
    }
  }
  #Check for hidden singles in the boxes
  for(i in c(1,4,7)){
    for(j in c(1,4,7)){
      if(sum(possibleSpots[i:(i+2), j:(j+2)]) == 1){
        hiddenSingles[i:(i+2), j:(j+2)] <- possibleSpots[i:(i+2), j:(j+2)]
      }
    }
  }
  return(hiddenSingles)
}

#This adds the three single finder functions together to makes the first steps at the start of the solveSudoku function
singleFinder <- function(sudoku, possibleSpotsList){

  progressCounterBefore <- sum(!is.na(sudoku))
  progressCounterAfter <- 0

  while(progressCounterBefore != progressCounterAfter){
    progressCounterBefore <- sum(!is.na(sudoku))

    #Find and fill in open singles for row, column and boxes
    sudoku <- t(apply(sudoku, 1, openSingleSelecter)) #open single rows
    sudoku <- apply(sudoku, 2, openSingleSelecter) #open single columns
    class(sudoku) <- "sudoku"
    for(i in c(1,4,7)){ #Open single's boxes
      for(j in c(1,4,7)){
        sudoku[i:(i+2),j:(j+2)] <- openSingleSelecter(sudoku[i:(i+2),j:(j+2)])
      }
    }

    possibleSpotsList <- possibleSpotEliminater(possibleSpotsList = possibleSpotsList, sudoku = sudoku)

    #Find and fill in lone singles
    #Lone singles are situation where a spot has only one posability.
    loneSingles <- (Reduce('+', possibleSpotsList) == 1) #This gives TRUE when there is only one possibility in that spot
    loneSingles[!loneSingles] <- NA #Change non lone singles to NA
    for(number in 1:9){ #Find corresponding number and fill in.
      sudoku[possibleSpotsList[[number]] == loneSingles] <- number
    }

    possibleSpotsList <- possibleSpotEliminater(possibleSpotsList = possibleSpotsList, sudoku = sudoku)

    #Get a list of matrix where only the hidden singles are TRUE
    hiddenSinglesList <- lapply(possibleSpotsList,hiddenSingleFinderOld)
    #Use this list to fill in the sudoku
    for(number in 1:9){
      sudoku[hiddenSinglesList[[number]]] <- number
    }
    progressCounterAfter <- sum(!is.na(sudoku))
    progressCounterAfter
  }
  return(sudoku)
}






