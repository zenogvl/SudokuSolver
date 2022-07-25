
# The hiddenPairsFinder function searches for hidden pairs, triplets, quadruples and even bigger pairs. For simplicity they are all called pairs from now on.
# When there is a pair, this changes the values in the possibleSpotsList of the other numbers and retruns an updated version of this list.
hiddenPairsFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  possiblePairsList <- possibleSpotsList
  possiblePairsList$sudoku <- sudoku
  hintList <- list()

  #Columns
  for(column in 1:9){
    #print(column)
    #Create a list possible spots and the sudoku of a single column
    possiblePairs <- lapply(possiblePairsList, "[", ,column )

    #Go to the next colomn if the column is allready completed.
    if(!any(is.na(possiblePairs$sudoku))){
      next
    }

    #Get the numbers that are not filled in for that column
    possibleNumbers <- setdiff(1:9, na.omit(possiblePairs$sudoku))


    if(length(possibleNumbers) < 2){
      next
    }

    #Select the spots matrix of the non filled in numbers and make it a matrix
    pairsCompare <- possiblePairs[possibleNumbers]
    class(pairsCompare) <- c("list","possibleSpotsVector")
    pairsCompare <- listToMatrix(pairsCompare, rowNumbering = possibleNumbers)

    #Use the search function to look for a pair.
    numbersOfPair <- pairOfNSearcher(pairsCompare = pairsCompare, possibleNumbers)
    if(length(numbersOfPair) > 4){
      next
    }
    if(length(numbersOfPair) == sum(is.na(sudoku[,column]))){
      next
    }

    if(!is.na(numbersOfPair[1])) {
      #If there is a pair get the index of the pair numbers
      pairIndex <- (1:9)[Reduce("+", possiblePairs[numbersOfPair]) > 0]


      if(returnHint){
        pairIndex <- cbind(row = pairIndex, col = column)
        hint <- list(nPair = length(numbersOfPair),
                     numbers = numbersOfPair,
                     direction = "column",
                     which = column,
                     index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass))
        class(hint) <- "hiddenPair"
        hintList <- append(hintList, list(hint))
      }


      #Remove possibility of the numbers not in the pair in the possibleSpotList
      for(i in setdiff(possibleNumbers, numbersOfPair)){
        possibleSpotsList[[i]][pairIndex ,column] <- FALSE
      }
    }
  }

  #rows
  for(row in 1:9){
    #Create a list possible spots and the sudoku of a single row
    possiblePairs <- lapply(possiblePairsList, "[", row, )

    if(!any(is.na(possiblePairs$sudoku))){
      next
    }

    #Get the numbers that are not filled in for that row
    possibleNumbers <- setdiff(1:9, na.omit(possiblePairs$sudoku))
    #Select the spots matrix of the non filled in numbers and make it a matrix
    pairsCompare <- possiblePairs[possibleNumbers]
    class(pairsCompare) <- c("list","possibleSpotsVector")
    pairsCompare <- listToMatrix(pairsCompare, rowNumbering = possibleNumbers)

    if(length(possibleNumbers) < 2){
      next
    }

    #Use the search function to look for a pair.
    numbersOfPair <- pairOfNSearcher(pairsCompare = pairsCompare, possibleNumbers)

    if(length(numbersOfPair) == sum(is.na(sudoku[row,]))){
      next
    }
    if(length(numbersOfPair) > 4){
      next
    }

    if(!is.na(numbersOfPair[1])) {
      #If there is a pair get the index of the pair numbers
      pairIndex <- (1:9)[Reduce("+", possiblePairs[numbersOfPair]) > 0]
      if(returnHint){
        pairIndex <- cbind(row = row, col = pairIndex)
        hint <- list(nPair = length(numbersOfPair),
                     numbers = numbersOfPair,
                     direction = "row",
                     which = row,
                     index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass))
        class(hint) <- "hiddenPair"
        hintList <- append(hintList, list(hint))
      }

      #Remove possibility of the numbers not in the pair in the possibleSpotList
      for(i in setdiff(possibleNumbers, numbersOfPair)){
        possibleSpotsList[[i]][row ,pairIndex] <- FALSE
      }
    }
  }

  #To look for pairs in boxes, we first separate all boxes in a list.
  boxes <- boxSeperator(possiblePairsList)
  startIndexBoxes <- cbind(row = rep(c(1,4,7), each = 3), col = rep(c(1,4,7), 3) ) - 1

  for(box in 1:9){
    possiblePairs <- boxes[[box]]
    possibleNumbers <- setdiff(1:9, na.omit(as.vector(possiblePairs$sudoku)))

    if(length(possibleNumbers) < 2){
      next
    }

    #For the boxes, the possiblePairs matrixes are changed to vectors.
    pairsCompare <- lapply(possiblePairs[possibleNumbers], as.vector)
    class(pairsCompare) <- c("list","possibleSpotsVector")
    pairsCompare <- listToMatrix(pairsCompare, rowNumbering = possibleNumbers)

    #Use the search function to look for a pair.
    numbersOfPair <- pairOfNSearcher(pairsCompare = pairsCompare, possibleNumbers)
    if(length(numbersOfPair) > 4){
      next
    }
    if(length(numbersOfPair) == sum(is.na(possiblePairs$sudoku))){
      next
    }

    if(!is.na(numbersOfPair[1])) {
      #To get the index, we first look up the index of the cells of the pair in the box and then add the starting numbers of the box.
      pairIndex <- which(Reduce("+", possiblePairs[numbersOfPair]) > 0, arr.ind = TRUE) +
        rep(startIndexBoxes[box,], each  = length(numbersOfPair))

      if(returnHint){
        hint <- list(nPair = length(numbersOfPair),
                     numbers = numbersOfPair,
                     direction = "box",
                     which = box,
                     index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass))
        class(hint) <- "hiddenPair"
        #Check if the pair is allready found in a column or row

        #Find hint with the same numbers
        sameNumbers <- sapply(lapply(lapply(lapply(hintList, "[[", "numbers"), sort), as.numeric), identical, as.numeric(sort(hint$numbers)))
        # If a hint has the same numbers check if the indexes are the same, if not add the hint.
        if(any(sameNumbers)){
          #For loop for every hint with the same numbers
          sameIndexes <- rep(NA, sum(sameNumbers))
          h <- 1
          for(h in 1:sum(sameNumbers)){
            #Create a ordered matrix of indexes from the hintList
            oldIndexes <- matrix(unlist(hintList[sameNumbers][[h]]$index), ncol = 2, byrow = T)
            oldIndexes <- oldIndexes[order(oldIndexes[,1]),]
            #Create a ordered matrix of indexes from the hint in the box
            newIndexes <- matrix(unlist(hint$index), ncol = 2, byrow = T)
            newIndexes <- newIndexes[order(newIndexes[,1]),]

            #Check if the index of the new hint is the same as the h old hint.
            sameIndexes[h] <- identical(as.numeric(oldIndexes), as.numeric(newIndexes))
          }
          #If no index is the same, add to the hintList
          if(!any(sameIndexes)){
            hintList <- append(hintList, list(hint))
          }
        } else {
          hintList <- append(hintList, list(hint))
        }
      }

      #Remove possibility of the numbers not in the pair in the possibleSpotList
      for(i in setdiff(possibleNumbers, numbersOfPair)){
        possibleSpotsList[[i]][pairIndex] <- FALSE
      }
    }
  }
  if(returnHint){
    return(hintList)
  } else {
    return(possibleSpotsList)
  }
}

#This is the function that actually look for a pair in a row, column or box.
pairOfNSearcher <- function(pairsCompare, possibleNumbers){
  stop <- FALSE
  for(nPair in 2:(length(possibleNumbers))){
    #Get a matrix for all unqiue combinations of possible numbers with the pair lenght
    uniqComb <- uniqueCombinations(possibleNumbers, nPair)
    for(i in 1:nrow(uniqComb)){
      #look at what positions the numbers can fit
      allPositions <- apply(pairsCompare[as.integer(rownames(pairsCompare)) %in% uniqComb[i,],], 2, sum) > 0
      #If the number of positions is equal to the number of numbers there is pair.
      if(sum(allPositions) == nPair){
        #Return the numbers that form the pair and end the loop.
        return(uniqComb[i,])
        stop <- TRUE
      }
      if(stop) {break}
    }
    if(stop) {break}
  }
  #if not pair is found, the function returns a NA.
  if(!stop){
    return(NA)
  }
}


#A function to find naked pairs
nakedPairsFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()

  #Column
  for(column in 1:9){
    #Go to the next box if there are less than 2 empty spots in the box.
    if(sum(is.na(sudoku[,column])) < 2){
      next
    }

    #Get the index of all spots in the column in a matrix
    index <- cbind(row = 1:9, col = rep(column, 9))
    #Create a list of which numbers are possible in the spots
    possbileNumbersInSpots <- apply(index, 1, possibleNumbersInSpotFinder, x = possibleSpotsList)
    #Go to the next box if all spots only have 1 option.
    if(!is.list(possbileNumbersInSpots)){
      next
    }
    #Create a vector of how much options are possible in each spot.
    nOptionsInSpot <- sapply(possbileNumbersInSpots, length)
    #Change filled in spots to zero.
    nOptionsInSpot[!sapply(possbileNumbersInSpots, is.numeric)] <- 0

    #Look for pairs of n length
    for(nPair in 2:4){
      #Stop loop if a pair is formed because all the other spots are filled in
      if(sum(sapply(possbileNumbersInSpots, is.numeric)) == nPair){
        next
      }

      #Select all the spots with the number of options as the pair that is looked for.
      if(sum(nOptionsInSpot <= nPair & nOptionsInSpot > 0) >= nPair){
        #Select all the different combinations of spots
        spotCombinations <- uniqueCombinations((1:9)[nOptionsInSpot <= nPair & nOptionsInSpot > 0], nPair)
        for(i in 1:nrow(spotCombinations)){
          #For every combination, look if the spots have the same value.
          if(length(unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))) == nPair){
            #Create a vector of the numbers that form the pair
            nakedPair <- unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))
            #Get the index of all the spots that are not in the naked pair.
            notPairIndex <- index[-spotCombinations[i,],]



            if(returnHint){
              pairIndex <- index[spotCombinations[i,],]
              #Get a list indexes for all the numbers in the pair where a possabilitie is removed
              removedPossibleSpots <- vector(mode = "list", length = nPair)
              names(removedPossibleSpots) <- nakedPair
              for(p in 1:nPair){
                spotsOutsidePair <- which(possibleSpotsList[[nakedPair[p]]][,column])[!(which(possibleSpotsList[[nakedPair[p]]][,column]) %in% pairIndex)]
                removedPossibleSpots[[p]] <- vector(mode = "list", length = length(spotsOutsidePair))
                if(length(spotsOutsidePair) > 0){
                  for(ind in 1:length(spotsOutsidePair)){
                    removedPossibleSpots[[p]][[ind]] <- c(row = spotsOutsidePair[ind], col = column)
                    class(removedPossibleSpots[[p]][[ind]]) <- "index"
                  }
                }
              }
              if(sum(sapply(removedPossibleSpots, length)) > 0){
                hint <- list(nPair = nPair,
                             numbers = nakedPair,
                             direction = "column",
                             which = column,
                             index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass),
                             removedPossibleSpots = removedPossibleSpots)
                class(hint) <- "nakedPair"
                hintList <- append(hintList, list(hint))
              }
            }
            #Change the value in the possibleSpotList to false for all the pair numbers that are not in the pair spots.
            for(j in nakedPair){
              possibleSpotsList[[j]][notPairIndex] <- FALSE
            }
          }
        }
      }
    }
  }

  #Row
  for(row in 1:9){
    #Go to the next box if there are less than 2 empty spots in the box.
    if(sum(is.na(sudoku[row, ])) < 2){
      next
    }
    #Get the index of all spots in the column in a matrix
    index <- cbind(row = rep(row, 9), col = 1:9)
    #Create a list of which numbers are possible in the spots
    possbileNumbersInSpots <- apply(index, 1, possibleNumbersInSpotFinder, x = possibleSpotsList)
    #Go to the next box if all spots only have 1 option.
    if(!is.list(possbileNumbersInSpots)){
      next
    }
    #Create a vector of how much options are possible in each spot.
    nOptionsInSpot <- sapply(possbileNumbersInSpots, length)
    #Change filled in spots to zero.
    nOptionsInSpot[!sapply(possbileNumbersInSpots, is.numeric)] <- 0

    #Look for pairs of n lenght
    for(nPair in 2:4){
      #Stop loop if a pair is formed because all the other spots are filled in
      if(sum(sapply(possbileNumbersInSpots, is.numeric)) == nPair){
        next
      }
      #Select all the spots with the number op options as the pair that is looked for.
      if(sum(nOptionsInSpot <= nPair & nOptionsInSpot > 0) >= nPair){
        #Select all the different combinations of spots
        spotCombinations <- uniqueCombinations((1:9)[nOptionsInSpot <= nPair & nOptionsInSpot > 0], nPair)
        for(i in 1:nrow(spotCombinations)){
          #For every combination, look if the spots have the same value.
          if(length(unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))) == nPair){
            #Create a vector of the numbers that form the pair
            nakedPair <- unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))
            #Get the index of all the spots that are not in the naked pair.
            notPairIndex <- index[-spotCombinations[i,],]

            if(returnHint){
              pairIndex <- index[spotCombinations[i,],]

              #Get a list indexes for all the numbers in the pair where a possabilitie is removed
              removedPossibleSpots <- vector(mode = "list", length = nPair)
              names(removedPossibleSpots) <- nakedPair
              for(p in 1:nPair){
                spotsOutsidePair <- which(possibleSpotsList[[nakedPair[p]]][row,])[!(which(possibleSpotsList[[nakedPair[p]]][row,]) %in% pairIndex)]
                removedPossibleSpots[[p]] <- vector(mode = "list", length = length(spotsOutsidePair))
                if(length(spotsOutsidePair) > 0){
                  for(ind in 1:length(spotsOutsidePair)){
                    removedPossibleSpots[[p]][[ind]] <- c(row = row, col = spotsOutsidePair[ind])
                    class(removedPossibleSpots[[p]][[ind]]) <- "index"
                  }
                }

              }
              if(sum(sapply(removedPossibleSpots, length)) > 0){
                hint <- list(nPair = nPair,
                             numbers = nakedPair,
                             direction = "row",
                             which = row,
                             index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass),
                             removedPossibleSpots = removedPossibleSpots)
                class(hint) <- "nakedPair"
                hintList <- append(hintList, list(hint))
              }
            }

            #Change the value in the possibleSpotList to false for all the pair numbers that are not in the pair spots
            for(j in nakedPair){
              possibleSpotsList[[j]][notPairIndex] <- FALSE
            }
          }
        }
      }
    }
  }

  #boxes
  startIndexBoxes <- cbind(row = rep(c(1,4,7), each = 3), col = rep(c(1,4,7), 3) ) - 1
  for(box in 1:9){
    #print(box)
    index <- cbind(row = rep(1:3, each = 3), col = rep(1:3,3)) + rep(startIndexBoxes[box,], each = 9)

    #Go to the next box if there are less than 2 empty spots in the box.
    if(sum(is.na(sudoku[index])) < 2){
      next
    }

    #Create a list of which numbers are possible in the spots
    possbileNumbersInSpots <- apply(index, 1, possibleNumbersInSpotFinder, x = possibleSpotsList)

    #Go to the next box if all spots only have 1 option.
    if(!is.list(possbileNumbersInSpots)){
      next
    }

    #Create a vector of how much options are possible in each spot.
    nOptionsInSpot <- sapply(possbileNumbersInSpots, length)
    #Change filled in spots to zero.
    nOptionsInSpot[!sapply(possbileNumbersInSpots, is.numeric)] <- 0

    #Look for pairs of n length
    for(nPair in 2:4){
      #Stop loop if a pair is formed because all the other spots are filled in
      if(sum(sapply(possbileNumbersInSpots, is.numeric)) == nPair){
        next
      }
      #Select all the spots with the number op options as the pair that is looked for.
      if(sum(nOptionsInSpot <= nPair & nOptionsInSpot > 0) >= nPair){
        #Select all the different combinations of spots
        spotCombinations <- uniqueCombinations((1:9)[nOptionsInSpot <= nPair & nOptionsInSpot > 0], nPair)
        for(i in 1:nrow(spotCombinations)){
          #For every combination, look if the spots have the same value.
          if(length(unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))) == nPair){
            #Create a vector of the numbers that form the pair
            nakedPair <- unique(unlist(possbileNumbersInSpots[spotCombinations[i,]]))
            #Get the index of all the spots that are not in the naked pair.
            notPairIndex <- index[-spotCombinations[i,],]

            if(returnHint){
              pairIndex <- index[spotCombinations[i,],]
              #Get a list indexes for all the numbers in the pair where a possabilitie is removed
              removedPossibleSpots <- vector(mode = "list", length = nPair)
              names(removedPossibleSpots) <- nakedPair
              for(p in 1:nPair){
                indexOutsidePair <- notPairIndex[possibleSpotsList[[nakedPair[p]]][notPairIndex], ,drop = FALSE]

                if(nrow(indexOutsidePair) == 1){
                  removedPossibleSpots[[p]][[1]] <- createIndexClass(indexOutsidePair)
                } else if(nrow(indexOutsidePair) > 1) {
                  removedPossibleSpots[[p]] <- lapply(lapply(seq_len(nrow(indexOutsidePair)), function(i) indexOutsidePair[i,]), createIndexClass)
                }
              }
              if(sum(sapply(removedPossibleSpots, length)) > 0){
                hint <- list(nPair = nPair,
                             numbers = nakedPair,
                             direction = "box",
                             which = box,
                             index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass),
                             removedPossibleSpots = removedPossibleSpots)
                class(hint) <- "nakedPair"

                #Check if the pair is allready found in a column or row
                #Find hint with the same numbers
                sameNumbers <- sapply(lapply(lapply(lapply(hintList, "[[", "numbers"), sort), as.numeric), identical, as.numeric(sort(hint$numbers)))
                # If a hint has the same numbers check if the indexes are the same, if not add the hint.
                if(any(sameNumbers)){
                  #For loop for every hint with the same numbers
                  sameIndexes <- rep(NA, sum(sameNumbers))
                  for(h in 1:sum(sameNumbers)){
                    #Create a ordered matrix of indexes from the hintList
                    oldIndexes <- matrix(unlist(hintList[sameNumbers][[h]]$index), ncol = 2, byrow = T)
                    oldIndexes <- oldIndexes[order(oldIndexes[,1]),]
                    #Create a ordered matrix of indexes from the hint in the box
                    newIndexes <- matrix(unlist(hint$index), ncol = 2, byrow = T)
                    newIndexes <- newIndexes[order(newIndexes[,1]),]

                    sameIndexes[h] <- identical(as.numeric(oldIndexes), as.numeric(newIndexes))
                  }
                  if(!any(sameIndexes)){
                    hintList <- append(hintList, list(hint))
                  }
                } else {
                  hintList <- append(hintList, list(hint))
                }
              }
            }
            #Change the value in the possibleSpotList to false for all the pair numbers that are not in the pair spots.
            for(j in nakedPair){
              possibleSpotsList[[j]][notPairIndex] <- FALSE
            }
          }
        }
      }
    }
  }
  if(returnHint){
    return(hintList)
  } else {
    return(possibleSpotsList)
  }
}


