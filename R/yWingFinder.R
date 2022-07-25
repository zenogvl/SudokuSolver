
# This function looks for y wings.
# For a explanation of of y wings, search online for more information.
yWingFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  possibleSpotsListOut <- possibleSpotsList
  hintList <- list()

  #Get the indexes of all spots that have two possabilities
  TwoPosSpots <- which(Reduce("+", possibleSpotsList) == 2, arr.ind = TRUE)

  for(cell in 1:nrow(TwoPosSpots)){
    if(nrow(TwoPosSpots) == 0){
      break
    }
    cellSelect <- TwoPosSpots[cell,]
    cellSelectNumbers <- possibleNumbersInSpotFinder(cellSelect, x = possibleSpotsList)

    if(nrow(getYtargetCells(cellSelect, TwoPosSpots[-cell,])) == 0){
      next
    }

    #Get the indexes and numbers of all the cells the selected cell inflences
    targets <- cbind(getYtargetCells(cellSelect, TwoPosSpots[-cell,]),
                     t(apply(getYtargetCells(cellSelect, TwoPosSpots[-cell,]), 1, possibleNumbersInSpotFinder, x = possibleSpotsList))
                     )
    if(nrow(targets) > 1){
      #Remove the cells that don't contain a number that is in the selected cell.
      targets <- targets[apply(targets[,3:4] == cellSelectNumbers[1] | targets[,3:4] == cellSelectNumbers[2], 1, any),, drop = FALSE]

      #Select numbers for the y wing to form.
      targetNumberCount <- table(as.vector(targets[,3:4])[!(as.vector(targets[,3:4]) %in% cellSelectNumbers)])
      targetNumbers <- as.integer(names(targetNumberCount)[targetNumberCount >= 2])

      #Select the indexes that contain the targetNumber
      for(targetNumb in targetNumbers){
        singleTargetNumb1 <- targets[apply(targets[,3:4] == targetNumb, 1, any) & apply(targets[,3:4] == cellSelectNumbers[1], 1, any),, drop = FALSE]
        singleTargetNumb2 <- targets[apply(targets[,3:4] == targetNumb, 1, any) & apply(targets[,3:4] == cellSelectNumbers[2], 1, any),, drop = FALSE]

        if(!(nrow(singleTargetNumb1) == 0 || nrow(singleTargetNumb2) == 0)){
          for(i in 1:nrow(singleTargetNumb1)){
            for(j in 1:nrow(singleTargetNumb2)){
              #Look for any spot that is TRUE and is seen by both the target numbers if, this is the case, there is a y wing.
              cellsToCheckForNumber <- cellInfluenceShower(rbind(singleTargetNumb1[i,1:2], singleTargetNumb2[j,1:2]))
              if(any(possibleSpotsList[[targetNumb]][cellsToCheckForNumber])){
                cellsToCheckForNumber <- cellInfluenceShower(rbind(singleTargetNumb1[i,1:2], singleTargetNumb2[j,1:2]))
                indexCellsToChange <- cellsToCheckForNumber[possibleSpotsList[[targetNumb]][cellsToCheckForNumber],, drop = FALSE]

                #Produce an hint.
                if(returnHint){
                  hint <- list(cellIndexes = list(start = createIndexClass(cellSelect),
                                                  wing1 = createIndexClass(singleTargetNumb1[i,1:2]),
                                                  wing2 = createIndexClass(singleTargetNumb2[j,1:2])),
                               cellNumbers = rbind(cellSelectNumbers, singleTargetNumb1[i,3:4], singleTargetNumb2[j,3:4]),
                               changedCellIndex = indexMatrix2List(indexCellsToChange),
                               changedCellNumber = targetNumb,
                               result = setdiff(possibleNumbersInSpotFinder(indexCellsToChange, possibleSpotsList), targetNumb))


                  class(hint) <- "ywing"
                  #return(hintOutput)
                  hintList <- append(hintList, list(hint))
                }

                #Change the possibleSpotList to update it because of the y wing.
                possibleSpotsListOut[[targetNumb]][cellInfluenceShower(rbind(singleTargetNumb1[i,1:2], singleTargetNumb2[j,1:2]))] <- FALSE
              }
            }
          }
        }
      }
    }
  }
  if(returnHint){
    return(hintList)
  } else {
    return(possibleSpotsListOut)
  }
}


#Function to get the cell indexes that find indexes of the cells the target cell can form an y wing with.
getYtargetCells <- function(posSelect, TwoPosSpots){
  out <- rbind(cellInfluenceShower(posSelect),TwoPosSpots)
  out <- out[duplicated(out),, drop = FALSE]
  return(out[!apply(out, 1, identical, y = posSelect),, drop = FALSE])
}



