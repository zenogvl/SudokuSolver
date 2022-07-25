
# This function find x-wings, swordfishes and sudokus.
# For more infromatation about winges, look online for a full explanation.

wingFinder <- function(possibleSpotsList, sudoku,  returnHint = FALSE){
  hintList <- list()

  for(numb in 1:9){
    possibleSpots <- possibleSpotsList[[numb]]

    ### Column

    #Get a vector of how many places the number can go to in for each column
    nPossibleCol <- apply(possibleSpots, 2, sum)

    if(sum(nPossibleCol) == 0){
      next
    }

    #Look for xwings, swordfishes and jellyfisher
    for(nWing in 2:4){
      #Only proceed with looking for a wing if there are equal or more columns that contain or equal or less than nWing options.
      if(sum(nPossibleCol <= nWing & nPossibleCol != 0) >= nWing){
        #Get the unique combinations of columns that contain equal or less options than nWing.
        uniqCombCols <- uniqueCombinations((1:9)[nPossibleCol <= nWing & !(nPossibleCol == 0)], nWing)
        #For every column combination check for a wing.
        for(i in 1:nrow(uniqCombCols)){
          #When for a certain column combination the number of rows where a number can fit is equal to nWing there is a wing.
          if(sum(apply(possibleSpots[,uniqCombCols[i, , drop = FALSE]], 1, any)) == nWing){
            #Check if one of the colums have only a single option. If so, it will be solved by a single finder
            if(any(apply(possibleSpots[,uniqCombCols[i, , drop = FALSE]], 2, sum) == 1)){
              next
            }

            #Get a vector of rows where the wing is in.
            wingRows <- (1:9)[apply(possibleSpots[,uniqCombCols[i, , drop = FALSE]], 1, any)]
            if(length(wingRows) == 0 | !any(possibleSpots[wingRows, -uniqCombCols[i, , drop = FALSE]])){
              next
            }

            if(returnHint){
              indexAll <- which(possibleSpots, arr.ind = TRUE)
              index <- indexAll[indexAll[,2] %in% uniqCombCols[i,],]
              indexList <- vector(mode = "list", length = nrow(index))

              for(ind in 1:nrow(index)){
                indexList[[ind]] <- createIndexClass(index[ind,])
              }

              indexOmmission <- indexAll[indexAll[,1] %in% wingRows & indexAll[,2] %!in% uniqCombCols[i,], , drop = FALSE]
              indexOmmissionList <- vector(mode = "list", length = nrow(indexOmmission))

              for(ind in 1:nrow(indexOmmission)){
                indexOmmissionList[[ind]] <- createIndexClass(indexOmmission[ind,])
              }

              hint <- list(number = numb,
                           nWing = nWing,
                           direction = "column",
                           cols = uniqCombCols[i,],
                           rows = wingRows,
                           index = indexList,
                           ommissionIndex = indexOmmissionList)
              class(hint) <- c("column","wing")
              hintList <- append(hintList, list(hint))
            }
            #Change the possibleSpots
            possibleSpotsList[[numb]][wingRows,setdiff(1:9,uniqCombCols[i,])] <- FALSE

          }
        }
      }
    }

    ### Rows
    nPossibleRow <- apply(possibleSpots, 1, sum)

    if(sum(nPossibleRow) == 0){
      next
    }

    #Look for xwings, swordfishes and jellyfisher
    for(nWing in 2:4){
      #Only proceed with looking for a wing if there are equal or more rows that contain or equal or less than nWing options.
      if(sum(nPossibleRow <= nWing & nPossibleRow != 0) >= nWing){

        #Get the unique combinations of columns that contain equal or less options than nWing.
        uniqCombRows <- uniqueCombinations((1:9)[nPossibleRow <= nWing & !(nPossibleRow == 0)], nWing)
        #For every row combination check for a wing.
        for(i in 1:nrow(uniqCombRows)){
          #When for a certain column combination the number of rows where a number can fit is equal to nWing there is a wing.
          if(sum(apply(possibleSpots[uniqCombRows[i,], ,drop = FALSE], 2, any)) == nWing){

            if(any(apply(possibleSpots[uniqCombRows[i,], ,drop = FALSE], 1, sum) == 1)){
              next
            }

            #Get a vector of colums where the wing is in.
            wingCols <- (1:9)[apply(possibleSpots[uniqCombRows[i,], ,drop = FALSE], 2, any)]

            if(length(wingCols) == 0 || !any(possibleSpots[-uniqCombRows[i, , drop = FALSE], wingCols])){
              next
            }

            if(returnHint){
              indexAll <- which(possibleSpots, arr.ind = TRUE)
              index <- indexAll[indexAll[,1] %in% uniqCombRows[i,],]
              indexList <- vector(mode = "list", length = nrow(index))
              for(ind in 1:nrow(index)){
                indexList[[ind]] <- createIndexClass(index[ind,])
              }

              indexOmmission <- indexAll[indexAll[,2] %in% wingCols & indexAll[,1] %!in% uniqCombRows[i,], , drop = FALSE]
              indexOmmissionList <- vector(mode = "list", length = nrow(indexOmmission))

              for(ind in 1:nrow(indexOmmission)){
                indexOmmissionList[[ind]] <- createIndexClass(indexOmmission[ind,])
              }

              hint <- list(number = numb,
                           nWing = nWing,
                           direction = "row",
                           cols = uniqCombRows[i,],
                           rows = wingCols,
                           index = indexList,
                           ommissionIndex = indexOmmissionList)
              class(hint) <- c("row","wing")
              hintList <- append(hintList, list(hint))
            }
            #Change the possibleSpots
            possibleSpotsList[[numb]][setdiff(1:9,uniqCombRows[i,]),wingCols] <- FALSE

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






