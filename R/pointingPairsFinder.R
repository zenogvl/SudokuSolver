

#Pointing pairs finder function
#There is a pointing pair when a number in a single box is limited to a single line. The number cannot be in other spots in this line.

pointingPairFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()
  startIndexBoxes <- cbind(row = rep(c(1,4,7), each = 3), col = rep(c(1,4,7), 3), box = 1:9)

  for(numb in 1:9){
    for(i in c(1,4,7)){
      for(j in c(1,4,7)){
        if(sum(possibleSpotsList[[numb]][i:(i+2), j:(j+2)]) == 1){
          next
        }

        #Omission in columns
        if(sum(apply(possibleSpotsList[[numb]][i:(i+2), j:(j+2)], 2, any)) == 1){ #TRUE when only one column in a box contains a number
          #When ommision is possible the relevent column and rows are selected.

          #The column is selected based on where the box start plus the column where the ommision spot is.
          columnSelect <- j + (0:2)[apply(possibleSpotsList[[numb]][i:(i+2), j:(j+2)], 2, any)]
          #The rows are selected by substracting the rows of the current box
          rowSelect <- setdiff(1:9, i:(i+2))

          if(returnHint){
            #Select all the indexes of the relevant number
            allIndexes <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
            #Select the indexes of the column of the pointing pair
            colIndexes <- allIndexes[allIndexes[,2] == columnSelect, , drop = FALSE]
            #Get the pointing pair index
            pointingPairIndex <- colIndexes[colIndexes[,1] %in% i:(i+2), , drop = FALSE]
            #Get the index of eliminated spots
            omissionIndex <- colIndexes[colIndexes[,1] %!in% i:(i+2), , drop = FALSE]

            #Check if there are spots eliminated by the pointing pair
            if(nrow(omissionIndex) != 0){
              hint <- list(number = numb,
                           nPair = nrow(pointingPairIndex),
                           box =  startIndexBoxes[startIndexBoxes[,1] == i & startIndexBoxes[,2] == j, "box"],
                           direction = "column",
                           which = columnSelect,
                           index = lapply(lapply(seq_len(nrow(pointingPairIndex)), function(i) pointingPairIndex[i,]), createIndexClass),
                           omissionIndex = lapply(lapply(seq_len(nrow(omissionIndex)), function(i) omissionIndex[i,]), createIndexClass))
              class(hint) <- "pointingPair"
              hintList <- append(hintList, list(hint))
            }
          }
          #All the other rows in the column are set to FALSE
          possibleSpotsList[[numb]][rowSelect, columnSelect] <- FALSE
        }

        #Omission in rows. The logic is the same for columns.
        if(sum(apply(possibleSpotsList[[numb]][i:(i+2), j:(j+2)], 1, any)) == 1){
          rowSelect <- i + (0:2)[apply(possibleSpotsList[[numb]][i:(i+2), j:(j+2)], 1, any)]
          columnSelect <- setdiff(1:9, j:(j+2))

          if(returnHint){
            allIndexes <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
            rowIndexes <- allIndexes[allIndexes[,1] == rowSelect, , drop = FALSE]
            pointingPairIndex <- rowIndexes[rowIndexes[,2] %in% j:(j+2), , drop = FALSE]
            omissionIndex <- rowIndexes[rowIndexes[,2] %!in% j:(j+2), , drop = FALSE]

            if(nrow(omissionIndex) != 0){
              hint <- list(number = numb,
                           nPair = nrow(pointingPairIndex),
                           box =  startIndexBoxes[startIndexBoxes[,1] == i & startIndexBoxes[,2] == j, "box"],
                           direction = "row",
                           which = rowSelect,
                           index = lapply(lapply(seq_len(nrow(pointingPairIndex)), function(i) pointingPairIndex[i,]), createIndexClass),
                           omissionIndex = lapply(lapply(seq_len(nrow(omissionIndex)), function(i) omissionIndex[i,]), createIndexClass))
              class(hint) <- "pointingPair"
              hintList <- append(hintList, list(hint))
            }
          }
          possibleSpotsList[[numb]][rowSelect, columnSelect] <- FALSE
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

#Box line reduction: when the possible spots for a number in a line is limated to a single box, the number cannot be in the other spots in this box.
boxLineReduction <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()

  #This is a matrix where for every row, the other lines in the box are returned.
  otherLinesinBox <- matrix(c(2, 3,
                              1, 3,
                              1, 2,
                              5, 6,
                              4, 6,
                              4, 5,
                              8, 9,
                              7, 9,
                              7, 8),
                            ncol = 2, byrow = TRUE)
  #A matrix used to select the box number for the hintczxa CVBVGDSAxasREWQ  asF\DCGBHGSADQA   QADFDSG A
  boxSelect <- matrix(c(1, 4, 7,
                        2, 5, 8,
                        3, 6, 9),
                      nrow = 3)

  for(numb in 1:9){
    for(row in 1:9){
      if(sum(possibleSpotsList[[numb]][row,]) == 1){
        next
      }

      #Look in how many boxes the number is
      numberInBoxes <- sapply(split(possibleSpotsList[[numb]][row,], rep(1:3, each = 3)), any)
      #When the number is only in one box, box line reduction is possible.
      if(sum(numberInBoxes) == 1){
        #Check if a possible spot is actually changed by the box line reduction
        if(any(possibleSpotsList[[numb]][otherLinesinBox[row,], cbind(1:3, 4:6, 7:9)[,numberInBoxes]])){
          if(returnHint){
            allIndexes <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
            pairIndex <- allIndexes[allIndexes[,1] == row, , drop = FALSE]
            omissionIndex <- allIndexes[allIndexes[,1] %in% otherLinesinBox[row,] & allIndexes[,2] %in% cbind(1:3, 4:6, 7:9)[,numberInBoxes], , drop = FALSE]
            hint <- list(numb = numb,
                         direction = "row",
                         which = row,
                         box = boxSelect[rep(1:3, each = 3)[row] ,numberInBoxes],
                         index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass),
                         omissionIndex = lapply(lapply(seq_len(nrow(omissionIndex)), function(i) omissionIndex[i,]), createIndexClass)
            )
            class(hint) <- "boxLineReducted"
            hintList <- append(hintList, list(hint))
          }

          #Change the possible spots list.
          possibleSpotsList[[numb]][otherLinesinBox[row,], cbind(1:3, 4:6, 7:9)[,numberInBoxes]] <- FALSE
        }
      }
    }
    # Columm
    for(column in 1:9){
      if(sum(possibleSpotsList[[numb]][, column]) == 1){
        next
      }

      numberInBoxes <- sapply(split(possibleSpotsList[[numb]][,column], rep(1:3, each = 3)), any)
      if(sum(numberInBoxes) == 1){
        if(any(possibleSpotsList[[numb]][cbind(1:3, 4:6, 7:9)[,numberInBoxes], otherLinesinBox[column,]])){
          if(returnHint){
            allIndexes <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
            pairIndex <- allIndexes[allIndexes[,2] == column, , drop = FALSE]
            omissionIndex <- allIndexes[allIndexes[,2] %in% otherLinesinBox[column,] & allIndexes[,1] %in% cbind(1:3, 4:6, 7:9)[,numberInBoxes], , drop = FALSE]

            hint <- list(numb = numb,
                         direction = "column",
                         which = column,
                         box = boxSelect[numberInBoxes,  rep(1:3, each = 3)[column]],
                         index = lapply(lapply(seq_len(nrow(pairIndex)), function(i) pairIndex[i,]), createIndexClass),
                         omissionIndex = lapply(lapply(seq_len(nrow(omissionIndex)), function(i) omissionIndex[i,]), createIndexClass)
                         )
            class(hint) <- "boxLineReducted"
            hintList <- append(hintList, list(hint))

          }
          possibleSpotsList[[numb]][cbind(1:3, 4:6, 7:9)[,numberInBoxes], otherLinesinBox[column,]] <- FALSE
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




