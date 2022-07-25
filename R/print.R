
#Change a index provided with a hint to a text to print.
indexToText <- function(index){
  paste0("R", index[1], "C", index[2])
}
#Function to get one or multiple numbers in a nice text format to print.
numbersToText <- function(numbers, inBrackets = TRUE){
  if(inBrackets){
    paste0("[",paste0(as.character(numbers), collapse = "/"),"]")
  } else {
    if(length(numbers) > 1){
      paste0(paste0(as.character(numbers[-(length(numbers))]), collapse = ", "), " & ", numbers[length(numbers)])
    } else {
      as.character(numbers)
    }
  }
}

#Functions to print a hint.
printHint <- function(hint, ...){
  UseMethod("printHint", hint)
}
printHint.default <- function(hint){
  print("default")
}

printHint.ywing <- function(hint){

  if(length(hint$changedCellIndex) == 1){
    changedCellIndexes <- indexToText(hint$changedCellIndex)
  } else if (length(hint$changedCellIndex) == 2){
    changedCellIndexes <- paste0(indexToText(hint$changedCellIndex[[1]]), " & ", indexToText(hint$changedCellIndex[[2]]))
  } else {
    changedCellIndexes <-  paste0(paste0(sapply(hint$changedCellIndex, indexToText)[-length(hint$changedCellIndex)], collapse = ", "), " & ", indexToText(hint$changedCellIndex[[length(hint$changedCellIndex)]]))
  }

  cat(paste0("Y-wing:", "\n", indexToText(hint$cellIndexes$start), " can take the numbers " ,
             hint$cellNumbers[1,1], " and ", hint$cellNumbers[1,2], ".",
             " One of the cells: ", indexToText(hint$cellIndexes$wing1), numbersToText(hint$cellNumbers[2,]),
             " & ", indexToText(hint$cellIndexes$wing2), numbersToText(hint$cellNumbers[3,]),
             " is thus always ", hint$changedCellNumber, ". Becasue of this, ", changedCellIndexes,
             " can't be ", hint$changedCellNumber,  "."))
}

printHint.wing <- function(hint, ...){
  UseMethod("printHint.wing", hint)
}
printHint.wing.column <- function(hint){
  name <- c("xwing", "swordfish", "jellyfish")[hint$nWing - 1]
  wingCols <- paste0(paste0(hint$cols[1:(hint$nWing - 2)], collapse = " and "), " & " , hint$cols[hint$nWing])
  rows <- paste0(paste0(hint$rows[1:(hint$nWing - 2)], collapse = " and "), " & " , hint$rows[hint$nWing])

  cat(paste0(name, ": \n", "In columns ", wingCols, " the number ", hint$number,
             " is limited to rows ", rows, ". ", hint$number, " can thus be removed as an option in all the columns in rows ", rows, "."))

}
printHint.wing.row <- function(hint){
  name <- c("xwing", "swordfish", "jellyfish")[hint$nWing - 1]
  wingRows <- paste0(paste0(hint$rows[1:(hint$nWing - 2)], collapse = " and "), " & " , hint$rows[hint$nWing])
  cols <- paste0(paste0(hint$cols[1:(hint$nWing - 2)], collapse = " and "), " & " , hint$cols[hint$nWing])

  cat(paste0(name, ": \n", "In rows ", wingRows, " the number ", hint$number,
             " is limited to columns ", cols, ". ", hint$number, " can thus be removed as an option in all the rows in columns ", cols, "."))

}

printHint.nakedPair <- function(hint){
  name <- c("Pair", "Triple", "Quadruple")[hint$nPair - 1]

  if(hint$nPair == 2){
    indexPair <- paste0(indexToText(hint$index[[1]]), " & ", indexToText(hint$index[[2]]))
  } else {
    indexPair <- paste0(paste0(sapply(hint$index, indexToText)[-hint$nPair], collapse = ", "), " & ", indexToText(hint$index[[hint$nPair]]))
  }

  cat(paste0("Naked ", name, ": \n In ", hint$direction, " ", hint$which, " numbers: ",
             numbersToText(hint$numbers, inBrackets = FALSE), " are limated to cells ", indexPair,
             ". These numbers can thus be removed as options from the other cells in this ", hint$direction, ".")
      )
}

printHint.hiddenPair <- function(hint){
  name <- c("Pair", "Triple", "Quadruple")[hint$nPair - 1]

  if(hint$nPair == 2){
    indexPair <- paste0(indexToText(hint$index[[1]]), " & ", indexToText(hint$index[[2]]))
  } else {
    indexPair <- paste0(paste0(sapply(hint$index, indexToText)[-hint$nPair], collapse = ", "), " & ", indexToText(hint$index[[hint$nPair]]))
  }

  cat(paste0("Hidden ", name, ": \n In ", hint$direction, " ", hint$which, " numbers: ",
             numbersToText(hint$numbers, inBrackets = FALSE), " can only be placed in ",
             indexPair, ". Other numbers thus can't be placed in those ", hint$nPair, " cells."
             ))

}

printHint.pointingPair <- function(hint){
  name <- c("Pair", "Triple")[hint$nPair - 1]

  nOmmission <- length(hint$omissionIndex)

  if(nOmmission == 2){
    indexOmission <- paste0(indexToText(hint$omissionIndex[[1]]), " & ", indexToText(hint$omissionIndex[[2]]))
  } else {
    indexOmission <- paste0(paste0(sapply(hint$omissionIndex, indexToText)[-nOmmission], collapse = ", "), " & ", indexToText(hint$omissionIndex[[nOmmission]]))
  }

  cat(paste0("Pointing ", name , ": \n In box ", hint$box, ", ", hint$number,
             " can only go in ", hint$direction, " ", hint$which, ". ", hint$number,
             " thus can't be in ", indexOmission))


}
printHint.boxLineReducted <- function(hint){

  nOmmission <- length(hint$omissionIndex)

  if(nOmmission == 1){
    indexOmission <- indexToText(hint$omissionIndex[[1]])
  } else if(nOmmission == 2){
    indexOmission <- paste0(indexToText(hint$omissionIndex[[1]]), " & ", indexToText(hint$omissionIndex[[2]]))
  } else {
    indexOmission <- paste0(paste0(sapply(hint$omissionIndex, indexToText)[-nOmmission], collapse = ", "), " & ", indexToText(hint$omissionIndex[[nOmmission]]))
  }


  cat(paste0("Box line reduction: \n In ", hint$direction, " ", hint$which, ", ", hint$numb,
      " can only go in box ", hint$box, ". ", indexOmission, " thus cannot be ", hint$numb))

}

printHint.hiddenSingle <- function(hint){
  cat(paste0("Hidden Single: \n In ", hint$direction, " ", hint$which,
    ", ", hint$number, " can only go in ", indexToText(hint$index), "."
  ))
}
printHint.loneSingle <- function(hint){
  cat(paste0("Lone Single: \n The only option for cell ", indexToText(hint$index), " is a ",
             hint$number, "."
  ))
}
printHint.skyscraper <- function(hint){

  nSkyscrapperTop <- length(hint$skyscraperTop)
  if(nSkyscrapperTop == 2){
    indexSkyscrapperTop <- paste0(indexToText(hint$skyscraperTop[[1]]), " & ", indexToText(hint$skyscraperTop[[2]]))
  } else {
    indexSkyscrapperTop <- paste0(paste0(sapply(hint$skyscraperTop, indexToText)[-nSkyscrapperTop], collapse = ", "), " & ", indexToText(hint$skyscraperTop[[nSkyscrapperTop]]))
  }

  nOmmission <- length(hint$ommittedCells)
  if(nOmmission == 1){
    indexOmission <- indexToText(hint$ommittedCells[[1]])
  } else if(nOmmission == 2){
    indexOmission <- paste0(indexToText(hint$ommittedCells[[1]]), " & ", indexToText(hint$ommittedCells[[2]]))
  } else {
    indexOmission <- paste0(paste0(sapply(hint$ommittedCells, indexToText)[-nOmmission], collapse = ", "), " & ", indexToText(hint$ommittedCells[[nOmmission]]))
  }

  cat(paste0("Skyscraper: \n In ", hint$direction, " ", hint$which, ", ", hint$number,
             " can only go in two spots: ", indexToText(hint$skyscraperBase[[1]]), " & ", indexToText(hint$skyscraperBase[[2]]),
             " this means that there is alway a number in the cells: ", indexSkyscrapperTop,
             " these cells see: ", indexOmission, ". These cells thus can't be ", hint$number, "."
             ))
}



