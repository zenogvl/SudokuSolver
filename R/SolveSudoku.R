

#' Sudoku Solver
#'
#' This function solves a sudoku. It does this by using the follow techniques: lone, hidden and naked singles,
#' hidden and naked pairs(and triples and quadruples), pointing pairs and triples, box line reduction, x-wings (and swordfishes and
#' jellyfishes), y-wings and skyscrapers. When another technique is needed to solve the sudoku, this function will
#' not be able to solve the sudoku.
#'
#' @param sudoku A object with data.frame, matrix or sudoku. It must range Must comply with general sudoku rules so have 9 rows and
#' columns and have numbers that range from 1 to 9. Empty spots must be denoted with a 0 or an NA. If the input sudoku has an error,
#' the function can result in random errors.
#' @return Returns the solved sudoku with an s3 class called sudoku. This object can be plotted to get a clear
#' visualization of the solved sudoku. This function also check if the sollution is correct and automaticly plots the sudoku.
#' @export
solveSudoku <- function(sudoku) {
  sudoku <- inputControl(sudoku)

  possibleSpotsList <- lapply(1:9, function(x) matrix(TRUE,nrow = 9, ncol = 9))
  possibleSpotsList <- possibleSpotEliminater(possibleSpotsList = possibleSpotsList, sudoku = sudoku)

  sudoku <- singleFinder(sudoku = sudoku, possibleSpotsList = possibleSpotsList)

  possibleSpotsList <- possibleSpotEliminater(possibleSpotsList = possibleSpotsList, sudoku = sudoku)

  removeCanditatesFunctions <- list(pointingPairFinder,
                                    boxLineReduction,
                                    hiddenPairsFinder,
                                    nakedPairsFinder,
                                    wingFinder,
                                    yWingFinder,
                                    skyscraperFinder)

  progressCounterBefore <- sum(!is.na(sudoku))
  progressCounterAfter <- 0
  while( progressCounterBefore != progressCounterAfter && sum(!is.na(sudoku)) != 81){
    progressCounterBefore <- sum(!is.na(sudoku))

    #print(paste("progress Before = ",progressCounterBefore))



    for(f in 1:length(removeCanditatesFunctions)){
      #print(c(f = f))

      possibleSpotsList <- do.call(removeCanditatesFunctions[[f]], list(possibleSpotsList, sudoku))

      update <- sudokuSolutionUpdater(possibleSpotsList, sudoku)

      possibleSpotsList <- update$possibleSpotsList
      sudoku <- update$sudoku


      if(sum(is.na(sudoku)) == 0){
        break
      }

    }
    progressCounterAfter <- sum(!is.na(sudoku))
  }
  if(sum(!is.na(sudoku)) < 81){
    print("Could not solve the sudoku, there is probably a method required that isn't part of this function or there isn't a unique solution")
    plot(sudoku)
    return(sudoku)
  } else {
    print(sudokuCorrectnessCheck(sudoku))
    plot(sudoku)
    return(sudoku)
  }
}

