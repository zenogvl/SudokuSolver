

#' Get a hint to solve the sudoku
#'
#' This function gives hints to solve sudokus. It can give hints for the following situations:hidden and naked singles,
#' hidden and naked pairs(and triples and quadruples), pointing pairs and triples, box line reduction, x-wings (and swordfishes and
#' jellyfishes), y-wings and skyscrapers. The user can ask for all hints or select a certain technique from above. It will provide the
#' hints one at the time and asks in the console if the user want to move on.
#'
#' @param sudoku A object with data.frame, matrix or sudoku. It must range Must comply with general sudoku rules so have 9 rows and
#' columns and have numbers that range from 1 to 9. Empty spots must be denoted with a 0 or an NA. If the input sudoku has an error,
#' the function can result in random errors.
#' @param randomHint If TRUE, the user will get all the hints. If FALSE, the user can select a desired hint. TRUE by default.
#'
#' @return Returns a (not completed) sudoku.
#' @export
sudokuHint <- function(sudoku, randomHint = TRUE){
  sudoku <- inputControl(sudoku)
  possibleSpotsList <- lapply(1:9, function(x) matrix(TRUE,nrow = 9, ncol = 9))
  possibleSpotsList <- possibleSpotEliminater(possibleSpotsList = possibleSpotsList, sudoku = sudoku)

  if(!is.logical(randomHint)){
    stop("randomHint must be TRUE of FALSE")
  }


  removeCanditatesFunctions <- list(hiddenSingleFinder,
                                    pointingPairFinder,
                                    boxLineReduction,
                                    hiddenPairsFinder,
                                    nakedPairsFinder,
                                    wingFinder,
                                    yWingFinder,
                                    loneSinglesFinder,
                                    skyscraperFinder)

  allHintClasses <- c("hiddenSingle", "pointingPair", "boxLineReducted", "hiddenPair", "nakedPair", "wing", "ywing", "loneSingle", "skyscraper")

  if(randomHint){
    start <- TRUE
    hintListOld <- list()
    while(start){
      #Get a list of hints.
      hintList <- list()
      for(f in 1:length(removeCanditatesFunctions)){
        hintList <- append(hintList, do.call(removeCanditatesFunctions[[f]], list(possibleSpotsList, sudoku, returnHint = TRUE)))
      }

      if(length(hintList) == 0){
        cat("Sorry, not a single hint could be generated \n")
        break
      }

      sameHints <- rep(FALSE, length(hintList))
      if(length(hintListOld) == length(hintList)){
        for(i in 1:length(hintList)){
          sameHints[i] <- identical(hintListOld[[1]],hintList[[1]])
        }
      }
      if(any(sameHints)){
        cat("Sorry, no new hints could be genareted \n")
        break
      }

      # A loop that shows the hints.
      h <- 0
      continue <- "y"
      while(h < length(hintList) && continue == "y"){

        h <- h + 1

        plot(sudoku)
        plot(hintList[[h]])
        printHint(hintList[[h]])

        continue <- readline(prompt = "next hint? (y for yes, n for no): ")

        while(continue %!in% c("y", "n")){
          continue <- readline("You made a mistake, you can only enter y or n, \n please try again:")
        }
      }

      cat("All the genarated hints are shown or the user terminated the process. ")
      solveHints <- readline("Do you want the program to solve the sudoku with the given hints? (y for yes, n for no): ")
      while(solveHints %!in% c("y", "n")){
        solveHints <- readline("You made a mistake, you can only enter y or n, please try again:")
      }

      if(solveHints == "y") {
        if(h < length(hintList)){
          cat("Warning: You did not complete all the genarated hints. Only the hints that are shown are solved. However, some of the last hints might not be included in solved sudoku")

          #Get a loop that ends with the function before the function of the last hint.
          for(f in 1:((1:length(removeCanditatesFunctions))[allHintClasses == lapply(hintList, class)[[4]]] - 1)){
            #Solve the sudoku.
            possibleSpotsList <- do.call(removeCanditatesFunctions[[f]], list(possibleSpotsList, sudoku))
            if(length(possibleSpotsList) == 2){
              sudoku <- possibleSpotsList$sudoku
              possibleSpotsList <- possibleSpotsList$possibleSpotsList
            } else {
              update <- sudokuSolutionUpdater(possibleSpotsList, sudoku)
              possibleSpotsList <- update$possibleSpotsList
              sudoku <- update$sudoku
            }
          }
        } else {

          # A for loop that uses all the function to solve.
          for(f in 1:length(removeCanditatesFunctions)){
            possibleSpotsList <- do.call(removeCanditatesFunctions[[f]], list(possibleSpotsList, sudoku))
            if(length(possibleSpotsList) == 2){
              sudoku <- possibleSpotsList$sudoku
              possibleSpotsList <- possibleSpotsList$possibleSpotsList
            } else {
              update <- sudokuSolutionUpdater(possibleSpotsList, sudoku)
              possibleSpotsList <- update$possibleSpotsList
              sudoku <- update$sudoku
            }
          }
        }
        if(sum(!is.na(sudoku)) == 81){
          cat("With those hints the sudoku can be completed, good luck! \n")
          plot(sudoku)
          start <- FALSE
        } else {
          cat("Do you want to generate a new set of hints based on the new sudoku? ")
          start <- ifelse("y" == readline("Press y to generate new hints or press any other key to stop: "),
                          yes = TRUE,
                          no = FALSE)
          hintListOld <- hintList
        }
      } else {
        start <- FALSE
      }
    }
  } else if (!randomHint){
    start <- TRUE
    while(start){
      cat("What stratagy do you want to use to generate a hint? \n
    1) Hidden Single
    2) Pointing Pair/Triple
    3) Box Line Reduction
    4) Hidden Pair/Triple/Quad
    5) Naked Pair/Triple/Quad
    6) X-wing, Swordfish or Jellyfish
    7) Y-wing
    8) Lone Single
    9) Skyscrapper \n")
      f <- readline("Press a number to choose: ")

      while(f %!in% as.character(1:length(removeCanditatesFunctions))){
        cat("The input should be a number from 1 to 8")
        f <- readline("Press a number to choose again: ")
      }
      hintList <- list()
      hintList <- append(hintList, do.call(removeCanditatesFunctions[[f]], list(possibleSpotsList, sudoku, returnHint = TRUE)))

      if(length(hintList) == 0){
        cat("Sorry, not a single hint could be generated")
        break
      }

      if(length(hintList) == 1){
        plot(sudoku)
        plot(hintList[[h]])
        printHint(hintList[[h]])
      } else {
        cat(paste0("This stratagy resulted in ", length(hintList), " hints."))
        h <- 1
        continue <- "y"
        while(h < length(hintList) && continue == "y"){
          plot(sudoku)
          plot(hintList[[h]])
          printHint(hintList[[h]])

          continue <- readline(prompt = "next hint? (y for yes, n for no): ")

          while(continue %!in% c("y", "n")){
            continue <- readline("You made a mistake, you can only enter y or n, \n please try again:")
          }
          h <- h + 1
        }
      }
      start <- ifelse("y" == readline("Do you want to generate a new hint? (y for yes, any other key for no): ",
                               yes = TRUE,
                               no = FALSE))
    }
  }
  return(sudoku)
}

