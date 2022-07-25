
# This function finds skyscrapers.
# Search online for more explination on skyscrappers.
skyscraperFinder <- function(possibleSpotsList, sudoku, returnHint = FALSE){
  hintList <- list()

  for(numb in 1:9){

    ###    Rows
    #Get a list of the number of possabilities in every row
    nPossibleRow <- apply(possibleSpotsList[[numb]], 1, sum)

    #Stop if all the rows are filled in.
    if(sum(nPossibleRow) == 0){
      next
    }

    #Check if any row has only two possabilities
    if(any(nPossibleRow == 2)){
      indexAll <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
      #Look for a skyscraper in every row with two possabilities
      for(row in (1:9)[nPossibleRow == 2]){

        #The top of the skyscraper are other options in the column of a row with only two possabilities
        skyscraperTop <- indexAll[indexAll[,2] %in% (1:9)[possibleSpotsList[[numb]][row,]] & indexAll[, 1] != row, , drop = FALSE]
        if(nrow(skyscraperTop) < 2){
          next
        }
        # Stop if the skyscrapper is actually a xwing.
        if(length(unique(skyscraperTop[,1])) == 1){
          next
        }
        #Check if the top cells of the skycraper see any cells.
        if(is.logical(cellInfluenceShower(skyscraperTop))){
          next
        }

        #Check if the skyscraper changes any cells.
        if(any(possibleSpotsList[[numb]][cellInfluenceShower(skyscraperTop), drop = FALSE])){

          #Get the index of the changed cells
          indexOmmittedCells <- cellInfluenceShower(skyscraperTop)[possibleSpotsList[[numb]][cellInfluenceShower(skyscraperTop), drop = FALSE], , drop = FALSE]

          if(returnHint){
            #Get the index of the base of the skyscraper
            skyscraperBase <- indexAll[indexAll[, 1] == row,]

            hint <- list(number = numb,
                         direction = "row",
                         which = row,
                         skyscraperBase = indexMatrix2List(skyscraperBase),
                         skyscraperTop = indexMatrix2List(skyscraperTop),
                         ommittedCells = indexMatrix2List(indexOmmittedCells))
            class(hint) <- "skyscraper"
            hintList <- append(hintList, list(hint))
          }
          possibleSpotsList[[numb]][indexOmmittedCells] <- FALSE

        }
      }
    }

    ###    Columns

    nPossibleCol <- apply(possibleSpotsList[[numb]], 2, sum)
    if(sum(nPossibleCol) == 0){
      next
    }

    #Check if any row has only two possibilities
    if(any(nPossibleCol == 2)){
      #Look for a skyscraper in every colum with two possibilities
      for(col in (1:9)[nPossibleCol == 2]){
        indexAll <- which(possibleSpotsList[[numb]], arr.ind = TRUE)
        #The top of the skyscraper are other options in the row of a colum with only two possibilities
        skyscraperTop <- indexAll[indexAll[,1] %in% (1:9)[possibleSpotsList[[numb]][, col]] & indexAll[, 2] != col, , drop = FALSE]
        if(nrow(skyscraperTop) < 2){
          next
        }
        # Stop if the skyscrapper is actually a xwing.
        if(length(unique(skyscraperTop[,2])) == 1){
          next
        }

        #Check if the top cells of the skyscraper see any cells.
        if(is.logical(cellInfluenceShower(skyscraperTop))){
          next
        }

        #Check if the skyscraper changes any cells.
        if(any(possibleSpotsList[[numb]][cellInfluenceShower(skyscraperTop), drop = FALSE])){
          #Get the index of the changed cells
          indexOmmittedCells <- cellInfluenceShower(skyscraperTop)[possibleSpotsList[[numb]][cellInfluenceShower(skyscraperTop), drop = FALSE], , drop = FALSE]

          if(returnHint){
            #Get the index of the base of the skyscraper
            skyscraperBase <- indexAll[indexAll[, 2] == col,]

            hint <- list(number = numb,
                         direction = "column",
                         which = col,
                         skyscraperBase = indexMatrix2List(skyscraperBase),
                         skyscraperTop = indexMatrix2List(skyscraperTop),
                         ommittedCells = indexMatrix2List(indexOmmittedCells))
            class(hint) <- "skyscraper"
            hintList <- append(hintList, list(hint))
          }
          possibleSpotsList[[numb]][indexOmmittedCells] <- FALSE

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


