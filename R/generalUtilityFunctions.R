

# Get a list of every box
boxSeperator <- function(x){
  boxes <- vector(mode = "list", length = 9)
  for(i in c(1,4,7)){
    for(j in c(1,4,7)){
      boxNumber <-  i  + rep((0:2), each = 3)[j]
      names(boxes)[boxNumber] <- paste("box", boxNumber)
      boxes[[boxNumber]] <- lapply(x, '[', i:(i+2), j:(j+2))

    }
  }
  return(boxes)
}

#This is a "not in" function
`%!in%` <- Negate(`%in%`)


#Function to transfrom lists to matrixes
listToMatrix <- function(possibleSpots, ...){
  UseMethod("listToMatrix")
}
listToMatrix.default <- function(x){
  stop("default not available")
}
listToMatrix.possibleSpotsVector <- function(possibleSpots, rowNumbering = NULL){
  if("list" %!in% class(possibleSpots)){
    stop("object must also have the list class")
  }
  output <- matrix(NA, ncol = 9, nrow = length(possibleSpots))
  for(i in 1:length(possibleSpots)){
    output[i,] <- possibleSpots[[i]]
  }
  rownames(output) <- rowNumbering
  return(output)
}
listToMatrix.index <- function(index, nCol){
  if("list" %!in% class(index)){
    stop("object must also have the list class")
  }
  out <- matrix(NA, ncol = nCol)
  for(i in (1:9)[sapply(index, nrow) > 0]){
    out <- rbind(out, index[[i]])

  }
  return(out[-1,])

}

#Get all the unique combinations of a set.
uniqueCombinations <- function(x, m){
  n <- length(x)
  e <- 0
  h <- m
  a <- seq_len(m)
  r <- x[a]
  out <- matrix(r, nrow = choose(n, m), ncol = m, byrow = TRUE)
  i <- 2
  nmmp1 <- n - m + 1
  while(a[1] != nmmp1) {
    if(e < n - h) {
      h <- 1
      e <- a[m]
      j <- 1
    } else {
      e <- a[m - h]
      h <- h + 1
      j <- 1:h
    }
    a[m - h + j] <- e + j
    r <-  x[a]
    out[i, ] <- r
    i <- i + 1
  }
  return(out)
}


# This function gets all the possible numbers in a certain spot.
possibleNumbersInSpotFinder <- function(index, x){
  possibleNumbers <-  sapply(x, "[", index[1], index[2])
  if(any(possibleNumbers)){
    return((1:9)[possibleNumbers])
  } else {
    return(FALSE)
  }
}

# Looks for identical values.
identicalValue <- function(x,y){
  if (identical(x,y)){
    TRUE
  } else {
    FALSE
  }
}


# This function shows what cells are influenced (or seen) by one or multiple cells.
cellInfluenceShower <- function(cells){
  #If input isn't a matrix, change it to a matrix
  if(!("matrix"  %in% class(cells))){
    cells <-  matrix(cells, ncol = 2, byrow = F)
  }
  #Get the index of the box start
  box <- c(row = rep(c(1,4,7), each = 3)[cells[1,1]], col = rep(c(1,4,7), each = 3)[cells[1,2]])
  #Create a matrix of all indexes influenced by a cell.
  cellsInfluenceNew <- rbind(cbind(row = rep(1:3, each = 3), col = rep(1:3,3)) + rep(box, each = 9) - 1,
                             cbind(rep(cells[1,1], 9), 1:9),
                             cbind(1:9, rep(cells[1,2], 9)))
  #Remove duplicates
  cellsInfluence <- cellsInfluenceNew[!duplicated(cellsInfluenceNew),]
  #Remove the considered cell
  #cellsInfluence <- cellsInfluenceNew[!apply(cellsInfluenceNew, 1, identical, y = cells[1,]), ]
  #When there are more cells to be considerd, find what cells are influenced by all cells.
  if(nrow(cells) > 1){
    for(i in 2:nrow(cells)){
      box <- c(row = rep(c(1,4,7), each = 3)[cells[i,1]], col = rep(c(1,4,7), each = 3)[cells[i,2]])
      cellsInfluenceNew <- rbind(cbind(row = rep(1:3, each = 3), col = rep(1:3,3)) + rep(box, each = 9) - 1,
                                 cbind(rep(cells[i,1], 9), 1:9),
                                 cbind(1:9, rep(cells[i,2], 9)))
      cellsInfluenceNew <- cellsInfluenceNew[!duplicated(cellsInfluenceNew), , drop = FALSE]
      #cellsInfluenceNew <- cellsInfluenceNew[!apply(cellsInfluenceNew, 1, identical, y = cells[i,]), ]
      cellsInfluence <- rbind(cellsInfluence, cellsInfluenceNew)
      cellsInfluence <- cellsInfluence[duplicated(cellsInfluence), , drop = FALSE]

    }
  }

  #Remove inputs cells

  if(nrow(cellsInfluence) > 0){
    for(i in 1:nrow(cells)){
      cellsInfluence <- cellsInfluence[!(cellsInfluence[,1] %in% cells[i, 1] & cellsInfluence[,2] %in% cells[i, 2]), , drop = FALSE]
    }

  }

  if(nrow(cellsInfluence) == 0){
    return(FALSE)
  } else {
    return(cellsInfluence)
  }
}

# A function to create give a index the class "index"
createIndexClass <- function(index){
  class(index) <- "index"
  #class(index) <- c("numeric", "index")
  return(index)
}

# Change a matrix of indexes to a list of indexes.
indexMatrix2List <- function(index){
  lapply(lapply(seq_len(nrow(index)), function(i) index[i,]), createIndexClass)
}



