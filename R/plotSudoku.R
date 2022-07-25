
#' Function to plot the Sudoku.
#'
#' This function takes an object with the S3 Sudoku class and plots it as a sudoku.
#'
#' @param sudoku A Sudoku object
#' @param ... Further argument passed to or from other methods.
#'
#' @export
plot.sudoku <- function(sudoku, ...){
  NextMethod("plot", sudoku)

  plot(0,type= "n", xlim = c(0,18), ylim = c(0,18), xlab = "", ylab = "", axes = FALSE) #empty plot

  for(i in c(0,3,6,9)*2){
    lines(x = c(i,i), y = c(0,18), lwd = 2)
    lines(x = c(0,18), y = c(i,i), lwd = 2)
  }
  for(i in c(1,2,4,5,6,7,8)*2){
    lines(x = c(i,i), y = c(0,18), lwd = 1)
    lines(x = c(0,18), y = c(i,i), lwd = 1)
  }
  for(i in 1:9){
    for(j in 1:9){
      if(!is.na(sudoku[i, j])){
        text(seq(1, 17, 2)[j],seq(17,1,-2)[i],sudoku[i,j])
      }
    }
  }
}

# Function to plot a index by creating a colered box around it.
plot.index <- function(box, col = "red"){
  lines(x = c(box[2] - 1, box[2])*2,
        y = rep((9:1)[box[1]], 2) * 2,
        col = col,lwd = 2)
  lines(x = c(box[2] - 1, box[2])*2,
        y =  rep((9:1)[box[1]] -1 , 2) * 2,
        col = col,lwd = 2)
  lines(x =  rep(box[2], 2) * 2,
        y =  c((9:1)[box[1]] -1 , (9:1)[box[1]]) * 2,
        col = col,lwd = 2)
  lines(x =  rep(box[2] -1 , 2) * 2,
        y =  c((9:1)[box[1]] -1 , (9:1)[box[1]]) * 2,
        col = col,lwd = 2)
}

# A bunch of functions to plot the different hints.
plot.ywing <- function(hint){
  plot(hint$cellIndexes$start, col = "green")
  plot(hint$cellIndexes$wing1, col = "yellow")
  plot(hint$cellIndexes$wing2, col = "yellow")
  for(i in 1:length(hint$changedCellIndex)){
    plot(hint$changedCellIndex[[i]], col = "red")
  }
}

plot.wing <- function(hint){
  for(i in 1:length(hint$index)){
    plot(hint$index[[i]], col = "green")
  }
  for(i in 1:length(hint$ommissionIndex)){
    plot(hint$ommissionIndex[[i]], col = "red")
  }
}

plot.nakedPair <- function(hint){
  for(i in 1:hint$nPair){
    plot(hint$index[[i]], col = "green")
  }
  chandedSpots <-  unlist(hint$removedPossibleSpots, recursive=FALSE)
  for(i in 1:length(chandedSpots)){
    plot(chandedSpots[[i]], col = "red")
  }
}

plot.hiddenPair <- function(hint){
  for(i in 1:hint$nPair){
    plot(hint$index[[i]], col = "green")
  }
}

plot.pointingPair <- function(hint){
  for(i in 1:hint$nPair){
    plot(hint$index[[i]], col = "green")
  }
  nOmmission <- length(hint$omissionIndex)

  for(i in 1:nOmmission){
    plot(hint$omissionIndex[[i]], col = "red")
  }
}

plot.boxLineReducted <- function(hint){
  for(i in 1:length(hint$index)){
    plot(hint$index[[i]], col = "green")
  }
  for(i in 1:length(hint$omissionIndex)){
    plot(hint$omissionIndex[[i]], col = "red")
  }
}

plot.hiddenSingle <- function(hint){
  plot(hint$index, col = "green")
}

plot.loneSingle <- function(hint){
  plot(hint$index, col = "green")
}

plot.skyscraper <- function(hint){
  for(i in 1:2){
    plot(hint$skyscraperBase[[i]], col = "green")
  }
  for(i in 1:length(hint$skyscraperTop)){
    plot(hint$skyscraperTop[[i]], col = "yellow")
  }
  for(i in 1:length(hint$ommittedCells)){
    plot(hint$ommittedCells[[i]], col = "red")
  }
}





