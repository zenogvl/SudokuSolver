OpenSingleEliminator <- function(Spots_mat){

  OpenSingleFinder <- function(x){
    if(length((1:9)[x == FALSE]) == 1){
      x[x == FALSE] <- TRUE
    }
    return(x)
  }


  SpotsRow_mat <- t(apply(Spots_mat, 1, OpenSingleFinder)) #Open single's row's
  SpotsCol_mat <- apply(Spots_mat, 2, OpenSingleFinder) #Open single's column's
  SpotsBox_mat <- Spots_mat
  for(i in c(1,4,7)){ #Open single's boxes
    for(j in c(1,4,7)){
      boxselect <-  Spots_mat[i:(i+2),j:(j+2)]
      if(length(boxselect[boxselect == FALSE]) == 1){
        boxselect[boxselect == FALSE] <- TRUE
      }
      SpotsBox_mat[i:(i+2),j:(j+2)] <- boxselect
    }
  }
  Update_mat <- Spots_mat + SpotsBox_mat + SpotsRow_mat + SpotsCol_mat #Combine results
  return(Update_mat > 0 & Update_mat < 4) #Return a matrix where only
}
