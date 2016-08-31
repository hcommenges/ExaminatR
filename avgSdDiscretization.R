

avgsd <- function(vec, nbcl){
  if(length(vec) < nbcl) {
    stop("Arrête tout")
  } else{
    minVec <- min(vec, na.rm = TRUE)
    maxVec <- max(vec, na.rm = TRUE)
    avgVec <- mean(vec, na.rm = TRUE)
    sdVec <- sqrt(sum((vec - avgVec) ^ 2) / length(vec[!is.na(vec)])) # écart-type population
    evenClass <- nbcl %% 2 == 0
    nbSd <- floor((nbcl - 1) / 2)
    seqNbSd <- seq(-nbSd, nbSd, by = 1)
    seqSd <- seqNbSd[seqNbSd != 0] * sdVec
    
    if(avgVec + min(seqSd) < minVec | avgVec + max(seqSd) > maxVec){
      stop("Attention tu vas m'énerver")
    } else {
      if(evenClass){
        brks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec, avgVec + seqSd[(nbSd+1):length(seqSd)], maxVec)
      } else {
        brks <- c(minVec, avgVec + seqSd[1:nbSd], avgVec + seqSd[(nbSd+1):length(seqSd)], maxVec)
      }
      return(brks[!is.na(brks)])
    }
  }
}

vec <- c(5, 8, 3, 6)
avgsd(vec = iris$Sepal.Length, nbcl = 3)
