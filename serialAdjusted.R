serialAdjusted <- function(data, SumVars, ss, alpha) {
  
  library(matrixStats)
  baseyear <- min(data[,1]) - 1
  
  X <- as.matrix(data[,3])
  rownames(X) <- data[,1] - baseyear
  colnames(X) <- data[,2]
  
  X[is.na(X)] <- 0
  
  nsea <- ncol(X)
  nyr <- nrow(X)
  
  B <- matrix(0, nrow=nyr-1, ncol=nyr-1)
  
  for(i in 1:nsea) {
    A1 <- triu(X[,i])
    A2 <- triu(X[,i], 1)
    A <- sign(A2 - A1)
    A[is.na(A)] <- 0
    B <- B + A
  }
  
  R <- matrixStats::rowRanks(X, preserve = TRUE)
  R[is.na(R)] <- 0
  
  Ravg <- rowMeans(R, na.rm=TRUE) 
  R2 <- sweep(matrix(ncol(X), nrow=nyr), 2, Ravg, FUN="-")
  R3 <- R2 + R
  RR <- sum(rowSums(R3^2) - rowSums(R3)^2)
  
  ng <- colSums(!is.na(X))
  ngh <- sum(ng + 1)
  ngh2 <- sum((ng + 1)^2)
  
  var_s <- sum(B^2 - (B != 0)) 
  cov_R <- RR
  var_R <- ngh^2 - ngh2
  n <- nyr
  
  sigma_gh <- (var_s + 4*cov_R - n*var_R)/3
  VarSmod <- SumVars + sigma_gh
  sigmaMod <- sqrt(VarSmod)
  
  if(ss == 0) {
    sigAdj <- pnorm(abs(1/sigmaMod), lower.tail=FALSE)*2
  } else {
    sigAdj <- pnorm(abs(ss/sigmaMod), lower.tail=FALSE)*2
  }
  
  return(sigAdj)
  
}