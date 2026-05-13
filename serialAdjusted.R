serialAdjusted <- function(data, SumVars, ss, alpha) {

  library(matrixStats)

  # ---- Build year × season matrix X ----
  baseyear <- min(data[, 1]) - 1
  years    <- sort(unique(data[, 1]))
  seasons  <- sort(unique(data[, 2]))
  nyr      <- length(years)
  nsea     <- length(seasons)

  X <- matrix(NA_real_, nrow = nyr, ncol = nsea)
  rownames(X) <- as.character(years)
  colnames(X) <- as.character(seasons)
  for (k in seq_len(nrow(data))) {
    ri        <- which(years   == data[k, 1])
    ci        <- which(seasons == data[k, 2])
    X[ri, ci] <- data[k, 3]
  }
  X[is.na(X)] <- 0

  # ---- Helper: upper-triangular pairwise sign matrix ----
  # Creates an (nyr-1) × (nyr-1) matrix from a length-nyr vector v.
  # triu(v, k=0) : entry [g,h] = v[g]   for h >= g  (lower-year value)
  # triu(v, k=1) : entry [g,h] = v[h+1] for h >= g  (upper-year value, shifted)
  # sign(triu(v,1) - triu(v,0))[g,h] = sign(v[h+1] - v[g]),
  # i.e., the sign of the difference between year (h+1) and year g.
  triu <- function(v, k = 0) {
    m <- length(v) - 1L          # output dimension = nyr - 1
    M <- matrix(0, nrow = m, ncol = m)
    for (g in seq_len(m)) {
      for (h in g:m) {
        M[g, h] <- if (k == 0L) v[g] else v[h + 1L]
      }
    }
    M
  }

  # ---- Pairwise sign-comparison matrix B (nyr-1 × nyr-1) ----
  # B[g,h] = sum over seasons of sign(X[h+1, season] - X[g, season])
  B <- matrix(0, nrow = nyr - 1, ncol = nyr - 1)
  for (i in seq_len(nsea)) {
    A1          <- triu(X[, i], k = 0)
    A2          <- triu(X[, i], k = 1)
    A           <- sign(A2 - A1)
    A[is.na(A)] <- 0
    B           <- B + A
  }

  # ---- Within-year rank statistics ----
  # preserveShape = TRUE keeps the matrix dimensions; ties.method = "average"
  # matches the standard SMK rank-based serial covariance adjustment.
  R           <- matrixStats::rowRanks(X, preserveShape = TRUE,
                                       ties.method = "average")
  R[is.na(R)] <- 0

  # Row-mean rank for each year; then centre R by subtracting the row mean.
  Ravg    <- rowMeans(R, na.rm = TRUE)
  R_dev   <- sweep(R, 1L, Ravg, FUN = "-")   # (nyr × nsea) centred-rank matrix

  # RR = sum of squared centred ranks, used in the covariance term.
  RR <- sum(R_dev^2, na.rm = TRUE)

  # ---- Hirsch-Slack (1984) serial-correlation variance adjustment ----
  ng   <- colSums(X != 0)
  ngh  <- sum(ng + 1)
  ngh2 <- sum((ng + 1)^2)

  var_s    <- sum(B^2 - (B != 0))
  cov_R    <- RR
  var_R    <- ngh^2 - ngh2
  n        <- nyr

  sigma_gh <- (var_s + 4 * cov_R - n * var_R) / 3
  VarSmod  <- SumVars + sigma_gh
  sigmaMod <- sqrt(abs(VarSmod))    # abs() guards against tiny negative rounding

  if (ss == 0) {
    sigAdj <- pnorm(abs(1 / sigmaMod), lower.tail = FALSE) * 2
  } else {
    sigAdj <- pnorm(abs(ss / sigmaMod), lower.tail = FALSE) * 2
  }

  return(sigAdj)
}
