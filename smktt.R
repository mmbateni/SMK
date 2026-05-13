smktt <- function(datain, alpha, wantplot, StartSeason = 1) {

  # ===========================================================================
  # AdjustSeasons: re-index season numbers so that StartSeason becomes 1
  # ===========================================================================
  AdjustSeasons <- function(datain, startSeason) {
    if (startSeason != 1) {
      season  <- unique(datain[, 2])
      maxseas <- max(season)
      delta   <- maxseas - startSeason + 1
      seasL   <- datain[, 2] <  startSeason
      seasU   <- datain[, 2] >= startSeason
      datain[seasU, 2] <- datain[seasU, 2] - (startSeason - 1)
      datain[seasL, 2] <- datain[seasL, 2] + delta
      datain[seasU, 1] <- datain[seasU, 1] + 1
    }
    datain <- seasonAverage(datain)
    return(datain)
  }

  # ===========================================================================
  # seasonAverage: collapse multiple observations in the same year-season cell
  #                to their median
  # ===========================================================================
  seasonAverage <- function(datain) {
    dte   <- as.Date(paste(datain[, 1], datain[, 2], "01", sep = "-"))
    data  <- cbind(dte, datain)
    n     <- nrow(data)
    sumobs <- NULL
    data2  <- matrix(0, nrow = length(unique(data[, 1])), ncol = 4)
    cnt    <- 2
    cntavg <- 0

    if (data[2, 1] != data[1, 1]) {
      cntavg        <- cntavg + 1
      data2[cntavg, ] <- data[1, ]
    } else {
      sumobs <- data[1, 4]
      data2  <- matrix(0, nrow = 0, ncol = 4)
    }

    while (cnt < n) {
      if (data[cnt, 1] == data[cnt + 1, 1]) {
        sumobs <- c(sumobs, data[cnt, 4])
        fl     <- 1
      } else if ((data[cnt, 1] != data[cnt + 1, 1]) &&
                 (data[cnt, 1] == data[cnt - 1, 1])) {
        sumobs <- c(sumobs, data[cnt, 4])
        fl     <- 0
      } else {
        sumobs <- data[cnt, 4]
        fl     <- 0
      }
      obsavg <- median(sumobs)
      if (fl == 0) {
        cntavg          <- cntavg + 1
        data2[cntavg, ] <- c(data[cnt, 1:3], obsavg)
        sumobs          <- NULL
      }
      cnt <- cnt + 1
    }

    if (data[cnt, 1] == data[cnt - 1, 1]) {
      sumobs <- c(sumobs, data[cnt, 4])
    } else {
      sumobs <- data[cnt, 4]
    }

    obsavg          <- median(sumobs)
    cntavg          <- cntavg + 1
    data2[cntavg, ] <- c(data[cnt, 1:3], obsavg)
    datain          <- data2[, 2:4]
    return(datain)
  }

  # ===========================================================================
  # ktaub (embedded, full implementation)
  #
  # Computes Kendall's tau-b, Sen's slope, S statistic, and variance of S for
  # a single season's data matrix (columns: year, season, value).
  #
  # Fix applied vs. stub: complete implementation replacing the empty body.
  # Fix applied vs. standalone ktaub.R: uses the correct group-based tie
  # correction  sum(t_i*(t_i-1)/2)  instead of  sum(duplicated()),  which
  # under-counts ties in groups of size >= 3 and yields inaccurate tau-b.
  # ===========================================================================
  ktaub <- function(data, alpha) {
    x <- data[, 1]   # years
    y <- data[, 3]   # observed values
    n <- length(x)

    if (n < 2) {
      return(list(S = 0, D = 1, Dall = 1, var = 0, nvar = 0,
                  tau_b = 0, sen = NA_real_))
    }

    npairs     <- n * (n - 1) / 2
    concordant <- 0L
    discordant <- 0L

    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        dy <- y[j] - y[i]
        if (!is.na(dy)) {
          if      (dy > 0) concordant <- concordant + 1L
          else if (dy < 0) discordant <- discordant + 1L
        }
      }
    }

    S <- concordant - discordant

    # Group-based tie correction: sum of t_i*(t_i-1)/2 over all tied groups.
    tie_corr <- function(v) {
      tbl <- as.numeric(table(v))
      sum(tbl * (tbl - 1) / 2)
    }
    ties_x <- tie_corr(x)
    ties_y <- tie_corr(y)

    D    <- sqrt((npairs - ties_x) * (npairs - ties_y))
    Dall <- npairs
    tau_b <- if (D > 0) S / D else 0

    # Full variance of S (Kendall 1975; Helsel & Hirsch 2002)
    # Includes cross-product correction terms for ties in both x and y.
    tx <- as.numeric(table(x))
    ty <- as.numeric(table(y))
    tx <- tx[tx > 1]
    ty <- ty[ty > 1]

    term1 <- n * (n - 1) * (2 * n + 5)
    if (length(tx) > 0) term1 <- term1 - sum(tx * (tx - 1) * (2 * tx + 5))
    if (length(ty) > 0) term1 <- term1 - sum(ty * (ty - 1) * (2 * ty + 5))
    varS <- term1 / 18

    if (n > 2 && length(tx) > 0 && length(ty) > 0) {
      varS <- varS +
        (sum(tx * (tx - 1) * (tx - 2)) * sum(ty * (ty - 1) * (ty - 2))) /
        (9 * n * (n - 1) * (n - 2)) +
        (sum(tx * (tx - 1)) * sum(ty * (ty - 1))) /
        (2 * n * (n - 1))
    }

    # Naive variance (no tie corrections) — used for comparison
    nvarS <- n * (n - 1) * (2 * n + 5) / 18

    # Sen's slope: median of all pairwise slopes (skip pairs with same x)
    slopes <- numeric(0)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        dx <- x[j] - x[i]
        if (dx != 0) slopes <- c(slopes, (y[j] - y[i]) / dx)
      }
    }
    sen_slope <- if (length(slopes) > 0) median(slopes, na.rm = TRUE) else NA_real_

    list(S = S, D = D, Dall = Dall, var = varS, nvar = nvarS,
         tau_b = tau_b, sen = sen_slope)
  }

  # ===========================================================================
  # serialAdjusted (embedded, full implementation)
  #
  # Computes the serial-correlation-adjusted p-value using the Hirsch & Slack
  # (1984) approach.
  #
  # Fixes applied vs. stub:
  #   1. Complete implementation replacing the empty body.
  #   2. Added triu() helper — base R has no such function.
  #   3. Fixed rowRanks() call: 'preserve' is not a valid argument;
  #      correct argument is preserveShape = TRUE.
  #   4. Fixed R2 sweep: original used sweep(matrix(ncol(X), nrow=nyr), 2, Ravg)
  #      which applies margin-2 sweep to a single-column matrix — wrong shape.
  #      Replaced with sweep(R, 1, Ravg) to centre each row by its mean rank.
  # ===========================================================================
  serialAdjusted <- function(data, SumVars, ss, alpha) {
    library(matrixStats)

    # Build year × season matrix
    years   <- sort(unique(data[, 1]))
    seasons <- sort(unique(data[, 2]))
    nyr     <- length(years)
    nsea    <- length(seasons)

    X <- matrix(NA_real_, nrow = nyr, ncol = nsea)
    rownames(X) <- as.character(years)
    colnames(X) <- as.character(seasons)
    for (k in seq_len(nrow(data))) {
      ri        <- which(years   == data[k, 1])
      ci        <- which(seasons == data[k, 2])
      X[ri, ci] <- data[k, 3]
    }
    X[is.na(X)] <- 0

    # triu: creates an (nyr-1) × (nyr-1) upper-triangular matrix from a
    # length-nyr vector v for pairwise cross-year sign comparisons.
    #   k=0: M[g,h] = v[g]   for h >= g   (value at earlier year g)
    #   k=1: M[g,h] = v[h+1] for h >= g   (value at later year h+1)
    # => sign(triu(v,1) - triu(v,0))[g,h] = sign(v[h+1] - v[g])
    triu <- function(v, k = 0L) {
      m <- length(v) - 1L
      M <- matrix(0, nrow = m, ncol = m)
      for (g in seq_len(m)) {
        for (h in g:m) {
          M[g, h] <- if (k == 0L) v[g] else v[h + 1L]
        }
      }
      M
    }

    # B[g,h] = sum over seasons of sign(X[h+1, s] - X[g, s])
    B <- matrix(0, nrow = nyr - 1, ncol = nyr - 1)
    for (i in seq_len(nsea)) {
      A           <- sign(triu(X[, i], 1L) - triu(X[, i], 0L))
      A[is.na(A)] <- 0
      B           <- B + A
    }

    R           <- matrixStats::rowRanks(X, preserveShape = TRUE,
                                         ties.method = "average")
    R[is.na(R)] <- 0

    # Centre ranks by row mean; RR = sum of squared centred ranks
    Ravg  <- rowMeans(R, na.rm = TRUE)
    R_dev <- sweep(R, 1L, Ravg, FUN = "-")
    RR    <- sum(R_dev^2, na.rm = TRUE)

    ng   <- colSums(X != 0)
    ngh  <- sum(ng + 1)
    ngh2 <- sum((ng + 1)^2)

    var_s    <- sum(B^2 - (B != 0))
    var_R    <- ngh^2 - ngh2
    n        <- nyr

    sigma_gh <- (var_s + 4 * RR - n * var_R) / 3
    VarSmod  <- SumVars + sigma_gh
    sigmaMod <- sqrt(abs(VarSmod))

    if (ss == 0) {
      sigAdj <- pnorm(abs(1 / sigmaMod), lower.tail = FALSE) * 2
    } else {
      sigAdj <- pnorm(abs(ss / sigmaMod), lower.tail = FALSE) * 2
    }

    return(sigAdj)
  }

  # ===========================================================================
  # Main body
  # ===========================================================================

  datain <- AdjustSeasons(datain, StartSeason)
  m      <- nrow(datain)
  n_cols <- ncol(datain)

  # Use missing() instead of exists() — wantplot is a formal argument, so
  # exists("wantplot") is always TRUE inside the function body.
  if (missing(wantplot)) wantplot <- 0

  if (n_cols >= 3) {
    sorteds <- datain[order(datain[, 1], datain[, 2]), , drop = FALSE]
    Seasons <- sort(unique(datain[, 2]))
  } else {
    stop("There is a problem in the structure of the input data.")
  }

  NumOfSeasons <- length(Seasons)
  nyears       <- max(datain[, 1]) - min(datain[, 1]) + 1
  baseyear     <- min(datain[, 1]) - 1   # kept for potential downstream use

  # Initialise per-season accumulation vectors
  S_vec     <- numeric(NumOfSeasons)
  D_vec     <- numeric(NumOfSeasons)
  Dall_vec  <- numeric(NumOfSeasons)
  vars_vec  <- numeric(NumOfSeasons)
  nvars_vec <- numeric(NumOfSeasons)
  sens_vec  <- numeric(NumOfSeasons)
  minn      <- nyears   # will track the minimum number of years across seasons

  for (ii in seq_len(NumOfSeasons)) {
    data_s <- sorteds[sorteds[, 2] == Seasons[ii], , drop = FALSE]
    ns     <- nrow(data_s)         # number of years with data for this season

    if (ns < minn) minn <- ns      # track minimum sample size across seasons

    if (ns < 3) {
      # Too few observations to compute meaningful statistics
      S_vec[ii]     <- 0
      D_vec[ii]     <- 1
      Dall_vec[ii]  <- 1
      vars_vec[ii]  <- 0
      nvars_vec[ii] <- 0
      sens_vec[ii]  <- NA_real_
      next
    }

    res           <- ktaub(data_s, alpha)
    S_vec[ii]     <- res$S
    D_vec[ii]     <- res$D
    Dall_vec[ii]  <- res$Dall
    vars_vec[ii]  <- res$var
    nvars_vec[ii] <- res$nvar
    sens_vec[ii]  <- res$sen
  }

  S     <- S_vec
  D     <- D_vec
  Dall  <- Dall_vec
  vars  <- vars_vec
  nvars <- nvars_vec
  sens  <- sens_vec

  # Aggregate statistics across all seasons
  Ss      <- sum(S)
  taubsea <- sum(S) / sum(D)
  tausea  <- sum(S) / sum(Dall)
  Sigmas  <- sqrt(sum(vars))
  nSigmas <- sqrt(sum(nvars))
  Sens    <- median(sens, na.rm = TRUE)
  SumVars <- sum(vars)

  # Continuity correction (applied when fewest-data season has < 10 years)
  ss <- Ss
  if (minn < 10) {
    if (Ss > 0) {
      ss <- Ss - 1
    } else if (Ss == 0) {
      ss <- 0
    } else if (Ss < 0) {
      ss <- Ss + 1
    } else if (is.na(Ss)) {
      stop("This function cannot process NaNs. Please remove records with NaNs.")
    }
    if (Ss == 1) {
      cat("\nSMKTT Message: When n-years for a season is less than 10 and S=1,")
      cat("\n              Continuity correction is setting S = 0.")
      cat("\n              This will affect calculated significance.\n")
    }
  }

  # ---- Test for homogeneity of trends across seasons ----
  # Chi-square statistic (Hirsch & Slack 1984):
  #   chi2 = sum(S_i^2 / Var_i) - (sum S_i)^2 / sum(Var_i)
  # Under H0 of a homogeneous trend, chi2 ~ chi^2(NumOfSeasons - 1).
  safe_vars     <- pmax(vars, 1e-10)
  chi_sq        <- sum(S^2 / safe_vars) - Ss^2 / max(sum(vars), 1e-10)
  chi_df        <- NumOfSeasons - 1
  p_homogeneity <- if (chi_df > 0) pchisq(chi_sq, df = chi_df,
                                           lower.tail = FALSE) else NA_real_

  # ---- Significance tests ----
  z_stat  <- if (Sigmas > 0) ss / Sigmas else 0
  p_value <- 2 * pnorm(-abs(z_stat))

  # Serial-correlation-adjusted p-value (wrapped so a failure does not abort)
  p_serial <- tryCatch(
    serialAdjusted(datain, SumVars, ss, alpha),
    error = function(e) {
      warning("serialAdjusted: ", conditionMessage(e))
      NA_real_
    }
  )

  # ---- Optional trend plot ----
  # wantplot = 1 draws the annual-median time series with the Sen's slope line.
  if (!is.na(wantplot) && wantplot == 1) {
    years_u  <- sort(unique(datain[, 1]))
    vals_ann <- tapply(datain[, 3], datain[, 1], median, na.rm = TRUE)

    # Sen's slope line: passes through (median_year, median_value)
    x_med     <- median(years_u)
    y_med     <- median(vals_ann, na.rm = TRUE)
    intercept <- y_med - Sens * x_med

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    plot(years_u, vals_ann,
         type = "b", pch = 16, col = "steelblue", lwd = 1.5,
         xlab = "Year", ylab = "Value",
         main = "Seasonal Mann-Kendall Trend Test")

    abline(a = intercept, b = Sens, col = "red", lwd = 2, lty = 2)

    legend("topleft", bty = "n",
           legend = c(
             "Annual median",
             sprintf("Sen's slope = %.4f", Sens),
             sprintf("p = %.4f  (z = %.3f)", p_value, z_stat)
           ),
           col = c("steelblue", "red", NA),
           lty = c(1, 2, NA), pch = c(16, NA, NA))
  }

  # ---- Return results ----
  list(
    Ss             = Ss,            # overall Mann-Kendall S statistic
    S_seasons      = S,             # per-season S statistics
    tau_b          = taubsea,       # Kendall's tau-b (with tie correction)
    tau            = tausea,        # Kendall's tau (no tie correction)
    Sens           = Sens,          # Sen's slope (median across all seasons)
    z              = z_stat,        # standardised Z statistic
    p_value        = p_value,       # two-tailed p-value
    p_serial       = p_serial,      # serial-correlation-adjusted p-value
    p_homogeneity  = p_homogeneity, # p-value for between-season trend homogeneity
    chi_sq         = chi_sq,        # chi-square statistic for homogeneity test
    Sigmas         = Sigmas,        # sqrt(sum of seasonal variances)
    NumOfSeasons   = NumOfSeasons,
    nyears         = nyears
  )
}
