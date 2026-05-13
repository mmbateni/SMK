# Sample data
# x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# y <- c(10, 9, 8, 7, 6.5, 6.5, 5.5, 4.5, 4.5, 3)
#
# Apply the function
# ktaub(x, y)

# Define a function to calculate Kendall's tau-b and Sen's slope
ktaub <- function(x, y) {

  # ---- Input validation ----
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("x and y must not have missing values")
  }

  # Sort the data by x values
  data <- data.frame(x, y)
  data <- data[order(data$x), ]

  # Number of pairs
  n      <- nrow(data)
  npairs <- n * (n - 1) / 2

  # Concordant and discordant pairs
  concordant <- 0L
  discordant <- 0L
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff_x <- data$x[j] - data$x[i]
      diff_y <- data$y[j] - data$y[i]
      if (diff_x * diff_y > 0) {
        concordant <- concordant + 1L
      } else if (diff_x * diff_y < 0) {
        discordant <- discordant + 1L
      }
    }
  }

  # ---- Tie correction: group-based formula ----
  # For each group of t tied values, the correction term is t*(t-1)/2.
  # sum(duplicated()) only counts (group_size - 1) per group, which
  # under-counts when groups have 3+ tied values and produces incorrect tau-b.
  tie_correction <- function(v) {
    tbl <- as.numeric(table(v))      # size of every tied group
    sum(tbl * (tbl - 1) / 2)        # sum of t_i*(t_i-1)/2 over all groups
  }

  ties_x <- tie_correction(data$x)
  ties_y <- tie_correction(data$y)

  # Kendall's tau-b
  denom <- sqrt((npairs - ties_x) * (npairs - ties_y))
  tau_b <- if (denom > 0) (concordant - discordant) / denom else 0

  # ---- Sen's slope ----
  # Collect all pairwise slopes; skip pairs with identical x to avoid Inf/NaN.
  slopes <- numeric(0)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dx <- data$x[j] - data$x[i]
      if (dx != 0) {
        slopes <- c(slopes, (data$y[j] - data$y[i]) / dx)
      }
    }
  }

  sen_slope <- if (length(slopes) > 0) median(slopes) else NA_real_

  list(tau_b = tau_b, sen_slope = sen_slope)
}
