# # Sample data
# x <- c(1,2,3,4,5,6,7,8,9,10)
# y <- c(10,9,8,7,6.5,6.5,5.5,4.5,4.5,3)
# 
# Apply the function
# ktaub(x,y)
# Define a function to calculate Kendall's tau-b and Sen's slope
ktaub <- function(x, y) {
  # Check the input vectors
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("x and y must not have missing values")
  }
  
  # Sort the data by x values
  data <- data.frame(x, y)
  data <- data[order(data$x), ]
  
  # Calculate the number of pairs
  n <- nrow(data)
  npairs <- n * (n - 1) / 2
  
  # Calculate the number of concordant and discordant pairs
  concordant <- 0
  discordant <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      diff_x <- data$x[j] - data$x[i]
      diff_y <- data$y[j] - data$y[i]
      if (diff_x * diff_y > 0) {
        concordant <- concordant + 1
      } else if (diff_x * diff_y < 0) {
        discordant <- discordant + 1
      }
    }
  }
  
  # Calculate the number of ties in x and y
  ties_x <- sum(duplicated(data$x))
  ties_y <- sum(duplicated(data$y))
  
  # Calculate Kendall's tau-b
  tau_b <- (concordant - discordant) / sqrt((npairs - ties_x) * (npairs - ties_y))
  
  # Calculate Sen's slope
  slopes <- numeric()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      slopes <- c(slopes, (data$y[j] - data$y[i]) / (data$x[j] - data$x[i]))
    }
  }
  
  # Use the median of slopes as Sen's slope
  sen_slope <- median(slopes)
  
  # Return a list of tau-b and sen_slope
  list(tau_b = tau_b, sen_slope = sen_slope)
}
