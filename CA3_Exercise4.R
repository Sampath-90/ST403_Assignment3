
#Question3

# 3.1
df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))

#3.2
nll_lm <- function(data, par) {
  # Extract response variable and predictor variables
  y <- data$y
  X <- cbind(1, as.matrix(data[, -1]))  # Add a column of ones for the intercept
  
  # Calculate residuals
  epsilon <- y - X %*% par
  
  # Calculate negative log-likelihood
  nll <- -sum(dnorm(epsilon, mean = 0, sd = sqrt(var(epsilon)), log = TRUE))
  
  return(nll)
}

nll_lm(df, c(0,0,0,0))

#3.3
lower_bounds <- c(-1000, -100, -100, -100) 
upper_bounds <- c(1000, 100, 100, 100)  

initial_guess <- c(mean(df$y),0,0,0)

# Use optim to find MLE
result <- optim(
  par = initial_guess,
  fn = nll_lm,
  data = df,
  lower = lower_bounds,
  upper = upper_bounds,
  method = "L-BFGS-B"  # You can choose a different optimization method if needed
)

# Extract the estimated parameters
beta_hat <- result$par
sigma_hat <- sqrt(var(df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat))

cat("Estimated Coefficients (beta):", beta_hat, "\n")
cat("Estimated Residual Standard Deviation (sigma):", sigma_hat, "\n")

#3.4
cat("optim() minimize functions by default. Since we want to find maximum likelihood we minimize the negative likelihood instead.")

#3.5

# Calculate coefficients using matrix operations
X <- cbind(1, as.matrix(df[, -1]))  # Add a column of ones for the intercept
y <- df$y

# Using optim result
beta_optim <- result$par

# Using beta_LS3 function
beta_LS3 <- function(X, y) {
  solve(crossprod(X), crossprod(X, y))
}
beta_ls3 <- beta_LS3(X, y)

# Compare coefficients
comparison <- data.frame(
  Parameter = c("Intercept", "beta_x1", "beta_x2", "beta_x3"),
  Coef_optim = beta_optim,
  Coef_ls3 = beta_ls3
)

print(comparison)

#3.6
# Using optim result
sigma_optim <- sigma_hat

# Using matrix operations
residuals <- df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat
sigma_matrix <- sqrt(var(residuals))

# Compare standard deviations
cat("Estimated Residual Standard Deviation (sigma) - optim:", sigma_optim, "\n")
cat("Estimated Residual Standard Deviation (sigma) - matrix operations:", sigma_matrix, "\n")

#3.8
hessian <- suppressWarnings(vcov(lm(y ~ x1 + x2 + x3, data = df)))

# Calculate standard errors
se <- sqrt(diag(hessian))

# Report the standard errors
cat("Standard Errors of Coefficients (beta):", se, "\n")


##additional part in q4
# Fit a linear model using lm()
lm_model <- lm(y ~ x1 + x2 + x3, data = df)

# Extract estimated coefficients (beta)
beta_lm <- coef(lm_model)

# Extract estimated residual standard deviation parameter (sigma)
sigma_lm <- summary(lm_model)$sigma

# Print the results
cat("Estimated Coefficients (beta) from lm():", beta_lm, "\n")
cat("Estimated Residual Standard Deviation (sigma) from lm():", sigma_lm, "\n")