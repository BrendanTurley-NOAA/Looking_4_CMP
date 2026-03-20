# -------------------------------------------------------------------------
# Truncated von Bertalanffy Growth Model (vBGM) Estimation
# Based on Diaz, Porch, and Ortiz (2004) - SEDAR7-AW-01
# -------------------------------------------------------------------------

#' Negative Log-Likelihood for Truncated vBGM
#' 
#' @param params A vector of initial parameter guesses: c(Linf, k, t0, sigma)
#' @param data A data frame with columns: 'age' (years), 'length' (mm), 'min_size' (mm)
truncated_vbgm_nll <- function(params, data) {
  # Extract parameters
  Linf  <- params[1]
  k     <- params[2]
  t0    <- params[3]
  sigma <- params[4]
  
  # Prevent standard deviation from becoming negative during optimization
  if(sigma <= 0) return(1e9) 
  
  # Predict average length at age (von Bertalanffy equation)
  # Eq 4: mean_l_a = Linf * (1 - exp(-k * (a - t0)))
  l_pred <- Linf * (1 - exp(-k * (data$age - t0)))
  
  # Numerator: Normal probability density of the observed length
  # Numerator of Eq 3 in the paper
  pdf_val <- dnorm(data$length, mean = l_pred, sd = sigma)
  
  # Denominator: Probability that the fish is larger than the minimum size limit
  # 1 - P(l < M_i | a) -> Denominator of Eq 3
  # Note: The paper notes that data without size limits should have M = 0 assigned to them.
  cdf_val <- pnorm(data$min_size, mean = l_pred, sd = sigma)
  prob_retained <- 1 - cdf_val
  
  # Add small buffer to avoid log(0) errors during optimization
  pdf_val <- ifelse(pdf_val <= 0, 1e-10, pdf_val)
  prob_retained <- ifelse(prob_retained <= 0, 1e-10, prob_retained)
  
  # Calculate log-likelihood for each observation
  log_likelihood <- log(pdf_val) - log(prob_retained)
  
  # Return the negative log-likelihood sum
  return(-sum(log_likelihood))
}

#' Fit the Truncated vBGM
#' 
#' @param data Data frame with 'age', 'length', and 'min_size'
#' @param init_params Vector of starting values c(Linf, k, t0, sigma)
fit_truncated_growth <- function(data, init_params = c(Linf = 1000, k = 0.2, t0 = 0, sigma = 50)) {
  
  # Ensure the data contains the required columns
  if(!all(c("age", "length", "min_size") %in% colnames(data))) {
    stop("Data must contain 'age', 'length', and 'min_size' columns.")
  }
  
  # Optimize the parameters using the Nelder-Mead method
  fit <- optim(
    par = init_params,
    fn = truncated_vbgm_nll,
    data = data,
    method = "Nelder-Mead",
    hessian = TRUE # Useful for calculating standard errors later
  )
  
  # Compile the results
  results <- list(
    Linf = fit$par[1],
    k = fit$par[2],
    t0 = fit$par[3],
    sigma = fit$par[4],
    convergence = fit$convergence,
    NLL = fit$value
  )
  
  return(results)
}


# 1. Create dummy data to test the function
# (e.g., A mix of restricted commercial landings and unrestricted juvenile surveys)
sample_data <- data.frame(
  age = c(1.5, 2.5, 3.0, 4.0, 5.5, 0.8, 1.2),
  length = c(380, 450, 500, 600, 680, 200, 250),
  min_size = c(381, 381, 381, 381, 381, 0, 0) # 0 for no size limit
)

# 2. Estimate the parameters
model_results <- fit_truncated_growth(sample_data, init_params = c(900, 0.2, 0.3, 60))

# 3. View the results
print(model_results)



# -------------------------------------------------------------------------
# Truncated von Bertalanffy Growth Model with Constant CV
# Based on Diaz, Porch, and Ortiz (2004) - SEDAR7-AW-01
# -------------------------------------------------------------------------

#' Negative Log-Likelihood for Truncated vBGM (Constant CV)
#' 
#' @param params A vector of initial parameter guesses: c(Linf, k, t0, CV)
#' @param data A data frame with columns: 'age', 'length', 'min_size'
truncated_vbgm_cv_nll <- function(params, data) {
  # Extract parameters
  Linf  <- params[1]
  k     <- params[2]
  t0    <- params[3]
  CV    <- params[4]
  
  # Prevent CV from becoming negative or zero during optimization
  if(CV <= 0) return(1e9) 
  
  # Predict average length at age
  l_pred <- Linf * (1 - exp(-k * (data$age - t0)))
  
  # Calculate age-specific standard deviation using the constant CV
  # sigma_a = CV * mean_l_a
  sigma_pred <- CV * l_pred
  
  # Ensure standard deviation is strictly positive to prevent dnorm/pnorm errors
  sigma_pred <- ifelse(sigma_pred <= 0, 1e-5, sigma_pred)
  
  # Numerator: Normal probability density of the observed length
  pdf_val <- dnorm(data$length, mean = l_pred, sd = sigma_pred)
  
  # Denominator: Probability that the fish is larger than the minimum size limit
  cdf_val <- pnorm(data$min_size, mean = l_pred, sd = sigma_pred)
  prob_retained <- 1 - cdf_val
  
  # Add small buffer to avoid log(0) errors during optimization
  pdf_val <- ifelse(pdf_val <= 0, 1e-10, pdf_val)
  prob_retained <- ifelse(prob_retained <= 0, 1e-10, prob_retained)
  
  # Calculate log-likelihood for each observation
  log_likelihood <- log(pdf_val) - log(prob_retained)
  
  # Return the negative log-likelihood sum
  return(-sum(log_likelihood))
}

#' Fit the Truncated vBGM with Constant CV
#' 
#' @param data Data frame with 'age', 'length', and 'min_size'
#' @param init_params Vector of starting values c(Linf, k, t0, CV)
fit_truncated_growth_cv <- function(data, init_params = c(Linf = 1000, k = 0.15, t0 = 0, CV = 0.15)) {
  
  # Ensure the data contains the required columns
  if(!all(c("age", "length", "min_size") %in% colnames(data))) {
    stop("Data must contain 'age', 'length', and 'min_size' columns.")
  }
  
  # Optimize the parameters using the Nelder-Mead method
  fit <- optim(
    par = init_params,
    fn = truncated_vbgm_cv_nll,
    data = data,
    method = "Nelder-Mead",
    hessian = TRUE # Useful if you want to extract standard errors later
  )
  
  # Compile the results
  results <- list(
    Linf = fit$par[1],
    k = fit$par[2],
    t0 = fit$par[3],
    CV = fit$par[4],
    convergence = fit$convergence,
    NLL = fit$value
  )
  
  return(results)
}


# 1. Create dummy data to test the function
sample_data <- data.frame(
  age = c(1.5, 2.5, 3.0, 4.0, 5.5, 0.8, 1.2),
  length = c(380, 450, 500, 600, 680, 200, 250),
  min_size = c(381, 381, 381, 381, 381, 0, 0) # 0 for unrestricted survey data
)

# 2. Estimate the parameters (Notice the 4th parameter is now a CV guess)
model_results_cv <- fit_truncated_growth_cv(sample_data, init_params = c(Linf = 900, k = 0.2, t0 = 0.3, CV = 0.16))

# 3. View the results
print(model_results_cv)
