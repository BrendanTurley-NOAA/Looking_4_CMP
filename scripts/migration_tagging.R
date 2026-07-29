### initial prompt: Determination of Fish Movement Patterns from Tag Recoveries using Maximum Likelihood Estimators
# reponse: Determination of Fish Movement Patterns from Tag Recoveries using Maximum Likelihood Estimators is a seminal 1990 fisheries science paper authored by Ray Hilborn. It established a robust mathematical framework for calculating spatial migration, natural mortality, and fishing vulnerability from mark-recapture data.Core MethodologyThe framework relies on a unified system structured into four primary components:Population Dynamics and Movement Model: A discrete space/time simulation (often based on a Markovian exchange process) tracking how tagged cohorts redistribute across distinct spatial zones while experiencing fishing and natural mortality.Observation Model: A model accounting for how tags are physically collected, incorporating human variables like reporting rates, handling mortality, and physical tag shedding.Likelihood Function: A statistical equation (frequently using a Poisson, multinomial, or negative binomial distribution) that evaluates the probability of the observed tag returns against the model's parameters.Nonlinear Optimization: A computer algorithm designed to adjust parameters systematically, locating the absolute peak (maximum likelihood) where the model best matches real-world data.Key Scientific ContributionsDecoupling Mortality from Movement: Prior methods struggled to determine whether a drop in tag returns meant fish migrated away or simply died. Hilborn’s model resolves this by analyzing spatial fishing effort concurrently.Handling Reporting Variation: The paper highlights how varying tag reporting rates among different fishing fleets or regions skew data, providing a mathematical template to control for these errors.Practical Management Impact: Originally applied to Skipjack Tuna populations, this methodology is widely utilized by modern bodies like the Food and Agriculture Organization (FAO) and the Indian Ocean Tuna Commission (IOTC) to set sustainable harvest quotas.If you are building or studying a spatial mark-recapture model, let me know if you would like to explore the specific mathematical equations used, the handling of tag-shedding assumptions, or how modern models adapt this framework for individual-based recapture data.


# Historical data matrices
# Rows: Release Area (1, 2) | Columns: Recovery Area (1, 2)
observed_recoveries <- matrix(c(120, 35,   # Released in 1: 120 stayed, 35 moved to 2
                                15,  95),  # Released in 2: 15 moved to 1, 95 stayed
                              nrow = 2, byrow = TRUE)

tags_released <- c(500, 400) # [Area 1 releases, Area 2 releases]
fishing_mortality <- c(0.3, 0.4) # Local fishing efforts (F) per area
natural_mortality <- 0.2 # Baseline natural mortality (M)
reporting_rate <- 0.75 # Fraction of caught tags actually reported


# The Objective Function to Minimize (Negative Log-Likelihood)
hilborn_mle_loss <- function(parameters) {
  
  # Extract movement probabilities from the parameter vector
  # m12: prob of moving 1 -> 2 | m21: prob of moving 2 -> 1
  m12 <- parameters[1]
  m21 <- parameters[2]
  
  # Bound constraints manually to keep probabilities valid (0 to 1)
  if (m12 < 0 || m12 > 1 || m21 < 0 || m21 > 1) return(Inf)
  
  # Step 1: Compute Area Transition Matrices
  # Rows: Origin | Columns: Destination
  transition_matrix <- matrix(c(1 - m12,     m12,
                                m21, 1 - m21), 
                              nrow = 2, byrow = TRUE)
  
  # Step 2: Population Dynamics & Survival
  # Total survival rate per area: exp(-(M + F))
  survival_rates <- exp(-(natural_mortality + fishing_mortality))
  
  # Step 3: Observation Model (Calculate Expected Recoveries)
  expected_recoveries <- matrix(0, nrow = 2, ncol = 2)
  
  for(i in 1:2) { # Release area loop
    for(j in 1:2) { # Recovery area loop
      # Expected = Released * Prob of Moving * Prob of Surviving * Catch Rate * Reporting Rate
      catch_rate <- fishing_mortality[j] / (natural_mortality + fishing_mortality[j])
      exploitation <- (1 - survival_rates[j]) * catch_rate
      
      expected_recoveries[i, j] <- tags_released[i] * 
        transition_matrix[i, j] * 
        exploitation * 
        reporting_rate
    }
  }
  
  # Step 4: Poisson Log-Likelihood Calculation
  # Avoid zeros or negative expected values disrupting logarithms
  expected_recoveries[expected_recoveries <= 0] <- 1e-6 
  
  # Negative log-likelihood calculation (Hilborn 1990 recommended Poisson)
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  
  return(neg_log_lik)
}



# Initial guesses for movement rates (e.g., 10% chance of swapping areas)
initial_guesses <- c(m12 = 0.1, m21 = 0.1)

# Run optimization using Nelder-Mead (or "L-BFGS-B" for strict box bounds)
fit_results <- optim(par = initial_guesses, 
                     fn = hilborn_mle_loss, 
                     hessian = TRUE) # Hessian yields variance/confidence intervals

# Print the Maximum Likelihood Estimates
print(fit_results$par)



#### generalize for x number of areas

# Define number of areas
X <- 3  

# Observed recoveries: Rows = Release Area, Columns = Recovery Area
observed_recoveries <- matrix(c(150,  30,  10,   # Released in 1
                                20, 110,  25,   # Released in 2
                                15,  15, 130),  # Released in 3
                              nrow = X, byrow = TRUE)

tags_released <- c(600, 500, 550)       # Total tags released per area
fishing_mortality <- c(0.25, 0.35, 0.30) # F per area
natural_mortality <- 0.2                  # M (assumed uniform here)
reporting_rate <- 0.80                   # Lambda

# Total parameters to estimate = X * (X - 1)
# For X = 3, we estimate 6 movement parameters (all transitions except staying)
num_params <- X * (X - 1)

# Generate flat initial guesses (e.g., 5% probability for any transition)
# initial_guesses <- rep(0.05, num_params)
# Generate a safe initial guess so row totals do not exceed 100%
safe_start_value <- 0.1 / (X - 1) 
initial_guesses <- rep(safe_start_value, num_params)

hilborn_generalized_mle <- function(parameters, X, tags_released, fishing_mortality, natural_mortality, reporting_rate, observed_recoveries) {
  
  # 1. Reject invalid probability parameter boundaries (must be between 0 and 1)
  if (any(parameters < 0) || any(parameters > 1)) return(Inf)
  
  # 2. Reconstruct the X x X Transition Matrix from the parameter vector
  transition_matrix <- matrix(0, nrow = X, ncol = X)
  param_index <- 1
  
  for (i in 1:X) {
    for (j in 1:X) {
      if (i != j) {
        transition_matrix[i, j] <- parameters[param_index]
        param_index <- param_index + 1
      }
    }
    # Diagonal element: Probability of staying in area i
    transition_matrix[i, i] <- 1 - sum(transition_matrix[i, -i])
    
    # Biological constraint: You cannot export more than 100% of the population
    if (transition_matrix[i, i] < 0) return(Inf)
  }
  
  # 3. Apply Population Dynamics and Survival Equations
  survival_rates <- exp(-(natural_mortality + fishing_mortality))
  expected_recoveries <- matrix(0, nrow = X, ncol = X)
  
  for (i in 1:X) {     # Loop over Release Areas
    for (j in 1:X) {   # Loop over Recovery Areas
      
      # Catch rate equation under competing risks of mortality
      catch_rate <- fishing_mortality[j] / (natural_mortality + fishing_mortality[j])
      exploitation <- (1 - survival_rates[j]) * catch_rate
      
      # Combined Model: Releases * Movement * Exploitation * Reporting
      expected_recoveries[i, j] <- tags_released[i] * 
        transition_matrix[i, j] * 
        exploitation * 
        reporting_rate
    }
  }
  
  # 4. Calculate Poisson Negative Log-Likelihood
  # expected_recoveries[expected_recoveries <= 0] <- 1e-6
  # Clamp very small or zero numbers to bypass log(0) calculation errors
  expected_recoveries[expected_recoveries < 1e-10] <- 1e-10
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  
  if (is.na(neg_log_lik) || is.infinite(neg_log_lik)) return(1e10) 
  
  return(neg_log_lik)
}


# Assign readable names to parameters for the output summary (e.g., "from1_to2")
param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    if (i != j) param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}
names(initial_guesses) <- param_names

# Execute optimization bounded between 0 and 1
generalized_fit <- optim(par = initial_guesses, 
                         fn = hilborn_generalized_mle, 
                         X = X,
                         tags_released = tags_released,
                         fishing_mortality = fishing_mortality,
                         natural_mortality = natural_mortality,
                         reporting_rate = reporting_rate,
                         observed_recoveries = observed_recoveries,
                         method = "Nelder-Mead",
                         # lower = rep(0, num_params),
                         # upper = rep(1, num_params),
                         # lower = rep(1e-6, num_params),       
                         # upper = rep(1 - 1e-6, num_params),   
                         hessian = TRUE)

# View results
print(generalized_fit$par)


# # 1. Verify the optimization engine converged successfully
# if (generalized_fit$convergence == 0) {
#   
#   # 2. Extract estimated movement parameter values
#   estimates <- generalized_fit$par
#   num_params <- length(estimates)
#   
#   # 3. Invert the Hessian matrix to obtain Variance-Covariance matrix
#   # optim returns the negative log-likelihood Hessian, so its inverse is directly the Vcov
#   vcov_matrix <- solve(generalized_fit$hessian)
#   
#   # 4. Extract standard errors from the diagonal variance values
#   # Any negative value on the diagonal indicates optimization instability
#   diag_vals <- diag(vcov_matrix)
#   diag_vals[diag_vals < 0] <- NA
#   standard_errors <- sqrt(diag_vals)
#   
#   # 5. Compute Upper and Lower 95% Confidence Bounds
#   z_score <- 1.96
#   lower_bounds <- estimates - (z_score * standard_errors)
#   upper_bounds <- estimates + (z_score * standard_errors)
#   
#   # 6. Apply logical boundary constraints (probabilities cannot exceed)
#   lower_bounds <- pmax(0, lower_bounds)
#   upper_bounds <- pmin(1, upper_bounds)
#   
#   # 7. Compile everything into a clean summary table
#   summary_table <- data.frame(
#     Estimate = round(estimates, 4),
#     StdError = round(standard_errors, 4),
#     Lower_95CI = round(lower_bounds, 4),
#     Upper_95CI = round(upper_bounds, 4)
#   )
#   
#   # Print the final compiled results
#   print(summary_table)
#   
# } else {
#   stop("Optimization did not converge. Hessian inversion is unreliable.")
# }

if (generalized_fit$convergence == 0) {
  estimates <- generalized_fit$par
  vcov_matrix <- solve(generalized_fit$hessian)
  
  diag_vals <- diag(vcov_matrix)
  diag_vals[diag_vals < 0] <- NA # Protect against mathematical instability
  standard_errors <- sqrt(diag_vals)
  
  z_score <- 1.96
  lower_bounds <- pmax(0, estimates - (z_score * standard_errors))
  upper_bounds <- pmin(1, estimates + (z_score * standard_errors))
  
  summary_table <- data.frame(
    Route = names(estimates),
    MLE_Estimate = round(estimates, 4),
    StdError = round(standard_errors, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  
  print(summary_table)
} else {
  cat("The optimization engine failed to converge. Error status:", generalized_fit$convergence, "\n")
}


# =========================================================================
# 1. SETUP DATA AND PARAMETERS FOR 3 AREAS
# =========================================================================
X <- 3  

observed_recoveries <- matrix(c(150,  30,  10,   # Released in 1
                                20, 110,  25,   # Released in 2
                                15,  15, 130),  # Released in 3
                              nrow = X, byrow = TRUE)

tags_released <- c(600, 500, 550)       
fishing_mortality <- c(0.25, 0.35, 0.30) 
natural_mortality <- 0.2                  
reporting_rate <- 0.80                   

num_params <- X * (X - 1)

# Logit transformation link helper functions
logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

# Generate clean initial guesses on the standard scale, then logit-transform them
safe_start_p <- 0.1 / (X - 1) 
initial_guesses_constrained <- rep(safe_start_p, num_params)
initial_guesses_unconstrained <- logit(initial_guesses_constrained)

# Name parameters
param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    if (i != j) param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}
names(initial_guesses_unconstrained) <- param_names

# =========================================================================
# 2. DEFINING THE LINKED LIKELIHOOD FUNCTION (NO MORE INF/1e10 ERRORS)
# =========================================================================
hilborn_logit_mle <- function(unconstrained_params, X, tags_released, fishing_mortality, natural_mortality, reporting_rate, observed_recoveries) {
  
  # Map unconstrained numbers (-Inf to +Inf) smoothly onto the probability scale (0 to 1)
  parameters <- inv_logit(unconstrained_params)
  
  transition_matrix <- matrix(0, nrow = X, ncol = X)
  param_index <- 1
  
  for (i in 1:X) {
    for (j in 1:X) {
      if (i != j) {
        transition_matrix[i, j] <- parameters[param_index]
        param_index <- param_index + 1
      }
    }
    # Remaining probability of fish staying in area i
    transition_matrix[i, i] <- 1 - sum(transition_matrix[i, -i])
    
    # If outward movement overflows 1.0, provide a smooth penalty gradient
    if (transition_matrix[i, i] < 0) {
      overflow <- sum(transition_matrix[i, -i]) - 1
      return(10000 + (overflow * 1e5)) # Smooth mathematical gradient back to reality
    }
  }
  
  survival_rates <- exp(-(natural_mortality + fishing_mortality))
  expected_recoveries <- matrix(0, nrow = X, ncol = X)
  
  for (i in 1:X) {     
    for (j in 1:X) {   
      catch_rate <- fishing_mortality[j] / (natural_mortality + fishing_mortality[j])
      exploitation <- (1 - survival_rates[j]) * catch_rate
      
      expected_recoveries[i, j] <- tags_released[i] * 
        transition_matrix[i, j] * 
        exploitation * 
        reporting_rate
    }
  }
  
  # Continuous scale math floor
  expected_recoveries[expected_recoveries < 1e-10] <- 1e-10
  
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  
  return(neg_log_lik)
}

# =========================================================================
# 3. RUNNING STANDARD UNCONSTRAINED OPTIMIZATION (NELDER-MEAD)
# =========================================================================
# Because parameters are unconstrained via logit, we can ditch temperamental L-BFGS-B 
# and use Nelder-Mead which never throws boundary errors.
unconstrained_fit <- optim(par = initial_guesses_unconstrained, 
                           fn = hilborn_logit_mle, 
                           X = X,
                           tags_released = tags_released,
                           fishing_mortality = fishing_mortality,
                           natural_mortality = natural_mortality,
                           reporting_rate = reporting_rate,
                           observed_recoveries = observed_recoveries,
                           method = "Nelder-Mead",
                           hessian = TRUE)

# =========================================================================
# 4. EXTRACT ESTIMATES & THE DELTA METHOD FOR CONFIDENCE INTERVALS
# =========================================================================
if (unconstrained_fit$convergence == 0) {
  
  # 1. Back-transform unconstrained parameters to final probability scale MLEs
  mle_estimates <- inv_logit(unconstrained_fit$par)
  
  # 2. Invert Hessian to get the covariance on the unconstrained scale
  vcov_unconstrained <- solve(unconstrained_fit$hessian)
  se_unconstrained <- sqrt(diag(vcov_unconstrained))
  
  # 3. Delta Method: Transform Standard Errors back to standard probability space
  # Derivative of inverse logit(x) is: exp(-x) / (1 + exp(-x))^2
  x_vals <- unconstrained_fit$par
  deriv_link <- exp(-x_vals) / (1 + exp(-x_vals))^2
  se_probability_scale <- se_unconstrained * deriv_link
  
  # 4. Compute 95% Confidence Bounds
  z_score <- 1.96
  lower_bounds <- pmax(0, mle_estimates - (z_score * se_probability_scale))
  upper_bounds <- pmin(1, mle_estimates + (z_score * se_probability_scale))
  
  summary_table <- data.frame(
    Route = names(mle_estimates),
    MLE_Estimate = round(mle_estimates, 4),
    StdError = round(se_probability_scale, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  
  print(summary_table)
  
} else {
  cat("Optimization failed to converge. Status:", unconstrained_fit$convergence, "\n")
}



# =========================================================================
# UNIFIED MULTI-SPATIAL FISH MOVEMENT ESTIMATOR (3-AREA GRADIENT MODEL)
# =========================================================================
X <- 3  

observed_recoveries <- matrix(c(150,  30,  10,   
                                20, 110,  25,   
                                15,  15, 130),  
                              nrow = X, byrow = TRUE)

tags_released <- c(600, 500, 550)       
fishing_mortality <- c(0.25, 0.35, 0.30) 
natural_mortality <- 0.2                  
reporting_rate <- 0.80                   

num_params <- X * (X - 1)

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

safe_start_p <- 0.1 / (X - 1) 
initial_guesses_constrained <- rep(safe_start_p, num_params)
initial_guesses_unconstrained <- logit(initial_guesses_constrained)

param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    if (i != j) param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}
names(initial_guesses_unconstrained) <- param_names

hilborn_logit_mle <- function(unconstrained_params, X, tags_released, fishing_mortality, natural_mortality, reporting_rate, observed_recoveries) {
  parameters <- inv_logit(unconstrained_params)
  transition_matrix <- matrix(0, nrow = X, ncol = X)
  param_index <- 1
  
  for (i in 1:X) {
    for (j in 1:X) {
      if (i != j) {
        transition_matrix[i, j] <- parameters[param_index]
        param_index <- param_index + 1
      }
    }
    transition_matrix[i, i] <- 1 - sum(transition_matrix[i, -i])
    
    if (transition_matrix[i, i] < 0) {
      overflow <- sum(transition_matrix[i, -i]) - 1
      return(10000 + (overflow * 1e5)) 
    }
  }
  
  survival_rates <- exp(-(natural_mortality + fishing_mortality))
  expected_recoveries <- matrix(0, nrow = X, ncol = X)
  
  for (i in 1:X) {     
    for (j in 1:X) {   
      catch_rate <- fishing_mortality[j] / (natural_mortality + fishing_mortality[j])
      exploitation <- (1 - survival_rates[j]) * catch_rate
      expected_recoveries[i, j] <- tags_released[i] * transition_matrix[i, j] * exploitation * reporting_rate
    }
  }
  
  expected_recoveries[expected_recoveries < 1e-10] <- 1e-10
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  return(neg_log_lik)
}

unconstrained_fit <- optim(par = initial_guesses_unconstrained, 
                           fn = hilborn_logit_mle, 
                           X = X,
                           tags_released = tags_released,
                           fishing_mortality = fishing_mortality,
                           natural_mortality = natural_mortality,
                           reporting_rate = reporting_rate,
                           observed_recoveries = observed_recoveries,
                           method = "Nelder-Mead",
                           hessian = TRUE)

if (unconstrained_fit$convergence == 0) {
  mle_estimates <- inv_logit(unconstrained_fit$par)
  vcov_unconstrained <- solve(unconstrained_fit$hessian)
  se_unconstrained <- sqrt(diag(vcov_unconstrained))
  
  x_vals <- unconstrained_fit$par
  deriv_link <- exp(-x_vals) / (1 + exp(-x_vals))^2
  se_probability_scale <- se_unconstrained * deriv_link
  
  z_score <- 1.96
  lower_bounds <- pmax(0, mle_estimates - (z_score * se_probability_scale))
  upper_bounds <- pmin(1, mle_estimates + (z_score * se_probability_scale))
  
  summary_table <- data.frame(
    Route = names(mle_estimates),
    MLE_Estimate = round(mle_estimates, 4),
    StdError = round(se_probability_scale, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  print(summary_table)
} else {
  cat("Optimization failed to converge. Status:", unconstrained_fit$convergence, "\n")
}

# prompt: fix the following code to include estimates for migration from and to the same location (i.e., the diagonal for the transition matrix):
# response: To include estimates for the diagonal of the transition matrix (fish staying in the same location, i.e., \(i = j\)), the script needs to transition from a binary logit transformation (which only tracks binary movement vs. staying) to a multinomial logit framework (Softmax).Using a row-wise Softmax ensures that all \(X \times X\) paths are structurally constrained to sum to \(1\) across every row, while freeing the optimization from boundary errors (\(<0\) or \(>1\)). This allows the Hessian matrix to capture the variances and standard errors for the diagonal elements directly via the Delta method or direct probability transformation.Here is the corrected and optimized R code:

# =========================================================================
# UNIFIED MULTI-SPATIAL FISH MOVEMENT ESTIMATOR (FULL UNCONSTRAINED MATRIX)
# =========================================================================
X <- 3
observed_recoveries <- matrix(c(150, 30, 10, 
                                20, 110, 25, 
                                15,  15, 130), nrow = X, byrow = TRUE)
tags_released <- c(600, 500, 550)
fishing_mortality <- c(0.25, 0.35, 0.30)
natural_mortality <- 0.2
reporting_rate <- 0.80

# Track all X * X parameters explicitly
num_params <- X * X
initial_guesses_constrained <- rep(1 / X, num_params)
# Use log transformation to move to unconstrained space for row-wise softmax
initial_guesses_unconstrained <- log(initial_guesses_constrained)

# Construct parameter names for all routes, including self-migration
param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}
names(initial_guesses_unconstrained) <- param_names

hilborn_softmax_mle <- function(unconstrained_params, X, tags_released, fishing_mortality, natural_mortality, reporting_rate, observed_recoveries) {
  # Reshape flat vector back into a matrix
  param_matrix <- matrix(unconstrained_params, nrow = X, ncol = X, byrow = TRUE)
  
  # Compute row-wise Softmax to guarantee rows sum to 1
  transition_matrix <- t(apply(param_matrix, 1, function(row) {
    exp_row <- exp(row - max(row)) # Subtract max to prevent numerical overflow
    return(exp_row / sum(exp_row))
  }))
  
  survival_rates <- exp(-(natural_mortality + fishing_mortality))
  expected_recoveries <- matrix(0, nrow = X, ncol = X)
  
  for (i in 1:X) {
    for (j in 1:X) {
      catch_rate <- fishing_mortality[j] / (natural_mortality + fishing_mortality[j])
      exploitation <- (1 - survival_rates[j]) * catch_rate
      expected_recoveries[i, j] <- tags_released[i] * transition_matrix[i, j] * exploitation * reporting_rate
    }
  }
  
  # Protect against log(0) numerical instability
  expected_recoveries[expected_recoveries < 1e-10] <- 1e-10
  
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  return(neg_log_lik)
}

# Run optimization using Nelder-Mead
unconstrained_fit <- optim(
  par = initial_guesses_unconstrained, 
  fn = hilborn_softmax_mle, 
  X = X, 
  tags_released = tags_released, 
  fishing_mortality = fishing_mortality, 
  natural_mortality = natural_mortality, 
  reporting_rate = reporting_rate, 
  observed_recoveries = observed_recoveries, 
  method = "Nelder-Mead", 
  control = list(maxit = 5000),
  hessian = TRUE
)

if (unconstrained_fit$convergence == 0) {
  # Reconstruct final probability matrix via Softmax
  final_param_matrix <- matrix(unconstrained_fit$par, nrow = X, ncol = X, byrow = TRUE)
  mle_matrix <- t(apply(final_param_matrix, 1, function(row) {
    exp_row <- exp(row - max(row))
    return(exp_row / sum(exp_row))
  }))
  mle_estimates <- as.vector(t(mle_matrix))
  names(mle_estimates) <- param_names
  
  # Delta Method for Softmax Standard Errors
  vcov_unconstrained <- solve(unconstrained_fit$hessian)
  se_probability_scale <- rep(0, num_params)
  
  for (i in 1:X) {
    row_indices <- ((i - 1) * X + 1):(i * X)
    p_row <- mle_matrix[i, ]
    
    # Jacobian matrix for the softmax transformation of row i
    jacobian <- matrix(0, nrow = X, ncol = X)
    for (m in 1:X) {
      for (n in 1:X) {
        if (m == n) {
          jacobian[m, n] <- p_row[m] * (1 - p_row[m])
        } else {
          jacobian[m, n] <- -p_row[m] * p_row[n]
        }
      }
    }
    
    # Extract row sub-variance-covariance matrix
    vcov_sub <- vcov_unconstrained[row_indices, row_indices]
    se_row <- sqrt(diag(jacobian %*% vcov_sub %*% t(jacobian)))
    se_probability_scale[row_indices] <- se_row
  }
  
  # Compute Wald 95% Confidence Intervals bounded between 0 and 1
  z_score <- 1.96
  lower_bounds <- pmax(0, mle_estimates - (z_score * se_probability_scale))
  upper_bounds <- pmin(1, mle_estimates + (z_score * se_probability_scale))
  
  summary_table <- data.frame(
    Route = names(mle_estimates),
    MLE_Estimate = round(mle_estimates, 4),
    StdError = round(se_probability_scale, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  
  print(summary_table)
} else {
  cat("Optimization failed to converge. Status:", unconstrained_fit$convergence, "\n")
}

#Key Enhancements Made\(X \times X\) Parameter Dimensioning: Expanded parameters from X * (X - 1) to X * X to map out the full grid.Row-wise Softmax Transformation: Replaced inv_logit() with a mathematically rigorous apply(..., 1, softmax) wrapper. This handles internal allocation elegantly without relying on volatile 1 - sum(...) boundary penalty statements.Multivariate Delta Method: Implemented a structural Jacobian matrix mapping inside the error loop. Because elements inside a softmax row are mutually dependent, this correctly factors in the covariances to yield valid standard errors and confidence intervals for your diagonal elements.Would you like to adjust this script to handle unequal reporting rates across your three spatial pools, or should we incorporate tag-shedding over time?

### prompt: yes, adjust this script to handle unequal reporting rates across your three spatial pools, and incorporate tag-shedding over time
### repsonse: To incorporate unequal reporting rates across your three regions and factor in tag-shedding over time, the framework must evolve from a single snapshot estimation into a multi-period cohort tracking model.Tag-shedding is modeled as a continuous rate (φ), which behaves mathematically like an extra source of natural mortality, reducing the number of identifiable tags over time. The matrix structures expand into a 3D array format to track recoveries by Release Area (i), Recapture Area (j), and Time Step (t).Here is the updated and expanded R code:

# =========================================================================
# MULTI-SPATIAL COHORT ESTIMATOR (UNEQUAL REPORTING & TIME-SERIES SHEDDING)
# =========================================================================
X <- 3  # Number of areas
T <- 4  # Number of recovery time steps (years/seasons)

# 3D Array for observed recoveries: [Release Area i, Recapture Area j, Time Step t]
# Filled with mock data tracking declining numbers across 4 time periods
observed_recoveries <- array(0, dim = c(X, X, T))
observed_recoveries[,,1] <- matrix(c(120, 20,  5,   15, 90, 15,   10, 10, 100), nrow=X, byrow=TRUE)
observed_recoveries[,,2] <- matrix(c( 60, 15,  5,   10, 50, 10,    8,  5,  60), nrow=X, byrow=TRUE)
observed_recoveries[,,3] <- matrix(c( 30,  8,  2,    5, 25,  5,    4,  2,  35), nrow=X, byrow=TRUE)
observed_recoveries[,,4] <- matrix(c( 15,  4,  1,    2, 12,  2,    2,  1,  20), nrow=X, byrow=TRUE)

tags_released <- c(600, 500, 550)
fishing_mortality <- c(0.25, 0.35, 0.30)
natural_mortality <- 0.2

# 1. Unequal Reporting Rates: Area-specific vector
reporting_rates <- c(0.80, 0.65, 0.85) 

# 2. Tag Shedding Rate: Continuous annual shedding rate
tag_shedding_rate <- 0.05 

# Optimization setups
num_params <- X * X
initial_guesses_constrained <- rep(1 / X, num_params)
initial_guesses_unconstrained <- log(initial_guesses_constrained)

param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}
names(initial_guesses_unconstrained) <- param_names

hilborn_dynamic_mle <- function(unconstrained_params, X, T, tags_released, fishing_mortality, 
                                natural_mortality, reporting_rates, tag_shedding_rate, observed_recoveries) {
  
  param_matrix <- matrix(unconstrained_params, nrow = X, ncol = X, byrow = TRUE)
  
  # Row-wise Softmax for unconstrained optimization boundaries
  transition_matrix <- t(apply(param_matrix, 1, function(row) {
    exp_row <- exp(row - max(row))
    return(exp_row / sum(exp_row))
  }))
  
  # Total loss rate per area includes: Natural M + Fishing M + Tag Shedding
  total_loss_rate <- natural_mortality + fishing_mortality + tag_shedding_rate
  survival_rates <- exp(-total_loss_rate)
  
  expected_recoveries <- array(0, dim = c(X, X, T))
  
  # Track active cohorts remaining alive with intact tags over time
  # Dim: [Initial Release Area i, Current Area k]
  current_tags_at_large <- matrix(0, nrow = X, ncol = X)
  for (i in 1:X) {
    current_tags_at_large[i, i] <- tags_released[i]
  }
  
  # Step forward through time slices
  for (t in 1:T) {
    # Step A: Apply migration matrix to redistribute the tags at large
    redistributed_tags <- current_tags_at_large %*% transition_matrix
    
    # Step B: Calculate catches using local fishing mortality and area-specific reporting rates
    for (i in 1:X) {
      for (j in 1:X) {
        catch_rate <- fishing_mortality[j] / total_loss_rate[j]
        exploitation <- (1 - survival_rates[j]) * catch_rate
        
        # Incorporates spatial reporting_rates[j]
        expected_recoveries[i, j, t] <- redistributed_tags[i, j] * exploitation * reporting_rates[j]
      }
    }
    
    # Step C: Update tags at large surviving to the next interval step
    for (i in 1:X) {
      for (j in 1:X) {
        current_tags_at_large[i, j] <- redistributed_tags[i, j] * survival_rates[j]
      }
    }
  }
  
  expected_recoveries[expected_recoveries < 1e-10] <- 1e-10
  
  neg_log_lik <- -sum(dpois(observed_recoveries, lambda = expected_recoveries, log = TRUE))
  return(neg_log_lik)
}

# Execute MLE tracking across dimensions
unconstrained_fit <- optim(
  par = initial_guesses_unconstrained, 
  fn = hilborn_dynamic_mle, 
  X = X, T = T,
  tags_released = tags_released, 
  fishing_mortality = fishing_mortality, 
  natural_mortality = natural_mortality, 
  reporting_rates = reporting_rates, 
  tag_shedding_rate = tag_shedding_rate,
  observed_recoveries = observed_recoveries, 
  method = "Nelder-Mead", 
  control = list(maxit = 10000),
  hessian = TRUE
)

if (unconstrained_fit$convergence == 0) {
  final_param_matrix <- matrix(unconstrained_fit$par, nrow = X, ncol = X, byrow = TRUE)
  mle_matrix <- t(apply(final_param_matrix, 1, function(row) {
    exp_row <- exp(row - max(row))
    return(exp_row / sum(exp_row))
  }))
  mle_estimates <- as.vector(t(mle_matrix))
  names(mle_estimates) <- param_names
  
  vcov_unconstrained <- solve(unconstrained_fit$hessian)
  se_probability_scale <- rep(0, num_params)
  
  for (i in 1:X) {
    row_indices <- ((i - 1) * X + 1):(i * X)
    p_row <- mle_matrix[i, ]
    jacobian <- matrix(0, nrow = X, ncol = X)
    for (m in 1:X) {
      for (n in 1:X) {
        if (m == n) { jacobian[m, n] <- p_row[m] * (1 - p_row[m]) } 
        else { jacobian[m, n] <- -p_row[m] * p_row[n] }
      }
    }
    vcov_sub <- vcov_unconstrained[row_indices, row_indices]
    se_row <- sqrt(diag(jacobian %*% vcov_sub %*% t(jacobian)))
    se_probability_scale[row_indices] <- se_row
  }
  
  z_score <- 1.96
  lower_bounds <- pmax(0, mle_estimates - (z_score * se_probability_scale))
  upper_bounds <- pmin(1, mle_estimates + (z_score * se_probability_scale))
  
  summary_table <- data.frame(
    Route = names(mle_estimates),
    MLE_Estimate = round(mle_estimates, 4),
    StdError = round(se_probability_scale, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  print(summary_table)
} else {
  cat("Optimization failed to converge. Status:", unconstrained_fit$convergence, "\n")
}

# Key Mathematical ChangesTime-Series Cohort Projections: Added a time loop (for (t in 1:T)) that scales down remaining tags at large step-by-step using an explicit survival decay matrix.Vectorized Spatial Reporting: Shifted reporting_rate from a scalar value to an array-indexed vector reporting_rates[j]. This evaluates the specific reporting behavior of the fleet working in the recapture zone j.Tag Shedding Penalty Integration: Expanded total_loss_rate to equal \(M_j + F_j + \phi\). This accounts for tags that are lost due to physical detachment or structural wear before the fish can be caught and processed.Would you like to extend this script further to estimate the tag-shedding rate (φ) directly from the data as a free parameter, or should we incorporate an initial handling/tagging mortality penalty?



# =========================================================================
# MULTI-SPATIAL COHORT ESTIMATOR (RTMB OPTIMIZED VERSION)
# =========================================================================
# Ensure you have RTMB installed: install.packages("RTMB")
library(RTMB)

X <- 3  # Number of areas
T <- 4  # Number of recovery time steps

# 3D Array for observed recoveries
observed_recoveries <- array(0, dim = c(X, X, T))
observed_recoveries[,,1] <- matrix(c(120, 20,  5,   15, 90, 15,   10, 10, 100), nrow=X, byrow=TRUE)
observed_recoveries[,,2] <- matrix(c( 60, 15,  5,   10, 50, 10,    8,  5,  60), nrow=X, byrow=TRUE)
observed_recoveries[,,3] <- matrix(c( 30,  8,  2,    5, 25,  5,    4,  2,  35), nrow=X, byrow=TRUE)
observed_recoveries[,,4] <- matrix(c( 15,  4,  1,    2, 12,  2,    2,  1,  20), nrow=X, byrow=TRUE)

# Pack everything into a structured data list for RTMB
data <- list(
  X = X,
  T = T,
  observed_recoveries = observed_recoveries,
  tags_released = c(600, 500, 550),
  fishing_mortality = c(0.25, 0.35, 0.30),
  natural_mortality = 0.2,
  reporting_rates = c(0.80, 0.65, 0.85),
  tag_shedding_rate = 0.05
)

# Parameters list: Start with a matrix of 0s on the log-scale (corresponds to equal probabilities)
parameters <- list(
  param_matrix = matrix(0, nrow = X, ncol = X)
)

# Route names for final reporting
param_names <- c()
for (i in 1:X) {
  for (j in 1:X) {
    param_names <- c(param_names, paste0("from", i, "_to", j))
  }
}

# =========================================================================
# RTMB OBJECTIVE FUNCTION
# =========================================================================
f <- function(pars) {
  # This automatically unpacks 'data' and 'pars' into the function's environment
  getAll(data, pars) 
  
  nll <- 0  # Initialize negative log-likelihood
  
  # 1. Row-wise Softmax for movement probabilities
  transition_matrix <- matrix(0, nrow = X, ncol = X)
  for (i in 1:X) {
    exp_row <- exp(param_matrix[i, ])
    transition_matrix[i, ] <- exp_row / sum(exp_row)
  }
  
  # 2. Mortalities & Survival
  total_loss_rate <- natural_mortality + fishing_mortality + tag_shedding_rate
  survival_rates <- exp(-total_loss_rate)
  
  # 3. Track active cohorts over time
  current_tags_at_large <- matrix(0, nrow = X, ncol = X)
  for (i in 1:X) {
    current_tags_at_large[i, i] <- tags_released[i]
  }
  
  # 4. State Dynamics Loop
  for (t in 1:T) {
    redistributed_tags <- current_tags_at_large %*% transition_matrix
    
    for (i in 1:X) {
      for (j in 1:X) {
        catch_rate <- fishing_mortality[j] / total_loss_rate[j]
        exploitation <- (1 - survival_rates[j]) * catch_rate
        
        # Calculate expected recoveries
        expected_rec <- redistributed_tags[i, j] * exploitation * reporting_rates[j]
        
        # Accumulate likelihood (RTMB overrides dpois automatically)
        nll <- nll - dpois(observed_recoveries[i, j, t], expected_rec, log = TRUE)
        
        # Update tags surviving to next time step
        current_tags_at_large[i, j] <- redistributed_tags[i, j] * survival_rates[j]
      }
    }
  }
  
  # Tell RTMB to track standard errors for the raw probabilities vector!
  mle_estimates_vec <- as.vector(t(transition_matrix))
  ADREPORT(mle_estimates_vec)
  
  return(nll)
}

# =========================================================================
# MODEL EXECUTION & SE INFERENCE
# =========================================================================
# Create the Automatic Differentiation object
obj <- MakeADFun(f, parameters)

# Optimize using nlminb (much faster & relies on exact gradients calculated via AD)
opt <- nlminb(obj$par, obj$fn, obj$gr)

if (opt$convergence == 0) {
  # Pull the parameter and standard error estimates automatically via delta method
  rep <- sdreport(obj)
  sd_summary <- summary(rep, "report")
  
  mle_est <- sd_summary[, "Estimate"]
  se_est  <- sd_summary[, "Std. Error"]
  
  z_score <- 1.96
  lower_bounds <- pmax(0, mle_est - (z_score * se_est))
  upper_bounds <- pmin(1, mle_est + (z_score * se_est))
  
  summary_table <- data.frame(
    Route = param_names,
    MLE_Estimate = round(mle_est, 4),
    StdError = round(se_est, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4),
    row.names = NULL
  )
  
  print(summary_table)
} else {
  cat("Optimization failed to converge. Status:", opt$convergence, "\n")
}