### experimenting with change in growth over time

# Load necessary libraries (install if needed: install.packages(c("nlme", "mgcv", "segmented")))
library(nlme)  # For Mixed-Effects Models (NLME)
library(mgcv)  # For Generalized Additive Models (GAMs)
library(segmented) # For Segmented Regression

# --- Simulate Data ---
set.seed(42)
N_fish <- 2000
N_years <- 50
Start_Year <- 1980

# Generate individual IDs
ID <- 1:N_fish
# Randomly assign a birth year (Cohort)
Cohort <- sample(Start_Year:(Start_Year + N_years - 10), N_fish, replace = TRUE) 
# The Year the fish was measured (Sampling Year)
Year <- sapply(Cohort, function(c) sample(c + 1:min(10, (Start_Year + N_years - 1) - c), 1))
# Age is Year - Cohort
Age <- Year - Cohort 

# Base von Bertalanffy parameters
Linf_base <- 400
K_base <- 0.35
t0 <- 0.01

# Introduce a quadratic temporal effect on Linf (decrease then increase, as described)
Linf_effect <- 0.05 * (Cohort - mean(Cohort))^2 - 1.5 * (Cohort - mean(Cohort)) + 15
Linf_individual <- Linf_base + Linf_effect + rnorm(N_fish, 0, 10)

# Generate Length
Length <- Linf_individual[ID] * (1 - exp(-K_base * (Age - t0))) + rnorm(N_fish, 0, 5)

# Create the final data frame
data <- data.frame(
  ID = factor(rep(ID, each = 1)),
  Cohort = Cohort[ID],
  Year = Year[ID],
  Age = Age[ID],
  Length = Length
)

# Filter for positive Age (realistic data)
data <- subset(data, Age > 0)

# plot data
plot(data$Age, data$Length)

lth_yr <- aggregate(Length ~ Year + Age, data = data, mean)
plot(lth_yr$Year, lth_yr$Length, typ = 'n',
     panel.first = grid())
for(i in unique(lth_yr$Age)){
  with(subset(lth_yr, Age==i),
       points(Year, Length, typ = 'o', col = which(i==unique(lth_yr$Age))))
}
with(aggregate(Length ~ Year, data = data, mean),
     points(Year, Length, typ = 'l', lwd = 3))

plot(Length ~ Age, data=data, col=data$Cohort)

data <- subset(data, Year > 1989 & Year < 2022)

# Nonlinear Mixed-Effects Model (NLME) ------------------------------------

## Install if necessary: install.packages("nlme")
library(nlme)

# 1. Define the von Bertalanffy Growth Function (VBGF)
vbgf_formula <- Length ~ Linf * (1 - exp(-K * (Age - t0)))

# 2. Set starting values for parameters
# Let's say the simple fit gave intercepts: Linf=380, K=0.4, t0=0.05
# Use these, and still use 0 for the complex terms (linear/quadratic Cohort effects)
start_vector_refined <- c(
  380, 0, 0, # Linf: Intercept (refined), Linear (0), Quadratic (0)
  0.4, 0, 0, # K: Intercept (refined), Linear (0), Quadratic (0)
  0.05, 0, 0 # t0: Intercept (refined), Linear (0), Quadratic (0)
)
start_list_correct <- list(fixed = start_vector_refined)


# 3. Fit the NLME model:
#   - Model Linf (asymptotic length) as a function of Cohort, 
#     using a quadratic term (Cohort^2) to capture the decrease-then-increase pattern.
#   - Individual fish ID is included as a grouping factor (random effect)
# Fixed: All parameters are just intercepts (3 parameters)

# Note: poly(Cohort, 2) results in 3 coefficients (Intercept, Linear, Quadratic)
# K ~ 1 results in 1 coefficient (Intercept)
# t0 ~ 1 results in 1 coefficient (Intercept)

start_vector_simple <- c(
  400,    # Intercept for Linf
  0.35,   # Intercept for K
  0.01    # Intercept for t0
)
# This is the structure that satisfies the error message:
start_list_correct_simple <- list(fixed = start_vector_simple)

model_simple <- nlme(
  model = vbgf_formula,
  fixed = Linf + K + t0 ~ 1, 
  random = Linf ~ 1 | ID,                       
  data = data,
  groups = ~ ID,
  # Use your original simple starts: c(400, 0.35, 0.01)
  start = start_list_correct_simple  
)
# Use coef(model_simple) or fixed.effects(model_simple) to get refined intercepts

# Display results
summary(model_simple)

# Create a centered cohort variable
data$Cohort_C <- data$Cohort - mean(data$Cohort)

# Use the structure where parameters are separated by pipes '|' for different covariates
model_revised_fixed <- nlme(
  model = vbgf_formula,
  # Only Linf depends on Cohort. K and t0 are only intercepts (~1).
  fixed = Linf ~ poly(Cohort_C, 2) | K ~ 1 | t0 ~ 1,
  random = Linf ~ 1 | Cohort_C,                       
  data = data,
  groups = ~ Cohort,
  # Now only 5 fixed parameters required:
  start = list(fixed = c(Linf=400, K=0.35, t0=0, Cohort = .5)),
  # ... control arguments ...
)

# Interpretation: 
# Examine the 'Fixed effects' table. The coefficients for 'poly(Cohort, 2)'
# indicate how the Linf parameter changes with the cohort. A significant 
# quadratic term confirms the non-linear (decrease then increase) growth change 
# over the 35 years.



# 2. 🌊 Generalized Additive Model (GAM) ----------------------------------

## Install if necessary: install.packages("mgcv")
library(mgcv)

# 1. Fit the GAM model
#   - s(Age): Smooth, non-linear effect of age (the base growth curve).
#   - s(Age, by=Cohort): A smooth interaction term. This allows the shape 
#     of the Age-Length relationship (i.e., the growth rate) to change smoothly 
#     as a function of the Cohort. This directly captures change in growth pattern.
model_gam <- gam(
  Length ~ s(Age) + s(Cohort) + te(Age, Cohort), # 'te' for tensor product smooth interaction
  data = data
)

model_gam <- gam(
  Length ~ s(Age) + s(Year) + te(Age, Year), # 'te' for tensor product smooth interaction
  data = data
)

# Display results
summary(model_gam)

# 2. Visualize the Cohort effect (Change in growth pattern)
plot(model_gam, select = 4, main = "Interaction between Age and Cohort", scheme = 1)
# The plot for the te(Age, Cohort) term shows the residuals and the magnitude 
# of the effect, highlighting where the growth pattern is most different across cohorts.
plot(model_gam)

# Interpretation:
# The significance of the 'te(Age, Cohort)' term indicates that the relationship 
# between Length and Age (the growth curve) is significantly different across cohorts.


# 3. 📅 Segmented Regression (Change-Point Detection) ---------------------

## Install if necessary: install.packages("segmented")
library(segmented)

# --- Step 1: Calculate the mean Linf for each Cohort (A proxy for growth) ---
# For simplicity, we calculate a mean Length per Cohort for older fish (Age > 5)
cohort_mean_growth <- aggregate(Length ~ Cohort, 
                                data = subset(data, Age > 5), 
                                FUN = mean)

# --- Step 2: Fit a linear model of mean growth over time (Cohort) ---
lm_base <- lm(Length ~ Cohort, data = cohort_mean_growth)

# --- Step 3: Find the change-point (break-point) ---
# Estimate a model with one break-point ('psi') based on the base linear model
model_segmented <- segmented(lm_base, 
                             seg.Z = ~ Cohort, 
                             psi = list(Cohort = c(1987,2013))) # Start search near Cohort Year 2000

# Display results
summary(model_segmented)
# Estimated break-point
print(paste("Estimated Change-Point (Cohort Year):", round(model_segmented$psi[2], 0)))

# 4. Plot the result to visualize the blocks
plot(cohort_mean_growth$Cohort, cohort_mean_growth$Length, 
     pch = 19, 
     main = "Segmented Regression: Change in Mean Growth",
     xlab = "Cohort Year",
     ylab = "Mean Length (Older Fish)")
plot(model_segmented, add = TRUE, col = "red", lwd = 2)

# Interpretation:
# The output provides the estimated break-point ('psi'), which defines the transition 
# from the first growth block (Regime 1) to the second growth block (Regime 2). 
# The two different slopes (one before 'psi' and one after) quantify the change in 
# trend between the two time blocks.
