library(tidyverse)
library(dplyr)

## ----
## load-the-dataset
## ----
file1 <- paste0("https://raw.githubusercontent.com/",
                "mcgillstat/regression/main/data/data-table-B3.csv")
data_table_B3 <- read.csv(file = file1)
data_table_B3
# for
# demonstration,
# print the first
# six rows of the
# matrix
head(data_table_B3)
#     y  x1    x2  x3  x4   x5   x6 x7  x8   x9  x10   x11
# 1 18.90 350 165 260 8.00 2.56  4  3 200.3 69.9 3910   1
# 2 17.00 350 170 275 8.50 2.56  4  3 199.6 72.9 3860   1
# 3 20.00 250 105 185 8.25 2.73  1  3 196.7 72.2 3510   1
# 4 18.25 351 143 255 8.00 3.00  2  3 199.9 74.0 3890   1
# 5 20.07 225  95 170 8.40 2.76  1  3 194.1 71.8 3365   0
# 6 11.20 440 215 330 8.20 2.88  4  3 184.5 69.0 4215   1








# Q1----------------------------------------------------------------------------
# Fit the model y = β0 + β1(x1) + β6(x6) + ε

dat <- data.frame(
  y = data_table_B3$y, # y = Miles/gallon
  x1 = data_table_B3$x1, # x1 = Displacement
  x6 = data_table_B3$x6 # x6 = Carburetor
)

# Fit multiple linear regression model 
model_1 <- lm(y ~ x1 + x6, data = dat)
summary(model_1)

# Call:
#   lm(formula = y ~ x1 + x6, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.0623 -1.6687 -0.3628  1.6221  6.2305 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   32.884551   1.535408  21.417  < 2e-16 ***
#   x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#   x6           0.959223   0.670277   1.431    0.163    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.013 on 29 degrees of freedom
# Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
# F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10






# Q2----------------------------------------------------------------------------
# Generate ANOVA table
anova(model_1)

# Analysis of Variance Table
# Response: y
#             Df Sum Sq Mean Sq F value    Pr(>F)    
#   x1         1 955.72  955.72 105.290 3.666e-11 ***
#   x6         1  18.59   18.59   2.048    0.1631    
# Residuals   29 263.23    9.08                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# H0: β1 = β6 = 0
# Neither engine displacement (x1) nor the number of carburetor barrels (x6)
# has any linear relationship with gasoline mileage.

# H1: At least one βj ≠ 0 
# At least one of the predictors (engine displacement or carburetor barrels) 
# is useful for predicting gasoline mileage.

# Test Statistic: "F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10" from
    # fitting the model,
    # F = MSR/MSRes = (SSR/(p-1))  /  (SSRes/(n-p)), where p = 3 and n = 32.
    # Recall: MSR = SSR/(p-1) = (SST - SSRes) / (p-1)
# Since p-value 1.79e-10 is much smaller than ⍺ = 0.05, 
    # we reject the null hypothesis H0.
# There is a statistically significant linear relationship between 
    # gasoline mileage (y) and the combination of engine displacement (x1) 
    # and carburetor barrels (x6).






# Q3----------------------------------------------------------------------------
# Calculate R2 and adjusted R2 for this model. Compare this to the R2 
#   and adjusted R2 for the simple linear regression model relating mileage
#   y to engine displacement x1.

# For my own sake: How well does my model explain the data?
# R-Squared: Proportion of variance in the response variable that is predictable
#   from the independent variables.


# Extract metrics from the Multiple Linear Regression (MLR) model
r2_mlr <- summary(model_1)$r.squared # 0.7872928 
adj_r2_mlr <- summary(model_1)$adj.r.squared # 0.7726233

# Fit the Simple Linear Regression model (y ~ x1)
model_slr <- lm(y ~ x1, data = dat)
summary(model_slr)

# Extract metrics from the SLR model
r2_slr <- summary(model_slr)$r.squared # 0.7722712 
adj_r2_slr <- summary(model_slr)$adj.r.squared # 0.7646803

# MLR
r2_mlr
adj_r2_mlr

# SLR
r2_slr
adj_r2_slr

# Conclusion:
# Multiple Linear Regression (x1 + x6)
# R-Squared of MLR: 0.7872928
# Adjusted R-Squared of MLR: 0.7726233

# Simple Linear Regression (x1 only)
# R-Squared of SLR: 0.7722712 
# Adjusted R-Squared of SLR: 0.7646803

# R-Squared always increases or stays the same when we add more variables 
# Coefficient of Determination R-Squared =measures the proportion of variance 
# in the response ($y$) explained by the predictors.
# R^2 = 1 - SSRes/SST

# Adjusted R-Squared penalizes the model for adding unnecessary predictors. 
# It accounts for the degrees of freedom (n-p)
# adj R^2 = 1 - (SSRes/(n-p)) / (SST/(n-1))

# Our interpretation
# The MLR model (R-Squared = approx. 78.73%$) explains slightly more variation 
# than the SLR model (R-Squared =  approx. 77.23%$).

# The adjusted metric R-Squared also increases from SLR's approx. 76.47% 
# to MLR's approx. 77.26%
# Thus, Adding carburetor barrels (x6) to the model provided a VERY small 
# improvement in fit, but only a tiny amount. Engine displacement (x1) accounts 
# for the vast majority of the predictive power.






# Q4----------------------------------------------------------------------------

summary(model_1)$coefficients
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 32.88455083 1.535407938 21.417468 2.546135e-19
# x1          -0.05314767 0.006136843 -8.660425 1.549965e-09
# x6           0.95922305 0.670277025  1.431084 1.630948e-01

# ⍺ = 0.05
# H0: βj = 0. 
# Predictor adds no value to model given all other predictors are 
#   already included

# Ha: βj ≠ 0. Predictor is significant. 

# For x1, 
# - t-test = -8.660425 according to the table. 
# - p-value = 1.549965e-09.

# Conclusion: since p = 1.549965e-09 < ⍺ = 0.05, we reject the null hypothesis 
#   H0: βj = 0. Engine displacement makes a significant contribution to the 
#   model even when carburetor barrels (x6) are included. There is strong 
#   evidence that engine displacement is associated with gasoline mileage.

# For x6, 
# t-test = 1.431, p-value = 1.630948e-01
# Conclusion: since p = 1.630948e-01 > ⍺ = 0.05, we FAIL to reject H0.
#   The number of carburetor barrels does not make a statistically significant
#   contribution to the model GIVEN that engine displacement is already 
#   included.

#   Despite the overall model being significant, only engine displacement is a 
#   significant individual predictor. Carburetor barrels appears to be redundant
#   with the presence of engine displacement. 







# Q5----------------------------------------------------------------------------
# 95% CI for x1
confint(model_1, "x1", level = 0.95)
#         2.5 %       97.5 %
# x1  -0.06569957  -0.04059643

# Where, 100(1-⍺)$ confidence interval for regression coefficient βj is given by 
# beta_hat_j ± t_(n-p),⍺/2 * estimate standard error (ese) of beta_hat_j
# in our summary: beta_hat_j = -0.05314767, ese(beta_hat_j) = 0.006136843
# The critical value from t-distribution with n-p = 32 - 3 = 29 and ⍺/2 = 0.025
# is approx. 2.045. 

# Result: We are 95% confident that the true slope for x1 lies between 
# -0.06569957 and -0.04059643.

# Since the entire interval is negative, we are statistically certain that
#   larger engines are associated with lower gas mileage.

# For every 1 cubic inch increase in engine displacement, we estimate the car's
#   fuel efficiency will drop between 0.041 and 0.066 miles per gallon, holding
#   the numbers of carburetors constant. 





# Q6----------------------------------------------------------------------------

# H0: β1 and β6 = 0, # of carburetors and engine displacement are redundant.
# Ha: β1 and β6 != 0, they provide significant predictions for mileage. 
# Testing if a coefficient = 0, t_0 = (beta_hat_j - 0) / (ese(beta_hat_j))
# t-distribution with n-p degrees of freedom under H0.
# We reject H0 if |t_0| > t_crit (n-p, ⍺/2)
# For ⍺ = 0.05 and df = 29, critical value is approx. 2.045  

# Extract the coefficients table to see t-values
coef_summary <- summary(model_1)$coefficients
coef_summary
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 32.88455083 1.535407938 21.417468 2.546135e-19
# x1          -0.05314767 0.006136843 -8.660425 1.549965e-09
# x6           0.95922305 0.670277025  1.431084 1.630948e-01

# Get specific t-values for x1 and x6
t_beta1 <- coef_summary["x1", "t value"] # [1] -8.660425
t_beta6 <- coef_summary["x6", "t value"] # [1] 1.431084 


# degrees of freedom for critical value
df_res <- summary(model_1)$df[2]  # [1] 29

# t-statistic for x1: 
t_beta1 # [1] -8.660425
# t-statistic for x6:
t_beta6 # [1] 1.431084 
# degrees of freedom:
df_res # [1] 29


# Conclusion:
# Displacement 
# For β1 the t-value = |-8.660425| = 8.660425, thus, we reject H0 since 
#  8.660425 > 2.045.
#  There is statistically significant evidence that engine displacement 
#  contributes to the model.

# Carburetors 
# For β_6 = |1.431084| = 1.431084, we FAIL to reject H0 since 1.431084 
#   is NOT > 2.045. There is insufficient evidence to claim that the number of 
#   carburetor barrels contribute significantly to the model when displacement 
#   is already included. 







# Q7----------------------------------------------------------------------------
# Defining the new observation (mean gasoline milage when x1 = 275, x6 = 2)
new_car <- data.frame(x1 = 275, x6 = 2)

# 95% CI for Mean Response
predict(model_1, newdata = new_car, interval = "confidence", level = 0.95)

# Extract the coefficients table to see beta_hat values
coef_summary <- summary(model_1)$coefficients
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 32.88455083 1.535407938 21.417468 2.546135e-19
# x1          -0.05314767 0.006136843 -8.660425 1.549965e-09
# x6           0.95922305 0.670277025  1.431084 1.630948e-01

#     fit      lwr      upr
# 1 20.18739 18.87221 21.50257

# The point estimate is approx. 20.19 from the equation: 
# y_hat = β0 - β1(275) + 0.96(2)

# Lower bound: 18.97221
# Upper Bound: 21.50257
#   Which measures the uncertainty in the position of the regression plane 
#   (the average) at that specific point.

# Conclusion:
# We are 95% confident that the true average gasoline mileage for all cars 
#   with an engine displacement of 275 cubic inches  and 2 carburetor barrels 
#   lies between 18.97 and 21.40 Miles per Gallon (y), a range of 2.43 MPG.

#   The small gap suggests that we are very sure about that the average mileage 
#   of cars with these specs, would be approx. 20.2 Miles Per Gallon, 
#   give or take 1.2 MPG. 







# Q8----------------------------------------------------------------------------
# 95% PI for new observation when x1 = 275 and x6 = 2. Unlike CI, we use ε for 
# Prediction Intervals.

# New car's specs
new_car_data <- data.frame(x1 = 275, x6 = 2)

# 95% Prediction Interval
predict(model_1, newdata = new_car_data, interval = "prediction", level = 0.95)


# Extract the coefficients table to see beta_hat values
coef_summary <- summary(model_1)$coefficients
#                Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 32.88455083 1.535407938 21.417468 2.546135e-19
# x1          -0.05314767 0.006136843 -8.660425 1.549965e-09
# x6           0.95922305 0.670277025  1.431084 1.630948e-01

# Residual Standard Error
sigma_hat <- summary(model_1)$sigma # [1] 3.012815
df_res <- summary(model_1)$df[2]  # [1] 29

#     fit     lwr      upr
# 1 20.18739 13.8867 26.48808

# point estimate: 20.18739
# Lower Bound: 13.8867
# Upper Bound: 26.48808


# Conclusion:
#   We are 95% confident that a single specific automobile with 275 cubic inches 
#   engine displacement and 2 carburetor barrels will have a gasoline mileage 
#   between 13.89 and 26.49 Miles per Gallon, a range of 12.6 MPG.

#   The large gap in the Prediction Interval compared the Confidence Interval 
#   suggests that individual cars are highly variable. 
#   If we buy one specific car, its mileage could plausibly be
#   as low as 14 or as high as 26 which in itself is a large pool of unit. 

#   While the model is very good at identifying the average behavior,
#   it is not very precise at predicting individual outcomes 
#   because there is a lot of random variation, ε,
#   between cars that engine size alone cannot explain.






# Q9----------------------------------------------------------------------------

model <- lm (model_1) 

# Residuals and Leverage (hat values)
hat_vals <- lm.influence(model)$hat
hat_vals

# Residuals
residuals <- resid(model) 

# PRESS Residuals, predictively adjusted residuals
pressres <- resid(model)/(1 - lm.influence(model)$hat) 

# SSRes, Residual sum of squares
SSRes <- sum(residuals ^2) 
# PRESS, Predicted Residual Error Sum of Squares
PRESS <- sum(pressres ^2) 

# Total Sum of Squares, Sum of differences from the mean of y
SST <- sum((dat$y - mean(dat$y))^2)

# Out of sample R-Squared (Prediction R-Squared)
R2_prediction <- 1 - (PRESS/SST)

# Residual Sum of Squares
cat("Residual Sum of Squares (SSRes): ", SSRes) # 263.2345
cat("Predicted Residual Error Sum of Squares (PRESS): ", PRESS) # 328.7654 
cat("Total Sum of Squares (SST): ", SST) # 1237.544 
cat("Out-of-sample R-Squared (R2_prediction): ", R2_prediction) # 0.7343405


# 9a) 
# SSRes measures the error on the data the model was trained on. 
#   The model is optimized specifically to minimize this error, 
#   "fitting" itself to the specific noise in that dataset. 

# PRESS measures the error on data points held out 
#   (leave-one-out cross-validation). Because the model hasn't seen these points 
#   during fitting, it cannot overfit to them, resulting in larger errors.

# PRESS residual for point i is measured by e_(i) = e_i / 1-h_ii
#   e_i being the original residual, h_ii is the leverage of i, 
#   which is always between 0 and 1 (and typically > 0).
#   Since the denominator is less than 1, the PRESS residual is mathematically
#   guaranteed to be larger in magnitude than the ordinary residual. 
#   Therefore, the sum of squared PRESS residuals PRESS will be larger than 
#   the sum of squared ordinary residuals SSRes.


# 9b) 
# SST = 1237.544 
# Out-of-sample R-Squared: 0.7343405 (73.43%)


# 9c) 
# R-Squared of MLR: 0.7872928
# Adjusted R-Squared of MLR: 0.7726233
# Out-of-sample R-Squared: 0.7343405

# Prediction R-Squared (0.734) is very close to the Adjusted R-Squared (0.773)
#   This indicates that the model generalizes well and the performance does not
#   drop much when it faces new cars. 

# It is not suffering from severe overfitting. The model is not relying 
#   on quirks or outliers in the 32 car dataset to achieve its score.

# We can expect this model to explain about 74% of the variation in gasoline 
#   mileage for future cars. 

# If the Prediction R-Squared were significantly lower (e.g., 0.50), 
#   it would suggest the model fits the training data well but fails 
#   to predict new, unseen observations accurately.








# Q10---------------------------------------------------------------------------

set.seed(123)

# Split
dat
n <- nrow(dat)
train_indexes <- sample(1:n, size = n/2) # Random 16 indexes

train_data <- dat[train_indexes, ]
test_data  <- dat[-train_indexes, ]

# Fit the model on the TRAINING data only
model_half <- lm(y ~ x1 + x6, data = train_data)
# model_half
# Call:
# lm(formula = y ~ x1 + x6, data = train_data)
# 
# Coefficients:
# (Intercept)     x1           x6  
# 32.18472     -0.05101      0.82413 

# Compare Coefficients
cat("Full Model (n=32): ", coef(model_1), "\n") # 32.88455 -0.05314767 0.9592231 
cat("Half Model (n=16): ", coef(model_half)) #    32.18472 -0.05101269 0.824131

# Out-of-Sample R-squared on the TEST data
# Predict y values for the test set using half-model
preds <- predict(model_half, newdata = test_data)

# Sum of Squared Errors (SSE) for the test set
sse_test <- sum((test_data$y - preds)^2)

# Total Sum of Squares (SST) for the test set
sst_test <- sum((test_data$y - mean(test_data$y))^2)

# Calculate R-Squared
r2_out <- 1 - (sse_test / sst_test)

# Predictive Performance
cat("Out-of-Sample R-Squared:", r2_out) # 0.8234939


# Conclusion:

# Have the regression coefficients changed dramatically?
#   In terms of the β1, Displacement:
#   - Full Model: -0.053 
#   - Half Model: -0.05101
#   The coefficient for β1 remained relatively stable, indicating a robust 
#   relationship. However, the coefficient for carburetors (β6) changed 
#   dramatically from 0.9592231 (full model) to 0.824131 (half model). 
#   This confirms that β6 is an unstable estimator sensitive to the specific 
#   data points used.

# How well does this model predict?
#   Out-of-Sample R-Squared = 0.8234939
#   In terms of this specific metric, the model appears to predict extremely 
#   well—better, than the model trained on the full dataset. 
#   It explains 82.35% of the variance in the unseen test data.
#   However, this high performance is likely misleading and indicates instability 
#   due to the small sample size (n = 16 for training)

#   We must consider the random 16 cars for testing, which could have lined up 
#   perfectly with the trend of the other 16 cars. 
#   We must also consider the high variance. A stable model would give 
#   consistent results, but one split can give 0.82 while another can give 0.40. 
#   Thus, the model performance is heavily dependent on exactly which cars 
#   are chose for training. 

#   While the Out-of-Sample R-Squared of 0.823 suggests excellent predictive 
#   power, the fact that it exceeds the full model's fit 0.787 implies this 
#   result is an artifact of sampling variability. The small sample size, N=32, 
#   makes the model unstable; its performance fluctuates wildly depending on 
#   which observations are included in the training set.







# Q11---------------------------------------------------------------------------

# Models
m1 <- lm(y ~ 1, data = dat)          # y = β0 + ε, Intercept only
m2 <- lm(y ~ x1, data = dat)         # y = β0 + β1x1 + ε, x1 only (Displacement)
m3 <- lm(y ~ x6, data = dat)         # y = β0 + β6x6 + ε, x6 only (Carburetors)
m4 <- lm(y ~ x1 + x6, data = dat)    # y = β0 + β1x1 + β6x6 + ε, Full Model

# MSE (σ^2) from the full model for Cp calculation
mse_full <- (summary(m4)$sigma)^2 # [1] 9.077053
mse_full

# Function to calculate all metrics for a single model
get_metrics <- function(model, full_mse) {
  s <- summary(model)
  n <- nrow(dat)
  p <- length(coef(model)) # Number of parameters with intercept
  rss <- sum(resid(model)^2) # Residual Sum of Squares
  
  # Metrics
  r2 <- s$r.squared
  adj_r2 <- s$adj.r.squared
  aic <- AIC(model)
  bic <- BIC(model)
  # Mallows' Cp Formula: (RSS / MSE_full) - (n - 2*p)
  cp <- (rss / full_mse) - (n - 2 * p)
  
  return(c(p=p, R2=r2, Adj_R2=adj_r2, Cp=cp, AIC=aic, BIC=bic))
}

# Results
results <- data.frame(
  Model_1 = get_metrics(m1, mse_full),
  Model_2 = get_metrics(m2, mse_full),
  Model_3 = get_metrics(m3, mse_full),
  Model_4 = get_metrics(m4, mse_full)
)

# Transpose for easier reading
print(t(results))
#         p      R2    Adj_R2         Cp      AIC      BIC
# Model_1 1 0.0000000 0.0000000 106.337646 211.7768 214.7083
# Model_2 2 0.7722712 0.7646803   3.048003 166.4296 170.8268
# Model_3 2 0.2371662 0.2117384  76.002961 205.1139 209.5111
# Model_4 3 0.7872928 0.7726233   3.000000 166.2460 172.1089


# Conclusion:
#   We can first eliminate model 1 because it assumes neither engine size nor
#   carburetors affect gas mileage. It assumes every car simply gets the average
#   mileage, ȳ. R-Squared and Adjusted R-Squared = 0, and AIC = 211.8 which is
#   the highest score, a.k.a. the worst score. We have previously proved that 
#   engine size do matter. 

# We can then eliminate model 3 because it assumes ONLY the number of 
#   carburetors matters while engine size does not. R-Squared = 0.237 means that
#   this model only explains 24% of the difference in gas mileage. Comparing 24% 
#   to Models 2 and 4's approx. 76-77%, we can already eliminate Model 3 since 
#   it only explains approx. 24% of the difference in gas mileage. AIC is also
#   205.1139 which is the second highest (worst) score. 

# Now between Model 2 and 4:
# - In terms of R-Squared and Adjusted R-Squared, Model 4's is slightly higher. 
#   This means that model 4, technically fits the the data better.
# - Model 4's AIC is slightly lower, which suggests that its prediction accuracy
#   is a slightly better.
# - Model 4's Mallow's C_p is 3.0 which is exactly p (p = 3). This is the 
#   definition of a "unbiased" model. 
#   Model 2's Mallow's C-p = 3.048003, which is also a good fit since it's 
#   approx. 2 and is not < 2. It hits the target despite 1 less variable. 
# - The most important metric revolves around BIC. BIC applies a stronger 
#   penalty for adding variables and favor the simpler model when the 
#   improvement in fit is marginal. The lower BIC for Model 2 confirms that the 
#   extra variable is unnecessary complexity. Recall, in Q6, we confirmed the
#   the t-test for x6 to be insignificant with p = 0.163 > ⍺ (0.05). Adding it
#   barely improves the fit. 
# - Model 2 is much simpler with only 1 predictor vs. having the need for 2.


# In conclusion:
#   Although Model 4 has a slightly higher Adjusted R-Squared and lower AIC, 
#   the differences are negligible. Model 2 has the lowest BIC (170.8 vs 172.1), 
#   which penalizes complexity more strictly. Additionally, the t-test from 
#   Q6 showed thatx6 is not statistically significant. Therefore, 
#   Model 2 provides the best balance of fit and simplicity, the better choice 
#   that performs just as well as Model 4, but simpler and easier to interpret.











# Q12---------------------------------------------------------------------------
set.seed(123)

# Shuffle the dataset randomly
#    We scramble the rows so the folds are random
dat_shuffled <- dat[sample(nrow(dat)), ]

# Create 5 "Folds" (groups)
#    This assigns a number (1, 2, 3, 4, or 5) to every row
folds <- cut(seq(1, nrow(dat_shuffled)), breaks = 5, labels = FALSE)

# Create a generic function to run CV on any model formula
calc_cv_error <- function(formula_input) {
  mse_vector <- numeric(5) # A place to store the 5 errors
  
  for(i in 1:5) {
    # Define Test and Train for this fold
    testIndices <- which(folds == i, arr.ind = TRUE)
    testData    <- dat_shuffled[testIndices, ]
    trainData   <- dat_shuffled[-testIndices, ]
    
    # fit the model using lm() on TRAINING data
    model <- lm(formula_input, data = trainData)
    
    # Predict on TEST data
    preds <- predict(model, newdata = testData)
    
    # Calculate MSE for this fold: mean((Actual - Predicted)^2)
    mse_vector[i] <- mean((testData$y - preds)^2)
  }
  
  # e) Return the average of the 5 MSEs
  return(mean(mse_vector))
}

# Run the function for all 4 models
cv_error_1 <- calc_cv_error(y ~ 1)          # Model 1
cv_error_2 <- calc_cv_error(y ~ x1)         # Model 2
cv_error_3 <- calc_cv_error(y ~ x6)         # Model 3
cv_error_4 <- calc_cv_error(y ~ x1 + x6)    # Model 4
cat("Manual CV Error Model 1:", cv_error_1, "\n") # 40.38771 
cat("Manual CV Error Model 2:", cv_error_2, "\n") # 10.5516
cat("Manual CV Error Model 3:", cv_error_3, "\n") # 32.77996
cat("Manual CV Error Model 4:", cv_error_4, "\n") # 9.76937 

# In terms of the different models, we can already eliminate Model 1 and 3 due 
#   to their high error results. Simply using the average and the number of 
#   carburetors are weak predictors. 
# Based on the 5-fold cross validation, Model 4 is the selected model because
#   it minimizes the estimated test Mean Squared Error and shows the smallest
#   cross-validation error result. 
#   This suggests that the carburetor variable, while not the most statistically 
#   significant in hypothesis testing, likely provides a very small amount of 
#   useful signal that improves prediction accuracy on unseen data just enough 
#   to outperform the simpler model.
# 
# In Q11, we preferred Model 2 because BIC and Parsimony favored simplicity, 
#   and the difference in fit, R-Squared, was negligible. Model 2 is better for 
#   explanation, while Model 4 is slightly better for pure prediction.
# 
