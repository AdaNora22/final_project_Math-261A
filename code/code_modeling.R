# import needed libraries
library(dplyr)
library(tidyr)
library(lubridate)
# install.packages("glmnet")
library(glmnet)
# install.packages("car")
library(car)
# install.packages("DescTools")
library(DescTools)
# install.packages("MASS")
library(MASS)


# Load data
unemployment_data <- read.csv("/Users/noraadadurova/Desktop/Math_261A/Project_2/data/clean_unemployment_data.csv", 
                              header = TRUE)

# let's see the data
head(unemployment_data)

# Baseline Model (Simple Linear Regression)
# with no lags or any other modifications
baseline_model <- lm(
  Initial.Claims.Rate ~ Insured.Unemployment.Rate + Continued.Claims.Rate,
  data = unemployment_data
)

# summary
summary(baseline_model)

# so, result: the model explains about 37% of variation in Initial.Claims.Rate
# also, predictors are statistically insignificant (p > 0.6) 
# so, unemployment claims have strong time dependence

# selection criteria
AIC(baseline_model)
BIC(baseline_model)

# OLS diagnostic plots
# - Linearity
# - Homoscedasticity
# - Normality of residuals
# - Influence / leverage

png(filename = "/Users/noraadadurova/Desktop/Math_261A/assumptions_plot.png",
  width = 8,
  height = 6,
  units = "in",
  res = 300)

par(mfrow = c(2, 2))
plot(baseline_model)
par(mfrow = c(1, 1))

dev.off()

# autocorrelation of residuals
# Checks independence of errors (key assumption for time-series data)

acf(residuals(baseline_model),
    main = "ACF of Baseline Model Residuals")


# residuals over time
# highlights time dependence, structural breaks, and heteroskedasticity
# was getting errors
unemployment_data_2 <- unemployment_data |>
  mutate(
    Filed.week.ended = ymd(Filed.week.ended)  # convert to Date
  ) |>
  arrange(Filed.week.ended)

plot(unemployment_data_2$Filed.week.ended,
     residuals(baseline_model),
     type = "l",
     xlab = "Time",
     ylab = "Residuals",
     main = "Baseline Model Residuals Over Time")
abline(h = 0, col = "red")


# multicollinearity check using VIF
vif(baseline_model)

# let's add seasonal variables to improve baseline
baseline_model_seasonal <- lm(
  Initial.Claims.Rate ~ Insured.Unemployment.Rate +
    Continued.Claims.Rate +
    factor(Month) +
    factor(Quarter),
  data = unemployment_data
)

# summary
summary(baseline_model_seasonal)

# selection criteria
AIC(baseline_model_seasonal)
# result: AIC improves for seasonal model fits the data better
BIC(baseline_model_seasonal)
# result: BIC worsens for seasonal model is penalized more heavily for adding many parameters

# conclusion: Including month indicators improves fit only marginally.
# This is expected because same-week predictors contain limited information, 
# and unemployment claims are strongly autocorrelated.
# So, lagged models will naturally perform much better.
# Advanced Model (Lagged Regression Model)

# create lagged dataset
unemployment_lagged <- unemployment_data |>
  mutate(
    ICR_lag1 = dplyr::lag(Initial.Claims.Rate, 1),
    ICR_lag2 = dplyr::lag(Initial.Claims.Rate, 2),
    
    IUR_lag1 = dplyr::lag(Insured.Unemployment.Rate, 1),
    IUR_lag2 = dplyr::lag(Insured.Unemployment.Rate, 2),
    
    CCR_lag1 = dplyr::lag(Continued.Claims.Rate, 1)
  ) |>
  drop_na()

# fit the advanced model on both lags
advanced_model <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1,
  data = unemployment_lagged
)

# summary
summary(advanced_model)

AIC(advanced_model)

BIC(advanced_model)

# Residual diagnostics
par(mfrow = c(2 , 2))
plot(advanced_model)

acf(residuals(advanced_model), 
    main = "ACF of Advanced Model Residuals")

# let's try more lags
unemployment_lagged2 <- unemployment_data |>
  mutate(
    ICR_lag1 = lag(Initial.Claims.Rate, 1),
    ICR_lag2 = lag(Initial.Claims.Rate, 2),
    ICR_lag3 = lag(Initial.Claims.Rate, 3),
    
    IUR_lag1 = lag(Insured.Unemployment.Rate, 1),
    IUR_lag2 = lag(Insured.Unemployment.Rate, 2),
    IUR_lag3 = lag(Insured.Unemployment.Rate, 3),
    
    CCR_lag1 = lag(Continued.Claims.Rate, 1),
    CCR_lag2 = lag(Continued.Claims.Rate, 2)
  ) |>
  drop_na()

# fit test model
model_test <- lm(
  Initial.Claims.Rate ~
    ICR_lag1 + ICR_lag2 + ICR_lag3 +
    IUR_lag1 + IUR_lag2 + IUR_lag3 +
    CCR_lag1 + CCR_lag2,
  data = unemployment_lagged2
)

# summary
summary(model_test)
AIC(model_test)
BIC(model_test)

# it performs a bit worse
# so, use the lag 2 model as the best performing model

# let's try adding seasonality to the Advanced Model
unemployment_lagged_term <- unemployment_data |>
  mutate(
    ICR_lag1 = lag(Initial.Claims.Rate, 1),
    ICR_lag2 = lag(Initial.Claims.Rate, 2),
    
    IUR_lag1 = lag(Insured.Unemployment.Rate, 1),
    IUR_lag2 = lag(Insured.Unemployment.Rate, 2),
    
    CCR_lag1 = lag(Continued.Claims.Rate, 1),
    
    Time = row_number()  # adding the trend term
  ) |>
  drop_na()

# Advanced Model + Seasonality
advanced_model_seasonal <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month),
  data = unemployment_lagged_term
)

summary(advanced_model_seasonal)
AIC(advanced_model_seasonal)
BIC(advanced_model_seasonal)

# let's also try to add a trend term
advanced_model_trend <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    Time,
  data = unemployment_lagged_term
)

summary(advanced_model_trend)
AIC(advanced_model_trend)
BIC(advanced_model_trend)

summary(advanced_model_trend)
AIC(advanced_model_trend)
BIC(advanced_model_trend)

# let's combine trend and seasonality
advanced_model_full <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time,
  data = unemployment_lagged_term
)

summary(advanced_model_full)
AIC(advanced_model_full)
BIC(advanced_model_full)

# let's also try regularization covered in class (Ridge or Lasso regression)
X <- model.matrix(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    Time +
    factor(Month),
  data = unemployment_lagged_term
)[, -1]

y <- unemployment_lagged_term$Initial.Claims.Rate

# Cross-validated LASSO (alpha = 1)
cv_lasso <- cv.glmnet(X, y, alpha = 1)
lasso_lambda <- cv_lasso$lambda.min
lasso_lambda

lasso_model <- glmnet(X, y, alpha = 1, lambda = lasso_lambda)
coef(lasso_model)

# Cross-validated RIDGE (alpha = 0)
cv_ridge <- cv.glmnet(X, y, alpha = 0)
ridge_lambda <- cv_ridge$lambda.min
ridge_lambda

ridge_model <- glmnet(X, y, alpha = 0, lambda = ridge_lambda)
coef(ridge_model)

# let's try log-transforming the rate
advanced_model_log <- lm(
  log(Initial.Claims.Rate) ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time,
  data = unemployment_lagged_term
)

summary(advanced_model_log)
AIC(advanced_model_log)
BIC(advanced_model_log)


# let's include dummy variables for major events
# such as Great Recession (2007 to 2009) and COVID-19 spike (2020 to 2021)
unemployment_lagged_term <- unemployment_lagged_term |>
  mutate(
    Recession = ifelse(Year >= 2007 & Year <= 2009, 1, 0),
    COVID = ifelse(Year >= 2020 & Year <= 2021, 1, 0)
  )

advanced_model_breaks <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time +
    Recession + COVID,
  data = unemployment_lagged_term
)

summary(advanced_model_breaks)
AIC(advanced_model_breaks)
BIC(advanced_model_breaks)

# let's try one more approuch using winsorization
# adding winsorize the response variable
# manually compute 1% and 99% quantiles of the response
q_ICR <- quantile(unemployment_lagged_term$Initial.Claims.Rate,
                  probs = c(0.01, 0.99),
                  na.rm = TRUE)

q_ICR

# create winsorized version of Initial.Claims.Rate
unemployment_wins <- unemployment_lagged_term |>
  mutate(
    ICR_win = pmin(pmax(Initial.Claims.Rate, q_ICR[1]), q_ICR[2])
  )

head(unemployment_wins$ICR_win)

# fit the winsorized advanced model
winsorized_model <- lm(
  ICR_win ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time +
    Recession + COVID,
  data = unemployment_wins
)

# summary
summary(winsorized_model)
AIC(winsorized_model)
BIC(winsorized_model)

# Note: the Winsorized model is useful only if the analysis intentionally aims to remove extreme shocks

# let's try ides from the class Weighted Least Squares (Slide set 15)
# Use absolute OLS residuals as a proxy for the error sd (heteroskedasticity)
sigma_hat <- abs(resid(advanced_model_breaks))

# fit the model
# so the main idewa of WLS: give lower weight to observations with larger residual variance
wls_model <- lm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time +
    Recession + COVID,
  data    = unemployment_lagged_term,
  weights = 1 / sigma_hat^2
)

# summary
summary(wls_model)
AIC(wls_model)
BIC(wls_model)

# let's try Robust Regression (Slide set 16)
huber_model <- rlm(
  Initial.Claims.Rate ~ 
    ICR_lag1 + ICR_lag2 +
    IUR_lag1 + IUR_lag2 +
    CCR_lag1 +
    factor(Month) +
    Time +
    Recession + COVID,
  data = unemployment_lagged_term,
  psi  = psi.huber   # Huber loss as in the slides
)

# summary
summary(huber_model)
AIC(huber_model)
BIC(huber_model)

# Optional: standardized residuals from robust fit
robust_resid <- resid(huber_model)


# let's try Nonlinear regression & GLMs (Slides 17–20)
# note that there’s no obvious nonlinear parametric curve to fit here, 
# and the assignment is “regression with this time-series-like data,” 
# so it should be fine not to use nls or similar models for this current project
# for GLMs, slides are focused on logistic / Poisson for discrete outcomes
# this data has a continuous rate, not counts or binary, so a GLM is not really natural here
# Gamma GLM with log link is another option for positive rates
# this could be an option for future development of the project
# but I will skip for now

# let's make general AIC/BIC comparison table
model_compare <- data.frame(
  Model = c(
    "Baseline",
    "Advanced",
    "Advanced + Seasonality",
    "Advanced + Trend",
    "Advanced + Seasonality + Trend",
    "Advanced + Log",
    "Advanced + Breaks",
    "Winsorized Advanced + Breaks",
    "WLS Advanced + Breaks",
    "Robust Regression"
  ),
  AIC = c(
    AIC(baseline_model),
    AIC(advanced_model),
    AIC(advanced_model_seasonal),
    AIC(advanced_model_trend),
    AIC(advanced_model_full),
    AIC(advanced_model_log),
    AIC(advanced_model_breaks),
    AIC(winsorized_model),
    AIC(wls_model),
    AIC(huber_model)
  ),
  BIC = c(
    BIC(baseline_model),
    BIC(advanced_model),
    BIC(advanced_model_seasonal),
    BIC(advanced_model_trend),
    BIC(advanced_model_full),
    BIC(advanced_model_log),
    BIC(advanced_model_breaks),
    BIC(winsorized_model),
    BIC(wls_model),
    BIC(huber_model)
  )
)

model_compare

# result: found that advanced_model_breaks performs the best on the original data set
# Because WLS and Winsorized models modify the error structure, 
# their AIC/BIC values are not directly comparable to the OLS models. 
# so, the extreme improvements in AIC/BIC reflect the weighting scheme rather 
# than true predictive improvement.

# use standard 4 diagnostic plots (Residuals vs Fitted, QQ, Scale-Location, Cook's distance)
par(mfrow = c(2, 2))
plot(advanced_model_breaks)

par(mfrow = c(1, 1))  # reset layout

# make a small diagnostics data frame
diag_df <- data.frame(
  fitted   = fitted(advanced_model_breaks),
  resid    = resid(advanced_model_breaks),
  std_res  = rstandard(advanced_model_breaks),
  leverage = hatvalues(advanced_model_breaks),
  cooks    = cooks.distance(advanced_model_breaks)
)

# ACF of residuals (to check remaining serial correlation)
acf(diag_df$resid, main = "ACF of Residuals: Advanced + Breaks")

# identify potentially problematic observations
high_std_resid   <- which(abs(diag_df$std_res) > 3)                # large residuals
high_leverage    <- which(diag_df$leverage > 2 * mean(diag_df$leverage))  # high leverage
high_cooks       <- which(diag_df$cooks > 4 / nrow(diag_df))       # influential points

high_std_resid
high_leverage
high_cooks

# check multicollinearity with VIF
# VIF is discussed in the multicollinearity slides.
# Requires the 'car' package
vif_advanced        <- vif(advanced_model)
vif_advanced_breaks <- vif(advanced_model_breaks)

vif_advanced
vif_advanced_breaks

# simple residual plots vs. key predictors
# Residuals vs Time (check for remaining trend pattern)
plot(unemployment_lagged_term$Time,
     diag_df$resid,
     xlab = "Time",
     ylab = "Residuals",
     main = "Residuals vs Time (Advanced + Breaks)")
abline(h = 0, col = "red")

# Residuals vs fitted (already in plot.lm, but explicit plot for report)
plot(diag_df$fitted,
     diag_df$resid,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Advanced + Breaks)")
abline(h = 0, col = "red")

