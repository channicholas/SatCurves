library(readr)
library(optimr)
library(car)
library(MASS)
source(paste0(getwd(), '/R/Improve/Sat_Transformation.R'))
source(paste0(getwd(), '/R/Improve/Sat_Optimizer.R'))
source(paste0(getwd(), '/R/Improve/Sat_Analytics.R'))


### S-curves###
set.seed(1)

predictor <- seq(0,6,.05)
mu <- 3; s <- .75
response <- pnorm(predictor, mu, s) * 20000 + rnorm(length(predictor), 1000, 900)  # generating a sigmoid (s-curve)
sig_df <- data.frame(predictor,response)
sig_df$predictor <- sig_df$predictor * 2500
lmSigmoid <- lm(response ~ predictor, sig_df)


formula <- formula(response~predictor)

(sat_obj_sig <- Curve_Reg(temp_df = sig_df, formula = formula,
                          curveVars = 'predictor', response = 'response',
                          startParams = c(8,.55), optFunc = "aic"))

# when we plot it you can see the improvement compared to the linear regression.

plot(sig_df$predictor, sig_df$response)
lines(sig_df$predictor, sat_obj_sig$linear_model$fitted.values, col = 'blue')
lines(sig_df$predictor, lmSigmoid$fitted.values, col = 'red')

# below you can see the power in prediction with the linear regression
# in-sample results vs the out of sample saturation curve.
# this dataframe is with a normal linear regression
error <- fitted(lmSigmoid) - sig_df$response
cbind(MAE = mean(abs(error)),
      RMSE = sqrt(mean(error^2)),
      RSQ = rsq(fitted(lmSigmoid), sig_df$response))

Cross_Validation(sat_obj_sig, startParams = c(8,.55)) # this is with the curve

### C-curve
set.seed(1)
predictor <- 1:100
# Add random noise to the sequence
variance <- rnorm(n, mean = 500, sd = 50)
response <- log(predictor) * 1000 + variance
log_df <- data.frame(predictor,response)
lmLog<- lm(response ~ predictor, log_df)

formula <- formula(response~predictor)

(sat_obj_log <- Curve_Reg(temp_df = log_df, formula = formula,
                          curveVars = 'predictor', response = 'response',
                          startParams = c(-1,.1), optFunc = "mae"))

plot(log_df$predictor, log_df$response)
lines(log_df$predictor, lmLog$fitted.values, col = 'blue')
lines(log_df$predictor, sat_obj_log$linear_model$fitted.values, col = 'red')
# below you can see the power in prediction with the linear regression
# in-sample results vs the out of sample saturation curve.
# this dataframe is with a normal linear regression
error <- fitted(lmLog) - log_df$response
cbind(MAE = mean(abs(error)),
      RMSE = sqrt(mean(error^2)),
      RSQ = rsq(fitted(lmLog), log_df$response))

Cross_Validation(sat_obj_log, startParams = c(sat_obj_log$alphas, sat_obj_log$gammas)) # this is with the curve



#### Salary Prediction
# https://www.kaggle.com/code/ibrahimyildiz/salary-predict-with-nonlinear-regression-models/data
#
salary_df <- data.frame(read.csv(paste0(getwd(), '/Data/Hitters.csv')))
salary_df <- salary_df[!is.na(salary_df$Salary),]
salary_df$League <- factor(salary_df$League)
salary_df$Division <- factor(salary_df$Division)

y <- 'Salary'
x <- c('HmRun', 'Years', 'Hits', 'Runs')

lm <- lm(Salary ~ HmRun + Years + League + Division + Hits + Runs, data = salary_df)
lm <- lm(Salary ~ ., data = salary_df)

step.model <- stepAIC(lm, direction='backward')

vif(lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns +
         CRBI + CWalks + Division + PutOuts + Assists, salary_df))
#removing CAtBat, AtBat, CRuns, CWalks
###### Note I should build in the ability to do LASSO regression
lm_salary <- lm(Salary ~ Hits + Walks + CRBI + Division + PutOuts + Assists, salary_df)
avPlots(lm_salary) # it looks like most are linear with the exception of CRBI and maybe Hits

formula <- formula(Salary ~ Hits + Walks + CRBI + Division + PutOuts + Assists)

(sat_obj_sal <- Curve_Reg(temp_df = salary_df, formula = formula,
                          curveVars = c('CRBI', 'Hits'), response = 'Salary',
                          startParams = c(-.5, 3,.5, .55), optFunc = "aic"))

# below you can see the power in prediction with the linear regression
# in-sample results vs the out of sample saturation curve.
# this dataframe is with a normal linear regression
error <- fitted(lm_salary) - salary_df$Salary
cbind(MAE = mean(abs(error)),
      RMSE = sqrt(mean(error^2)),
      RSQ = rsq(fitted(lm_salary), salary_df$Salary))

Cross_Validation(sat_obj_sal, startParams = c(sat_obj_sal$alphas, sat_obj_sal$gammas)) # this is with the curve
