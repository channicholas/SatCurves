library(readr)
library(optimr)
source(paste0(getwd(), '/R/curve.R'))
source(paste0(getwd(), '/R/plot.R'))
source(paste0(getwd(), '/R/modelAnalytics.R'))
source(paste0(getwd(), '/R/regFunctions.R'))
# china gdp data Example 1:
#https://s3-api.us-geo.objectstorage.softlayer.net/cf-courses-data/CognitiveClass/ML0101ENv3/labs/china_gdp.csv
china_gdp <- read_csv(paste0(getwd(), "/Data/china_gdp.csv"))

lm <- lm(Value ~ Year, china_gdp)
summary(lm)
# here we plot the linear model which doesn't show any trend in the data.
plot(china_gdp$Year, china_gdp$Value)
lines(china_gdp$Year, lm$fitted.values, col = 'red')

# we set up to run a non-linear regression
df <- china_gdp
x <- 'Year'
y <- 'Value'
# here we choose rmse as our minimizing function the other option is mae
# this also just runs Nelder-Mead optimization
fitted1 <- s_curve(china_gdp, y = y, x = x, startParams = c(25,5),
                   minFunc = "rmse", optFunc = "singleOptim")
# when we plot it you can see the improvement compared to the linear regression.
# there still looks like it may be a little under fit. I will need to look more
# into this
plot(china_gdp$Year, china_gdp$Value)
lines(china_gdp$Year, lm$fitted.values, col = 'red')
lines(china_gdp$Year, fitted1, col = 'blue')

# this is showing a fit using the mulitple optim, this will run the
# CG and nlminb methods from optimr package, the output is a list of vectors
fitted2 <- s_curve(china_gdp, y = y, x = x, startParams = c(65,5),
                   minFunc = "rmse", optFunc = "multipleOptim")
lines(china_gdp$Year, fitted2[[1]], col = 'green')
lines(china_gdp$Year, fitted2[[2]], col = 'yellow')

# here you can use the rsq function to analyze how well it did in terms of r^2
rsq(fitted1, china_gdp$Value)
rsq(lm$fitted.values, china_gdp$Value)
rsq(fitted2[[1]], china_gdp$Value)
rsq(fitted2[[2]], china_gdp$Value)



### S-curves###
set.seed(1)
predictor <- seq(0,5,.05)
mu <- 2.5; s <- .75
response <- pnorm(predictor, mu, s) * 20000 + rnorm(length(predictor), 1000, 900)  # generating a sigmoid (s-curve)
sig_df <- data.frame(predictor,response)
sig_df$predictor <- sig_df$predictor * 2500
lmSigmoid <- lm(response ~ predictor, sig_df)


y <- 'response'
x <- 'predictor'
df <- sig_df
fitted3 <- s_curve(df, y = y, x = x, startParams = c(2,.02),
                   minFunc = "mae", optFunc = "singleOptim") # returning same number?
# fitted3 <- as.vector(fitted3)

plot(sig_df$predictor, sig_df$response)
lines(sig_df$predictor, lmSigmoid$fitted.values)
lines(sig_df$predictor, fitted3, col = 'red')

rsq(lm$fitted.values, sig_df$response); rsq(fitted3, sig_df$response);



### C-curve
set.seed(1)
predictor <- seq(1,6,.05)
mu <- 2.5; s <- .75
response <- log(predictor) * 20000 + log(rnorm(length(predictor), 1500, 300))  # generating a sigmoid (s-curve)
sig_df <- data.frame(predictor,response)
sig_df$predictor <- sig_df$predictor * 2500
lmSigmoid <- lm(response ~ predictor, sig_df)


y <- 'response'
x <- 'predictor'
df <- sig_df
fitted4 <- s_curve(df, y = y, x = x, startParams = c(.5,.25),
                   minFunc = "mae", optFunc = "singleOptim") # returning same number?
# fitted3 <- as.vector(fitted3)

plot(sig_df$predictor, sig_df$response)
lines(sig_df$predictor, lmSigmoid$fitted.values)
lines(sig_df$predictor, fitted4, col = 'red')

rsq(lm$fitted.values, sig_df$response); rsq(fitted3, sig_df$response);





#### Salary Prediction
# https://www.kaggle.com/code/ibrahimyildiz/salary-predict-with-nonlinear-regression-models/data
# salary_df <- data.frame(read_csv('Hitters.csv'))
# salary_df <- salary_df[!is.na(salary_df$Salary),]
# salary_df$League <- factor(salary_df$League)
# salary_df$Division <- factor(salary_df$Division)
#
# y <- 'Salary'
# x <- c('HmRun', 'Years', 'Hits', 'Runs')
# df <- salary_df
# factVars <- c('League', 'Division')
# # rm(lineVars)
# lineVars <-  c()
# lm <- lm(Salary ~ HmRun + Years + League + Division + Hits + Runs, data = salary_df)
# fitted <- s_curve(salary_df, y, x, factVars, seed = 1,
#                   startParams = c(25,5,25,5,25,5,25,5),
#                   minFunc = "rmse", optFunc = "singleOptim")
# hist(salary_df$Salary)
# plot(salary_df$HmRun, salary_df$Salary)
# plot(salary_df$HmRun, salary_df$Years)
#
#
# # pairs(salary_df[-c(14,15,20)])
# rsq(fitted,salary_df$Salary)
# rsq(lm$fitted.values, salary_df$Salary)


