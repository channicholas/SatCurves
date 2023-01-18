outlier <- function(vec){
  upperQuantile <- mean(vec) + 2.5 * sd(vec)
  lowerQuantile <- mean(vec) - 2.5 * sd(vec)
  df <- cbind(ifelse(vec >= upperQuantile, TRUE, FALSE), ifelse(vec <= lowerQuantile, TRUE, FALSE))
  return(apply(df, 1, max))
}

rsq <- function(predicted, actuals){
  rss <- sum((predicted - actuals) ^ 2)  ## residual sum of squares
  tss <- sum((actuals - mean(actuals)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  return(rsq)
}

my_apply <- function(iterator, params, c_predictors){
  if(ncol(c_predictors != 1)) x <- c_predictors[iterator]
  alpha <- params[iterator]
  gamma <- params[iterator + 1]
  saturation_hill(x, alpha, gamma)
}

predict_curve <- function(curveSlopes, sats, c_predictors, applyCase = FALSE){
  # making s curve with intercept correction
  c_predictors <- data.frame(c_predictors)
  if(ncol(c_predictors) == 1 & applyCase == FALSE){
    beta <- as.numeric(curveSlopes[colnames(c_predictors)])
    temp_sat <- data.frame(sats[colnames(c_predictors)])
    (temp_predictor <- data.frame(c_predictors))
    I <- lapply(((temp_predictor * beta) * (1 - temp_sat)), median)

    p <- beta * temp_sat * temp_predictor + I
  }else if(applyCase == TRUE){
    beta <- curveSlopes
    temp_sat <- data.frame(sats)
    temp_predictor <- c_predictors
    print(nrow(temp_predictor))
    print(nrow(sats))
    int <- ((temp_predictor * beta) * (1 - temp_sat))
    I <- lapply(int, median)

    p <- beta * temp_sat * temp_predictor + I
  }else{
    iterator <- 1:ncol(c_predictors)
     p <- data.frame(mapply(predict_curve, curveSlopes, sats, c_predictors, TRUE))
  }
  return(p)
}

predict_lm <- function(curveSlopes, c_predictors){
  return(curveSlopes * c_predictors)
}

fitted_values <- function(sats, cIterator, curveSlopes, c_predictors, lm){

  curve_predictions <- apply(data.frame(
    predict_curve(curveSlopes, sats, c_predictors)),MARGIN = 1, sum)
  corrections <- apply(data.frame( # this gets linear predictions
    mapply(predict_lm, curveSlopes, c_predictors)), MARGIN = 1, sum)
  predictions <- lm$fitted.values - corrections + curve_predictions

  return(predictions)
}

# theta <- c(1,.3,2,1.1)
mae <- function(t, theta){
  if(any(theta <= 0)) return(Inf)
  if(any(theta[1:(length(theta) / 2) * 2] > 1)) return(Inf)

  response <- t[[1]] # this is column from df
  c_predictors <- t[[2]] # this is a df
  curveSlopes <- t[[3]]
  lm <- t[[4]]
  cIterator <- 1:ncol(c_predictors)

  sats <- data.frame(sapply(cIterator, my_apply, theta, c_predictors)) # nested apply to run saturation_hill
  print(lm$fitted.values)
  predictions <- fitted_values(sats, cIterator, curveSlopes, c_predictors, lm)
  error <- c(predictions - response)
  return(round(mean(abs(error[[colnames(response)]]))))
}

final_predict <- function(optTheta, cIterator, c_predictors, curveSlopes, lm){
  sats <- data.frame(sapply(cIterator, my_apply, optTheta, c_predictors)) # nested apply to run saturation_hill
  predictions <- fitted_values(sats, cIterator, curveSlopes, c_predictors, lm)
  return(predictions)
}

rmse <- function(t, theta){
  if(any(theta < .001)) return(Inf)

  response <- t[[1]] # this is column from df
  c_predictors <- t[[2]] # this is a df
  curveSlopes <- t[[3]]
  lm <- t[[4]]
  cIterator <- 1:ncol(c_predictors)

  sats <- data.frame(sapply(cIterator, my_apply, theta, c_predictors)) # nested apply to run saturation_hill
  predictions <- fitted_values(sats, cIterator, curveSlopes, c_predictors, lm)

  error <- c(predictions - response)
  return(round(mean((error[[colnames(response)]])^2)))
}










