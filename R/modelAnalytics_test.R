mae_test <- function(t, theta){
  if(any(theta <= 0)) return(Inf)
  if(any(theta[1:(length(theta) / 2) * 2] > 1)) return(Inf)

  response <- t[[1]] # this is column from df
  c_predictors <- t[[2]] # this is a df
  curveSlopes <- t[[3]]
  lm <- t[[4]]
  cIterator <- 1:ncol(c_predictors)

  sats <- data.frame(sapply(cIterator, my_apply, theta, c_predictors)) # nested apply to run saturation_hill
  predictions <- fitted_values_test(sats, cIterator, curveSlopes, c_predictors, lm)
  error <- c(predictions - response)
  return(round(mean(abs(error[[colnames(response)]]))))
}

#change this to exclude corrections
fitted_values_test <- function(sats, cIterator, curveSlopes, c_predictors, lm){

  curve_predictions <- apply(data.frame(
    predict_curve(curveSlopes, sats, c_predictors)),MARGIN = 1, sum)
  corrections <- apply(data.frame( # this gets linear predictions
    mapply(predict_lm, curveSlopes, c_predictors)), MARGIN = 1, sum)
  predictions <- lm$fitted.values - corrections + curve_predictions

  return(predictions)
}
