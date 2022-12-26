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
