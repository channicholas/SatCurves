rsq <- function(predicted, actuals){
  rss <- sum((predicted - actuals) ^ 2)  ## residual sum of squares
  tss <- sum((actuals - mean(actuals)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  return(rsq)
}

Cross_Validation <- function(sat_object, startParams, numIter = 20, trainSamplePct = .75){
  error <- c()
  preds <- c()
  actuals <- c()
  for(i in 1:numIter){
    trainIds <- sample(nrow(sat_object$original_df), floor(.7*nrow(sat_object$original_df)))

    train_df <- sat_object$original_df[trainIds,]
    test_df <- sat_object$original_df[-trainIds,]

    sat_obj_temp <- Curve_Reg(temp_df = train_df, formula = sat_object$formula,
                              curveVars = sat_object$curveVars, sat_object$response, startParams,
                              optFunc = "mae")

    preds <- c(preds, Sat_Predict(sat_obj_temp, test_df))
    actuals <- c(actuals, test_df[[sat_object$response]])

  }
  error <- preds - actuals

  return(cbind(MAE = mean(abs(error)), RMSE = sqrt(mean(error^2)), RSQ = rsq(preds, actuals)))
}

Sat_Predict <- function(sat_object, new_df){
  test_df <- Data_Cleaning(sat_object$original_df, sat_object$curveVars,
                           sat_object$alphas, sat_object$gammas, new_df)
  lm.test <- lm(sat_object$formula, sat_object$transformed_df)
  preds <- predict(lm.test, test_df)
  return(preds)
}
