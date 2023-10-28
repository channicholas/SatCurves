
Return_Value_Function <- function(optFunc, model){
  error <- resid(model)
  if(optFunc == 'rmse') returnValue <- sqrt(mean((error)^2))
  else if(optFunc == 'aic') returnValue <- AIC(model)
  else returnValue <- mean(abs(error))
  return(returnValue)
}

Rules <- function(alphas, gammas, alphaMax, alphaMin, gammaMax, gammaMin){
  if(any(alphas >= alphaMax) | any(alphas <= alphaMin) |
     any(gammas >= gammaMax) | any(gammas <= gammaMin)) return(TRUE)
  else return(FALSE)
}

Curve_Optimizer <- function(theta, t){
  temp_df <- t[[1]]
  numVars <- t[[2]]
  formula <- t[[3]]
  curveVars <- t[[4]]
  response <- t[[5]]
  optFunc <- t[[6]]
  alphaMax <- t[[7]]
  alphaMin <- t[[8]]
  gammaMax <- t[[9]]
  gammaMin <- t[[10]]

  alphas <- theta[1:numVars]
  gammas <- theta[(numVars + 1):(numVars * 2)]

  if(Rules(alphas, gammas, alphaMax, alphaMin, gammaMax, gammaMin)) return(9999999)
  temp_df2 <- Data_Cleaning(temp_df, curveVars, alphas, gammas)

  test.lm <- lm(formula, temp_df2)

  val <- Return_Value_Function(optFunc, test.lm)

  return(val)
}

Make_Sat_Curve_Object <- function(t, opts){
  temp_df <- t[[1]]
  numVars <- t[[2]]
  formula <- t[[3]]
  curveVars <- t[[4]]
  response <- t[[5]]
  optfunc <- t[[6]]

  # 1. save opts
  alphas <- opts$par[1:numVars]
  gammas <- opts$par[(numVars + 1):(numVars * 2)]
  # 2. Data clean
  temp_df2 <- Data_Cleaning(temp_df, curveVars, alphas, gammas)
  # 3. make model
  test.lm <- lm(formula, temp_df2)

  sat_object <- list(
    linear_model = test.lm,
    transformed_df = temp_df2,
    original_df = temp_df,
    curveVars = curveVars,
    response = response,
    formula = formula,
    alphas = alphas,
    gammas = gammas,
    optim_output = opts
  )

  return(sat_object)
}

Curve_Reg <- function(temp_df, formula, curveVars, response, startParams, optFunc,
                      maxIterations = 1000, alphaMax = 55, alphaMin = -55,
                      gammaMax = 1, gammaMin = 0){
  numVars <- length(curveVars)
  if(length(startParams) != (numVars * 2)) return(1)

  curveList <- list(temp_df, numVars, formula, curveVars, response, optFunc,
                    alphaMax, alphaMin, gammaMax, gammaMin)

  opts <- optim(startParams, fn = Curve_Optimizer, t = curveList,
                control = list(maxit = maxIterations))

  sat_object <- Make_Sat_Curve_Object(curveList, opts)

  return(sat_object)
}


