singleOptim_test <- function(theta, minFunc, curveList){
  opt <- optim(theta, fn = paste(minFunc, '_test'), t = curveList, control = list(maxit = 1000, ndeps = 1),
               method = "Nelder-Mead")
  print(opt)

  optTheta <- opt$par
  Iterator <- 1:ncol(curveList[[2]])
  fittedValues <- final_predict(optTheta, Iterator, curveList[[2]], curveList[[3]], curveList[[4]])
  return(fittedValues)
}


s_curve_test <- function(df, y, x, startParams, factVars = ' ', lineVars = ' ',
                         seed = 1, minFunc = "rmse", optFunc = "optim"){
  #changes factors to a single name for the regression formula
  if(length(startParams) != length(x) * 2){
    message('You need and Alpha and Gamma for each non-linear coefficient.')
    return(1)
  }
  numCurveVars <- length(x)
  if(factVars[1] != ' '){
    f_factors <- paste('+ ', factVars, collapse = ' ')
  } else{f_factors <- factVars}

  if(lineVars[1] != ' '){
    f_lineVars <- paste('+ ', lineVars, collapse = ' ')
  } else{f_lineVars <- lineVars}

  if(numCurveVars > 1){
    f_curveVars <- paste(x, collapse = ' + ')
  } else{f_curveVars <- x}

  lm <- reg(df, y, f_curveVars, f_factors, f_lineVars) # running regression
  reg_coef <- coef(lm)

  # setting variables to run optimizer
  response <- df[y] # renaming to y
  c_predictors <- df[x]
  curveSlopes <- reg_coef[x] # will this get all of the slopes for multiple x's?
  theta <- startParams # setting optim starting params 50,5

  curveList <- list(response, c_predictors, curveSlopes, lm)

  # running optimizer
  print('running optimizer')
  set.seed(seed)
  print(paste(minFunc, 'is the minimizing function'))
  print(paste(optFunc, 'is the optimizing function'))
  minFunc <- match.fun(minFunc)
  optFunc <- match.fun(optFunc)
  fittedValues <- optFunc(theta, minFunc, curveList)

  return(fittedValues)
}



