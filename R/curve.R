saturation_hill <- function(x, alpha, gamma, x_marginal = NULL) {
  inflexion <- c(range(x) %*% c(1 - gamma, gamma)) # linear interpolation by dot product
  if (is.null(x_marginal)) {
    x_scurve <- x**alpha / (x**alpha + inflexion**alpha) # plot(x_scurve) summary(x_scurve)
  } else {
    x_scurve <- x_marginal**alpha / (x_marginal**alpha + inflexion**alpha)
  }
  return(x_scurve)
}

singleOptim <- function(theta, minFunc, curveList){
  opt <- optim(theta, fn = minFunc, t = curveList, control = list(maxit = 1000, ndeps = 1),
               method = "Nelder-Mead")
  print(opt)

  optTheta <- opt$par
  Iterator <- 1:ncol(curveList[[2]])
  fittedValues <- final_predict(optTheta, Iterator, curveList[[2]], curveList[[3]], curveList[[4]])
  return(fittedValues)
}

multipleOptim <- function(theta, minFunc, curveList){
  opt <- optimr::opm(theta, fn = minFunc, t = curveList, control = list(maxit = 1000, ndeps = 1),
                     method=c('CG', 'nlminb'))

  optThetaCG <- c(opt$p1[1], opt$p2[1])
  optThetaNLM <- c(opt$p1[2], opt$p2[2])

  Iterator <- 1:ncol(curveList[[2]])
  fittedValuesCG <- final_predict(optThetaCG, Iterator, curveList[[2]], curveList[[3]], curveList[[4]])
  fittedValuesNLM <- final_predict(optThetaNLM, Iterator, curveList[[2]], curveList[[3]], curveList[[4]])

  fittedValues <- list(fittedValuesCG, fittedValuesNLM)
  print(opt)

  return(fittedValues)
}

s_curve <- function(df, y, x, startParams, factVars = ' ', lineVars = ' ',
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
  minFunc <- match.fun(minFunc)
  optFunc <- match.fun(optFunc)

  fittedValues <- optFunc(theta, minFunc, curveList)
  print('exit opt')
  return(fittedValues)
}


