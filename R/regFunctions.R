reg <- function(df, y, x, factVars = ' ', lineVars = ' '){

  if(factVars[1] == ' '){formula <- paste(y, '~', x)
  } else{formula <- paste(y, '~', x, factVars)}
  if(lineVars[1] != ' ') formula <- paste(formula, lineVars)


  ols <- lm(formula, data = df)
  coef <- data.frame(coef(ols))
  return(ols)
}

coefs <- function(Var, reg_coef){
  return(reg_coef[Var])
}

predict_reg <- function(iterator, coef, sat, c_predictors, intercept){
  # making s curve with intercept correction
  coef[iterator] * sat[iterator] * c_predictors[iterator] +
    (coef[iterator] * c_predictors[iterator] -
       coef[iterator] * sat[iterator] * c_predictors[iterator]) +
    intercept
}
