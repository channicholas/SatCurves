Saturation_Hill <- function(tempSpend, alpha, gamma, newData = NA){ # newData is a vector
  inflexion <- c(range(tempSpend) %*% c(1 - gamma, gamma))

  if(any(!is.na(newData))){ # if there is new data
    sat <- newData * (newData**alpha / (newData**alpha + inflexion**alpha))

  }else{ # no newdata means run like normal
    sat <- tempSpend * (tempSpend**alpha / (tempSpend**alpha + inflexion**alpha))
  }
  return(sat)
}

Data_Gather <- function(temp_df, transformed_df){
  # temp_df is the original untransformed dataframe
  # transformed_df is the transformed dataframe but only with transformed media spend columns (no control columns)
  # so this function adds those control columns back in
  nTempDf <- nrow(temp_df)
  nTransformedDf <- nrow(transformed_df)
  stopifnot(nTempDf == nTransformedDf) # check if these are equal

  temp_df <- temp_df[!colnames(temp_df) %in% colnames(transformed_df)] # get the columns that are not in the transformed_df
  output_df <- cbind(temp_df, transformed_df)
  return(output_df)
}


Data_Cleaning <- function(temp_df, curveVars, alphas, gammas, newData = NA){
  split_col <- c(asplit(temp_df[curveVars], 2)) # splits up my data frame by column to apply adstock and saturation

  if(any(is.na(newData))){ # is there isn't new data
    transformed_df <- mapply(Saturation_Hill, split_col, alphas, gammas, SIMPLIFY = FALSE)
  }else{
    split_new_data <- c(asplit(newData[curveVars], 2))
    # this also needs the original data adstocked and not just the new data adstocked.
    transformed_df <- mapply(Saturation_Hill, split_col, alphas, gammas, split_new_data, SIMPLIFY = FALSE)
  }

  transformed_df <- data.frame(do.call(cbind, transformed_df))

  colnames(transformed_df) <- curveVars # reassign names

  if(any(is.na(newData))){ # if there are NA's no newData
    output_df <- Data_Gather(temp_df, transformed_df)
  }else{
    output_df <- Data_Gather(newData, transformed_df)
  }

  return(output_df)
}
