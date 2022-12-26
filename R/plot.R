curve_plot <- function(listed_df, names = names(listed_df), rsqs, threshold = 0, save = FALSE){
  RSQ_INDEX <- which(rsqs > threshold)
  NAMES_INDEX <- which(grepl(paste(names, collapse = '|'), listed_df))
  if(length(RSQ_INDEX) > length(NAMES_INDEX)) INDEX <- match(NAMES_INDEX, RSQ_INDEX)[!is.na(match(NAMES_INDEX, RSQ_INDEX))]
  else INDEX <- match(RSQ_INDEX, NAMES_INDEX)
  print(paste('Printing', toString(length(INDEX)), ' out of ', toString(length(NAMES_INDEX)), 'given Names'))

  for(i in 1:length(INDEX)){
    j <- INDEX[i]
    if(listed_df[[j]][1,1] == 'NON-BRAND' | listed_df[[j]][1,1] == 'BRAND'){
      plot <- ggplot(listed_df[[j]]) +
        geom_smooth(mapping = aes(x = SPEND,
                                  y = FITTED_VALUES), color = 'black') +
        geom_point(mapping = aes(x = SPEND,
                                 y = FITTED_VALUES), color = 'black') +

        geom_point(mapping = aes(x = SPEND,
                                 y = TTL_GM, shape = EGM_FLG,
                                 color = EGM_FLG))+
        labs(title = paste('SATURATION CURVE -', listed_df[[j]][1,2]),
             x = "REALIZED SPEND", y = "TOTAL GROSS MARGIN",
             caption=paste0(toString(round(rsqs[j],2) * 100), "% of the variabilty in the data is explained by the model"))+
        theme_grey()
      print(plot)
    }else{
      plot <- ggplot(listed_df[[j]]) +
        geom_smooth(mapping = aes(x = REALIZED_SPEND,
                                  y = FITTED_VALUES), color = 'black') +
        geom_point(mapping = aes(x = REALIZED_SPEND,
                                 y = FITTED_VALUES), color = 'black') +

        geom_point(mapping = aes(x = REALIZED_SPEND,
                                 y = TTL_GM, shape = EGM_FLG,
                                 color = EGM_FLG))+
        labs(title = paste('SATURATION CURVE -', listed_df[[j]][1,2]),
             x = "REALIZED SPEND", y = "TOTAL GROSS MARGIN",
             caption=paste0(toString(round(rsqs[j],2) * 100), "% of the variabilty in the data is explained by the model"))+
        theme_grey()
      print(plot)
    }

    if(save == TRUE){
      pdf(paste0("", toString(i),".pdf"))
      par(mfrow = c(1,2))
      print(plot)
      dev.off()
    }
  }
}
