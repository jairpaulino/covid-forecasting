getTrendAnalysis = function(timeSeries_df, w){
  #timeSeries_df = incDia
  timeSeries_df = na.omit(timeSeries_df)
  len = length(timeSeries_df)
  #w = 21
  
  trend_df = data.frame(matrix(ncol = (w+5), nrow = (len-w)))
  names_trend = NULL
  for(i in 1:w){names_trend[i] = paste("x_", i, sep="")}
  names(trend_df)[1:w] = names_trend
  names(trend_df)[(w+1):(w+5)] = c("A", "B", "mk_pvalue", "S", "Class") 
  
  for (i in 1:(len-w)){#i=1
    mkTest = mk.test(timeSeries_df[i:(i+w-1)]) 
    pvalue = mkTest$p.value
    trend_df$mk_pvalue[i] = pvalue
    S = mkTest$estimates[1]
    trend_df$S[i] = S
    
    trend_df[i,1:w] = timeSeries_df[i:(i+w-1)]
    
    lm = lm(timeSeries_df[i:(i+w-1)] ~ seq(1, w))
    
    trend_df$A[i] = lm$coefficients[2]
    trend_df$B[i] = lm$coefficients[1]
    
    col = c("yellow", "red", "blue")
    
    alpha = 0.05
    if(pvalue > alpha | is.na(pvalue)){
      alphaColor = col[1]
      trend_df$Class[i] = "None"
    }else{
      if(S >= 0){
        alphaColor = col[2]
        trend_df$Class[i] = "Positive"
      }else{
        alphaColor = col[3]
        trend_df$Class[i] = "Negative"
      }
}
    
    #View(trend_df)
    
    # png(paste("Results/linearCoef_img/cl_", i, ".png",sep = ""))
    # plot(timeSeries_df[i:(i+w-1)],# ~ i:(i+w-1),
    #      xlab = "Observação",
    #      ylab = "Incidência", 
    #      lwd = 4, pch = 19,
    #      axes = T)
    # abline(lm, col = alphaColor, lwd = 4)
    # if(max(timeSeries_df[i:(i+w-1)])==0){
    #   text(paste("lc = ", round(lm$coefficients[2], 3), sep=""),
    #        x = (5), y = 1)
    # }else{
    #   text(paste("lc = ", round(lm$coefficients[2], 3), sep=""),
    #        x = (5), y = max(max(timeSeries_df[i:(i+w-1)])))
    # }
    # dev.off()
  }
  return(trend_df)
}
