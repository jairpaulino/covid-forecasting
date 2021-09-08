#' getTrendAnalysis
#'
#' @param timeSeries_df An Array of numeric values. 
#' @param w An Integer indicating the sliding window length. 
#' @alpha 
#'
#' @return A matrix with trend analysis.
#' @export
#'
#' @examples
getTrendAnalysis = function(timeSeries_df, w, alpha = 0.05, nStepAhead = 14){
  #timeSeries_df = mm14LogIncDia
  #timeSeries_df = na.omit(timeSeries_df)
  len = length(timeSeries_df)
  #w = 14
  newTS = NULL
  for (i in 1:len){
    if(timeSeries_df[i] >= 0){
      newTS[i] = timeSeries_df[i]
    }
  }
  timeSeries_df = na.omit(newTS)
  len = length(newTS)
  trend_df = data.frame(matrix(ncol = (w+5), nrow = (len-w)))
  names_trend = NULL
  for(i in 1:w){names_trend[i] = paste("x", i, sep="")}
  names(trend_df)[1:w] = rev(names_trend)
  names(trend_df)[(w+1):(w+5)] = c("A", "B", "mk_pvalue", "S", "Class") 
  
  for (i in 1:(len-w)){#i=1
    mkTest = mk.test(timeSeries_df[i:(i+w-1)]) 
    pvalue = mkTest$p.value
    trend_df$mk_pvalue[i] = pvalue
    S = mkTest$estimates[1]
    trend_df$S[i] = S
    
    trend_df[i,1:w] = timeSeries_df[i:(i+w-1)]
    trend_df$nStepAhead[i] = timeSeries_df[i+w-1+nStepAhead]
    
    lm = lm(timeSeries_df[i:(i+w-1)] ~ seq(1, w))
    
    trend_df$A[i] = lm$coefficients[2]
    trend_df$B[i] = lm$coefficients[1]
    
    col = c("yellow", "red", "blue")
    
    #alpha = 0.05
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
  return(na.omit(trend_df))
}

generateGraph = function(matrix, timeSeries, w, title = "Incidence"){#i=1
  #matrix = trendAnalysis_df; timeSeries = mm14LogIncDia
  len = length(matrix$Class)
  #lenTs = length(timeSeries)
  drifPosition = NULL; c = 1;
  for (i in 1:(len-1)){
    if(matrix$Class[i] != matrix$Class[i+1]){
      drifPosition[c] = i; 
      c = c + 1
    }
  }#View(matrix)
  
  plot.ts(timeSeries)
  
  plot.ts(timeSeries, ylab = title)
  for (i in 1:length(drifPosition)) {
    abline(v = w + drifPosition[i], col = 2)
  }
  # lm = lm(timeSeries[(w+66-13):(w+66)] ~ seq(1, 14))
  # abline(lm, col = 2, lwd = 2)
  
  #return()
}

getSlidingWindow =  function(matrix = trendAnalysis_df, w = 14){
  slidingWindow_df = trendAnalysis_df[,1:w]
  
  for (i in 15:100){
    slidingWindow_df$target[i] = slidingWindow_df[,]
      
  }
}
