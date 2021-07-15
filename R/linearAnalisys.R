generateImg = function(timeSeries_df){
  #timeSeries_df = incDia
  timeSeries_df = na.omit(timeSeries_df)
  len = length(timeSeries_df)
  w = 21
  
  for (i in 1:(len-w)){#i=9
    mkTest = mk.test(timeSeries_df[i:(i+w-1)]) 
    pvalue = mkTest$p.value
    S = mkTest$estimates[1]
    
    lm = lm(timeSeries_df[i:(i+w-1)] ~ seq(1, w))
    
    col = c("yellow", "red", "blue")
    
    alpha = 0.05
    if(pvalue > alpha | is.na(pvalue)){
      alphaColor = col[1]
    }else{
      if(S >= 0){
        alphaColor = col[2]
      }else{
        alphaColor = col[3]
      }
    }
    
    
    png(paste("Results/linearCoef_img/cl_", i, ".png",sep = ""))
    plot(timeSeries_df[i:(i+w-1)],# ~ i:(i+w-1),
         xlab = "Observação",
         ylab = "Incidência", 
         lwd = 4, pch = 19,
         axes = T)
    abline(lm, col = alphaColor, lwd = 4)
    if(max(timeSeries_df[i:(i+w-1)])==0){
      text(paste("lc = ", round(lm$coefficients[2], 3), sep=""),
           x = (5), y = 1)
    }else{
      text(paste("lc = ", round(lm$coefficients[2], 3), sep=""),
           x = (5), y = max(max(timeSeries_df[i:(i+w-1)])))
    }
    dev.off()
  }
  
}


function(timeSeries_df_2){
  #timeSeries_df = incDia
  timeSeries_df = na.omit(timeSeries_df)
  len = length(timeSeries_df)
  w = 30

  linearCoeficient = NULL
  for (i in 1:(len-w)){#i=4
    lm = lm(timeSeries_df[i:(i+w-1)] ~ seq(1, w))
    #notrend_test(timeSeries_df, test = "t")$p.value #test = c("t", "MK", "WAVK") 
    
    col = c( "blue", "yellow", "red")
    
    limite = 20
    if(as.numeric(lm$coefficients[2]) <= -limite){
      alphaColor = col[1]
    }else{
      if(as.numeric(lm$coefficients[2]) >= limite){
        alphaColor = col[3]
      }else{
        alphaColor = col[2]
      }
    }
    
    
    png(paste("Results/linearCoef_img/cl_", i, ".png",sep = ""))
    plot(timeSeries_df[i:(i+w-1)],# ~ i:(i+w-1),
         xlab = "Observação",
         ylab = "Incidência", 
         lwd = 4, pch = 19,
         axes = T)
    abline(lm, col = alphaColor, lwd = 4)
    text(paste("lc = ", round(lm$coefficients[2], 3), sep=""), 
         x = (5),
         y = max(max(timeSeries_df[i:(i+w-1)]), 1)
         )
    dev.off()
  
  }
  

}
