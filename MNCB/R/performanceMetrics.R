calculateMetrics = function(resultsMatrix){ #resultsMatrix = results
  metricsMatrix = data.frame(matrix(nrow = (length(resultsMatrix)-1), ncol = 5))
  rownames(metricsMatrix) = names(resultsMatrix)[2:(length(resultsMatrix))]
  colnames(metricsMatrix) = c('MSE', 'MAE', 'MAPE', 'ARV', 'THEIL')
  #resultsMatrixWONA = na.omit(resultsMatrix)
  
  for (i in 1:3) { #i=4
    for (j in 1:5) {
      if(j == 1){
        metricsMatrix[i,j] = getMSE(resultsMatrix[[i+1]], resultsMatrix$OBS)
      }
      if(j == 2){
        metricsMatrix[i,j] = getMAE(resultsMatrix[[i+1]], resultsMatrix$OBS)
      }
      if(j == 3){
        metricsMatrix[i,j] = getMAPE(resultsMatrix[[i+1]], resultsMatrix$OBS)
      }    
      if(j == 4){
        metricsMatrix[i,j] = getARV(resultsMatrix[[i+1]], resultsMatrix$OBS)
      }
      if(j == 5){
        metricsMatrix[i,j] = getTheil(resultsMatrix[[i+1]], resultsMatrix$OBS)
      }
    }
  }

# getMSE(onestep_ftsga, data_test) #FUZZY
# getMSE(onestep_arima, data_test) #ARIMA
# getMSE(onestep_ets, data_test) #ETS
# getMSE(onestep_nnar, data_test) #NNETAR
  
  return(metricsMatrix)  
}

getMAE = function(target,forecast){ # target = a; forecast = b
  values = na.omit(data.frame(target = target, forecast = forecast))
  MAE = sum(abs(values$target - values$forecast))/length(values$target)
  return(MAE)
}

getSE = function(target,forecast){
  SE=(target-forecast)^2
  return(SE)
}

getAPE = function(target,forecast){
  target[which(target==0)] = NA#1e-10*min(abs(target[target!=0]))
  APE=abs((target-forecast)/target)
  return(APE)  
}

getRV = function(target,forecast){
  n = length(target)
  meanT = numeric(n)
  for(t in 1:n){
    meanT[t] = mean(target[1:t], na.rm=TRUE)#now, meanT is the mean until instant t
  }
  RV=((target-forecast)^2)/(sum((forecast-meanT)^2, na.rm=TRUE))
  return(RV)
}

#Index of Disagreement (complementar to the Index of Agreement)
getID = function(target,forecast){
  SE = getSE(target,forecast)
  SSE = sum(SE, na.rm=TRUE)
  n = length(target)
  meanT = numeric(n)
  for(t in 1:n){
    meanT[t] = mean(target[1:t], na.rm=TRUE)#now, meanT is the mean until instant t
  }
  divisor = abs(forecast - meanT) + abs(target - meanT)
  divisor = divisor^2
  divisor = sum(divisor, na.rm = TRUE)
  ID = SSE/divisor
  return(ID)
}
getRegressionMetrics = function(target,forecast){
  linearModel = lm(target~forecast)
  coefficients = linearModel$coefficients
  anova = summary(linearModel)
  R2 = anova$r.squared
  ret = list()
  ret$Intercept = coefficients[1]
  ret$Slope = coefficients[2]
  ret$WR2 = 1-R2
  return(ret)
}
getMSE = function(target,forecast){
  SE = getSE(target,forecast)
  MSE=mean(SE, na.rm=TRUE)
  MSE
}
getMAPE = function(target,forecast){
  APE = getAPE(target,forecast)
  MAPE = mean(APE, na.rm=TRUE)
  return(100*MAPE)
}
getARV = function(target,forecast){
  RV = getRV(target,forecast)
  ARV=sum(RV, na.rm=TRUE)
  ARV
}
getTheil = function(target,forecast){
  seriesSize = length(target)
  squaredSumTF = 0
  squaredSumTT = 0
  i=2
  while(i<=seriesSize){
    squaredSumTF = squaredSumTF + (target[i]-forecast[i])^2
    squaredSumTT = squaredSumTT + (target[i]-target[i-1])^2
    #valor.theil[i]=((target[i]-forecast[i])^2)/(sum((target[i]-target[i+1])^2))
    i=i+1
  }
  Theil = squaredSumTF/squaredSumTT
  Theil
}
getPOCID = function(target,forecast){
  Dt = 0
  i=1
  seriesSize = length(target)
  while(i<seriesSize){
    TT = (target[i+1]-target[i])
    FF = (forecast[i+1]-forecast[i])
    #if((target[i+1]-target[i])*(forecast[i+1]-forecast[i])>0){
    if(TT*FF>0 | (TT==0 & FF==0)){
      Dt= Dt + 1 
    }
    i=i+1
    #print(i)
  }
  POCID=100*(Dt/(seriesSize-1))
  POCID
}
#WRONG Prediction on Change of Direction
getWPOCID = function(target,forecast){
  Dt = 0
  i=1
  seriesSize = length(target)
  while(i<seriesSize){
    TT = (target[i+1]-target[i])
    FF = (forecast[i+1]-forecast[i])
    #if((target[i+1]-target[i])*(forecast[i+1]-forecast[i])>0){
    if(TT*FF>0 | (TT==0 & FF==0)){
      Dt= Dt + 1 
    }
    i=i+1
    #print(i)
  }
  WPOCID=1-(Dt/(seriesSize-1))
  WPOCID
}
#Root Mean Squared Error (RMSE)
getRMSE = function(target,forecast){
  MSE = getMSE(target,forecast)
  RMSE = sqrt(MSE)
  return(RMSE)
}
#Normalized Mean Squared Error (NMSE)
getNMSE = function(target,forecast){
  MSE = getMSE(target,forecast)
  sampleVar = var(target, na.rm = TRUE)
  NMSE = MSE/sampleVar
  return(NMSE)
}
#Normalized Root Mean Squared Error (NRMSE)
getNRMSE = function(target, forecast){
  SE = getSE(target,forecast)
  ESS = sum(SE, na.rm=TRUE)
  meanT = mean(target, na.rm = TRUE)
  TSS = sum((target - meanT)^2, na.rm = TRUE)
  NRMSE = sqrt(ESS/TSS)
  return(NRMSE)
}

computePerformanceMetrics= function(all.series, from=1, to=n, phaseLabel = "Training"
                                    , seriesName=seriesNm_i, withNormalization=FALSE) {
  seriesNames = names(all.series)
  nSeries = length(seriesNames)
  targetName = seriesNames[1]
  target = all.series[[targetName]][from: to]
  MSE_row = NULL; MAPE_row = NULL; Theil_row = NULL; ARV_row = NULL; WPOCID_row = NULL; 
  REG_Intercept_row = NULL; REG_Slope_row = NULL; REG_WR2_row = NULL; ID_row = NULL
  for(i in 2:nSeries){
    forecasterName_i = seriesNames[i]
    forecast_i = all.series[[forecasterName_i]][from: to]
    target_i = target[!is.na(forecast_i)]
    forecast_i = forecast_i[!is.na(forecast_i)]
    MSE_row = c(MSE_row, getMSE(target=target_i, forecast=forecast_i))
    MAPE_row = c(MAPE_row, getMAPE(target=target_i, forecast=forecast_i))
    ARV_row = c(ARV_row, getARV(target=target_i, forecast=forecast_i))
    ID_row = c(ID_row, getID(target=target_i, forecast=forecast_i))
    
    Theil_row = c(Theil_row, getTheil(target=target_i, forecast=forecast_i))
    WPOCID_row = c(WPOCID_row, getWPOCID(target=target_i, forecast=forecast_i))
    
    RegMetrics_i = getRegressionMetrics(target=target_i, forecast=forecast_i)
    REG_Intercept_row = c(REG_Intercept_row, RegMetrics_i$Intercept)
    REG_Slope_row = c(REG_Slope_row, RegMetrics_i$Slope)
    REG_WR2_row = c(REG_WR2_row, RegMetrics_i$WR2)
  }
  #print(paste("Metric", phaseLabel, sep="."))
  colNames = seriesNames[2:nSeries]
  performanceMatrix = rbind(MSE= MSE_row
                            , MAPE= MAPE_row
                            , ARV=ARV_row
                            , ID=ID_row
                            , Theil=Theil_row
                            , WPOCID=WPOCID_row
                            , Reg_Intercept = REG_Intercept_row
                            , Reg_Slope = REG_Slope_row
                            , Reg_WR2 = REG_WR2_row)
  dimnames(performanceMatrix)[[2]] = colNames
  
  nMetrics = dim(performanceMatrix)[1]
  metricsNames = dimnames(performanceMatrix)[[1]]
  #NORMALIZED PERFORMANCE METRICS
  if(withNormalization){  
    n.performanceMatrix = NULL
    for(i in 1:nMetrics){
      row = performanceMatrix[i,]
      metricName_i = metricsNames[i]
      if(metricName_i=="Reg_Intercept"){ row = abs(row)}
      else if(metricName_i=="Reg_Slope"){ row = abs(abs(row)-1)}
      min_ = min(row, na.rm = TRUE); max_ = max(row, na.rm = TRUE)
      delta = max_ - min_
      row_norm = (row - min_)/delta
      n.performanceMatrix = rbind(n.performanceMatrix, row_norm)
    }
    dimnames(n.performanceMatrix)[[1]] = paste("n", metricsNames, sep = ".")
    nModels = dim(performanceMatrix)[2]
    colMean = NULL; colSd = NULL
    for(i in 1:nModels){
      colMean[i] = mean(n.performanceMatrix[,i], na.rm = TRUE)
      colSd[i] = sd(n.performanceMatrix[,i], na.rm = TRUE)
    }
    png(filename = paste(RESULTS_PATH, seriesName, "_", phaseLabel, "_normPerformance.png", sep=""), width = 2.7*480, height = 1.25*480)
    sortIndexes = order(colMean, decreasing = TRUE)
    oColMean = round(colMean[sortIndexes], 4)
    oColNames = colNames[sortIndexes]
    mar = c(15,11,1,1); mgp = c(4,1,0); par(family="serif", mar=mar, mgp=mgp, las=2, cex.axis=2)
    barplot(height = oColMean, names.arg = oColNames, axes=FALSE
            , horiz=TRUE, col="white")#, ylab="model", xlab = "aggregate performance metric")
    axis(1, at=oColMean, las=2
         , cex.lab=1, cex.axis = 2, cex=1)#, las=2)#lab=seq(from=1, to=seriesSize, by = by_x))
    title(xlab="aggregate performance metric", col.lab=rgb(0,0,0)
          , cex.lab=2, cex.axis = 2, cex=1, line = 9)#, las=2)
    title(ylab="model", col.lab=rgb(0,0,0)
          , cex.lab=2, cex.axis = 2, cex=1, line = 7)#, las=2)
    dev.off()
    n.performanceMatrix = rbind(n.performanceMatrix, n.Mean = colMean)
    n.performanceMatrix = rbind(n.performanceMatrix, n.Sd = colSd)
    performanceMatrix = rbind(performanceMatrix, n.performanceMatrix)
  }  
  #WORST AND BEST MODEL PER METRIC
  Worst = NULL; Best = NULL
  modelsNames = dimnames(performanceMatrix)[[2]]
  metricsNames = dimnames(performanceMatrix)[[1]]
  nMetrics = dim(performanceMatrix)[1]
  for(i in 1:nMetrics){#i=5
    row_i = performanceMatrix[i,]
    metricName_i = metricsNames[i]
    if(metricName_i=="Reg_Intercept"){ row_i = abs(row_i)}
    else if(metricName_i=="Reg_Slope"){ row_i = abs(abs(row_i)-1)}
    best_index = which(row_i==min(row_i, na.rm = TRUE))
    worst_index = which(row_i==max(row_i, na.rm = TRUE))
    best_row = modelsNames[best_index[1]]
    nBests = length(best_index)
    j=2
    while(j<=nBests){
      best_row = paste(best_row, modelsNames[best_index[j]], sep=", "); j = j+1
    }
    worst_row = modelsNames[worst_index[1]]
    nWorsts = length(worst_index)
    j=2
    while(j<=nWorsts){
      worst_row = paste(worst_row, modelsNames[worst_index[j]], sep=", "); j = j+1
    }
    Worst=rbind(Worst, worst_row)
    Best=rbind(Best, best_row)
  }
  df = NULL
  df[["Metric"]]=dimnames(performanceMatrix)[[1]]
  df[["Worst"]] = Worst[1:nMetrics]
  df[["Best"]] = Best[1:nMetrics]
  #performanceMatrix = round(performanceMatrix, 3)
  pm = as.data.frame(performanceMatrix)
  performanceMatrix = as.data.frame(cbind(as.data.frame(df), pm))
  #dimnames(performanceMatrix)[[2]] = c("Worst", "Best", modelsNames)
  
  #ExPORTING PERFORMANCE METRICS MATRIX
  resultName = paste(seriesName, "Performance", phaseLabel, sep="_")
  write.table(x = performanceMatrix, file = paste(RESULTS_PATH, resultName, ".csv", sep="")
              , sep="\t")#, row.names = TRUE)
  #View(performanceMatrix, title = resultName)
  return(performanceMatrix)
}
#the following method is customized for paper Ref. No.:  KNOSYS-D-19-00853
#Title: A temporal-window framework for modeling and forecasting time series
#Jurnal: Knowledge-Based Systems
computePerformanceMetrics02= function(all.series, from=1, to=n, phaseLabel = "Training"
                                    , seriesName=seriesNm_i, withNormalization=FALSE) {
  seriesNames = names(all.series)
  nSeries = length(seriesNames)
  targetName = seriesNames[1]
  target = all.series[[targetName]][from: to]
  MAPE_row = NULL; POCID_row = NULL; Theil_row = NULL; 
  NRMSE_row = NULL; NMSE_row = NULL; RMSE_row = NULL; REG_R2_row = NULL
  for(i in 2:nSeries){
    forecasterName_i = seriesNames[i]
    forecast_i = all.series[[forecasterName_i]][from: to]
    target_i = target[!is.na(forecast_i)]
    forecast_i = forecast_i[!is.na(forecast_i)]
    MAPE_row = c(MAPE_row, 100*getMAPE(target=target_i, forecast=forecast_i))
    POCID_row = c(POCID_row, getPOCID(target=target_i, forecast=forecast_i))
    Theil_row = c(Theil_row, getTheil(target=target_i, forecast=forecast_i))
    NRMSE_row = c(NRMSE_row, getNRMSE(target=target_i, forecast=forecast_i))
    NMSE_row = c(NMSE_row, getNMSE(target=target_i, forecast=forecast_i))
    RMSE_row = c(RMSE_row, getRMSE(target=target_i, forecast=forecast_i))
    RegMetrics_i = getRegressionMetrics(target=target_i, forecast=forecast_i)
    REG_R2_row = c(REG_R2_row, (1-RegMetrics_i$WR2))
  }
  #print(paste("Metric", phaseLabel, sep="."))
  colNames = seriesNames[2:nSeries]
  performanceMatrix = rbind(MAPE= MAPE_row
                            , POCID=POCID_row
                            , Theil=Theil_row
                            , NRMSE = NRMSE_row
                            , NMSE = NMSE_row
                            , RMSE = RMSE_row
                            , Reg_R2 = REG_R2_row)
  dimnames(performanceMatrix)[[2]] = colNames
  
  #NORMALIZED PERFORMANCE METRICS
  if(withNormalization){  
    nMetrics = dim(performanceMatrix)[1]
    metricsNames = dimnames(performanceMatrix)[[1]]
    n.performanceMatrix = NULL
    for(i in 1:nMetrics){
      row = performanceMatrix[i,]
      metricName_i = metricsNames[i]
      if(metricName_i=="Reg_Intercept"){ row = abs(row)}
      else if(metricName_i=="Reg_Slope"){ row = abs(abs(row)-1)}
      min_ = min(row, na.rm = TRUE); max_ = max(row, na.rm = TRUE)
      delta = max_ - min_
      row_norm = (row - min_)/delta
      n.performanceMatrix = rbind(n.performanceMatrix, row_norm)
    }
    dimnames(n.performanceMatrix)[[1]] = paste("norm", metricsNames, sep = ".")
    nModels = dim(performanceMatrix)[2]
    colMean = NULL; colSd = NULL
    for(i in 1:nModels){
      colMean[i] = mean(n.performanceMatrix[,i], na.rm = TRUE)
      colSd[i] = sd(n.performanceMatrix[,i], na.rm = TRUE)
    }
    n.performanceMatrix = rbind(n.performanceMatrix, n.Mean = colMean)
    n.performanceMatrix = rbind(n.performanceMatrix, n.Sd = colSd)
    performanceMatrix = rbind(performanceMatrix, n.performanceMatrix)
  }  
  performanceMatrix = round(performanceMatrix, 3)
  #WORST AND BEST MODEL PER METRIC
  Worst = NULL; Best = NULL
  modelsNames = dimnames(performanceMatrix)[[2]]
  nMetrics = dim(performanceMatrix)[1]
  for(i in 1:nMetrics){#i=5
    row_i = performanceMatrix[i,]
    best_index = which(row_i==min(row_i, na.rm = TRUE))
    worst_index = which(row_i==max(row_i, na.rm = TRUE))
    best_row = modelsNames[best_index[1]]
    nBests = length(best_index)
    j=2
    while(j<=nBests){
      best_row = paste(best_row, modelsNames[best_index[j]], sep=", "); j = j+1
    }
    worst_row = modelsNames[worst_index[1]]
    nWorsts = length(worst_index)
    j=2
    while(j<=nWorsts){
      worst_row = paste(worst_row, modelsNames[worst_index[j]], sep=", "); j = j+1
    }
    Worst=rbind(Worst, worst_row)
    Best=rbind(Best, best_row)
  }
  df = NULL
  df[["Metric"]]=dimnames(performanceMatrix)[[1]]
  df[["Worst"]] = Worst[1:nMetrics]
  df[["Best"]] = Best[1:nMetrics]
  pm = as.data.frame(performanceMatrix)
  performanceMatrix = as.data.frame(cbind(as.data.frame(df), pm))
  #dimnames(performanceMatrix)[[2]] = c("Worst", "Best", modelsNames)
  
  #ExPORTING PERFORMANCE METRICS MATRIX
  resultName = paste(seriesName, "Performance", phaseLabel, sep="_")
  write.csv(x = performanceMatrix, file = paste(RESULTS_PATH, resultName, ".csv", sep="")
            , row.names = TRUE)
  View(performanceMatrix, title = resultName)
}
