KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")
pGSA = list(); pGSA$max.call = 10000; pGSA$max.time = 20
pGSA$maxit = 10; pGSA$nb.stop.improvement = 5; 
pGSA$temperature = 10

getSVMPar_GenSA = function(dataTrain, dataValid, nStepAhead=1
                           , alpha=0.05){
  #dataTrain = normTrain; dataValid = normValid; nStepAhead = 1
  
  set.seed(123)
  
  fitnessSVR = function(parameters){
    #data = normTrain; parameters = c(15, 15, 0.03, 10, 10, 10, 1, 0.5, 1)
    #nStepAhead = floor(parameters[10])
    #phi = floor(parameters[1])
    
    runningMeanincDia = getRunningMean(dataTrain, phi)
    #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
    
    #w = floor(parameters[2])
    #alpha = parameters[3]
    
    timeSeriesnName = "runningMeanincDia"
    timeSeries = runningMeanincDia
    title = paste("Rolling ",phi,"-day average", sep="")
    trendAnalysis_df = getTrendAnalysis(timeSeries_ts = timeSeries, w = w, alpha = alpha) 
    #View(trendAnalysis_df)
    
    # Create Sliding window matrix
    trainTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = dataTrain, 
                                             w = w, 
                                             alpha = alpha,
                                             nStepAhead = nStepAhead)
    
    validTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = dataValid, 
                                             w = w, 
                                             alpha = alpha,
                                             nStepAhead = nStepAhead) 
    
    #dataTrain = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == class),]
    #dataValid = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == class),]
    #View(formula_svm)
    
    svmParameters = list()
    #svmParameters$phi = (parameters[1])
    #svmParameters$w = (parameters[2])
    #svmParameters$alpha = (parameters[3])
    svmParameters$cost = (parameters[1])#integer in [1, 15]
    svmParameters$epsilon = (parameters[2])#integer in [1, 15]
    svmParameters$gamma = (parameters[3])#integer in [1, 15]
    svmParameters$degree = (parameters[4])#integer in [1, 15]
    svmParameters$coef0 = (parameters[5])#integer in [1, 15]
    index = floor(parameters[6])#in [-.5+1e-10, 5.5-1e-10]
    svmParameters$KernelFunction = KernelFunctionsLabels[index]

    dataTrain_fit_model = trainTrendAnalysis_df[,c(1:w,w+6)]
    modelSVM = svm(formula = nStepAhead ~ .
                   , x = dataTrain_fit_model
                   , cost = as.numeric(svmParameters[1])
                   , epsilon =as.numeric(svmParameters[2])
                   , gamma =as.numeric(svmParameters[3])
                   , degree = as.numeric(svmParameters[4])
                   , coef0 =  as.numeric(svmParameters[5])
                   , kernel = svmParameters$KernelFunction)
    
    predSVM_train = predict(modelSVM, dataTrain_fit_model)
    
    dataValid_fit_model = validTrendAnalysis_df[,c(1:w,w+6)]
    predSVM_valid = predict(modelSVM, dataValid_fit_model)
    MSE = getMSE(target = dataValid_fit_model$nStepAhead, forecast = predSVM_valid)
    return(MSE)
  }

  min_cost = 1e-5; max_cost = 1e+2
  min_epsilon = 1e-5; max_epsilon = 1
  min_gamma = 1e-5; max_gamma = 1e+4
  min_degree = 1; max_degree = 5
  min_coef0 = 0; max_coef0 = 1e+1
  max_KernelFunctions = (length(KernelFunctionsLabels)+1-1e-5)
  lowers = c(min_cost, min_epsilon, min_gamma, min_degree, min_coef0)
  uppers = c(max_cost, max_epsilon, max_gamma, max_degree, max_coef0)
  lowers = c(lowers, 1)
  uppers = c(uppers, max_KernelFunctions)
  tol <- 1e-3
  modellingTime = proc.time()[[3]]
  out = 
    tryCatch({
      expr = GenSA(lower = lowers, upper = uppers
                   , fn = fitnessSVR
                   , par = (lowers+uppers)/2
                   , control=list(max.call = pGSA$max.call
                                  , max.time = pGSA$max.time
                                  , maxit = pGSA$maxit
                                  , verbose = FALSE 
                                  , smooth = FALSE
                                  , seed = 123
                                  , nb.stop.improvement = pGSA$nb.stop.improvement
                                  , temperature = pGSA$temperature))
    }, 
    error = function(e){
      message(paste("Error in GenSA.", e)); 
      return(return(NA))
    } 
    #, finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
    )
  modellingTime = proc.time()[[3]] - modellingTime#time in seconds
  modellingTime = as.numeric(modellingTime) 
  
  optimal = list()
  optimal$modellingTime = modellingTime
  #optimal$forecastingTime = NA
  optimal$GenSA_output = out
  optimal$svmParameters = out$par
  return(optimal)
  return(optimal)
}

getSVR_MKCD_par = function(dataTrain, dataValid, w, phi
                           , Class, nStepAhead=1, alpha=0.05){
  #data = normTrain; nStepAhead = 7
  
  set.seed(123)

  fitnessSVR = function(parameters){
    #data = normTrain; parameters = c(1, 2, 1, 1.5, 1.5, 2)
    #Class = "None"; w = 7; phi = 14; alpha = 0.05
     
    runningMeanincDia = getRunningMean(normTrain, phi)
    #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
    #w = w; alpha = alpha; nStepAhead = 1
    
    timeSeriesnName = "runningMeanincDia"
    timeSeries = runningMeanincDia
    title = paste("Rolling ", phi, "-day average", sep="")
    trendAnalysis_df = getTrendAnalysis(timeSeries_ts = normTrain, w = w
                                        , alpha = alpha, nStepAhead = 1) 
    #View(trendAnalysis_df)

    # Create Sliding window matrix
    trainTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = normTrain, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = 1)
    
    validTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = normValid, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = 1) 

    dataTrain_fit = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == Class),]
    dataValid_fit = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == Class),]
    #View(dataTrain_fit)
  
    svmParameters = list()
    #svmParameters$phi = (parameters[1])
    #svmParameters$w = (parameters[2])
    #svmParameters$alpha = (parameters[3])
    svmParameters$cost = (parameters[1])#integer in [1, 15]
    svmParameters$epsilon = (parameters[2])#integer in [1, 15]
    svmParameters$gamma = (parameters[3])#integer in [1, 15]
    svmParameters$degree = (parameters[4])#integer in [1, 15]
    svmParameters$coef0 = (parameters[5])#integer in [1, 15]
    index = floor(parameters[6])#in [-.5+1e-10, 5.5-1e-10]
    svmParameters$KernelFunction = KernelFunctionsLabels[index]
    
    formula_svm = "nStepAhead ~ "
    for(i in 1:w){#i=1
      if(i == 1){
        formula_svm = paste(formula_svm, paste(colnames(dataTrain_fit)[i], sep=""), sep="") 
      }else{
        formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_fit)[i], sep=""), sep="") 
      }
    }
    
    dataTrain_fit_model = data.frame(dataTrain_fit[,c(1:w,w+6)])
    
    if(svmParameters$KernelFunction == 'linear'){
      modelSVM = svm(formula(formula_svm)
                     , dataTrain_fit_model #View(dataTrain_fit_model)
                     , kernel = svmParameters$KernelFunction
                     , cost = as.numeric(svmParameters[1])
                     , epsilon = as.numeric(svmParameters[2]))
    }
    if(svmParameters$KernelFunction == 'polynomial'){
      modelSVM = svm(formula(formula_svm)
                     , dataTrain_fit_model #View(dataTrain_fit_model)
                     , kernel = svmParameters$KernelFunction
                     , gamma = as.numeric(svmParameters[3])
                     , coef0 = as.numeric(svmParameters[5])
                     , degree = as.numeric(svmParameters[4])
                     , epsilon = as.numeric(svmParameters[2]))
    }
    if(svmParameters$KernelFunction == 'radial'){
      modelSVM = svm(formula(formula_svm)
                     , dataTrain_fit_model #View(dataTrain_fit_model)
                     , kernel = svmParameters$KernelFunction
                     , gamma = as.numeric(svmParameters[3]))
    }
    if(svmParameters$KernelFunction == 'sigmoid'){
      modelSVM = svm(formula(formula_svm)
                     , dataTrain_fit_model #View(dataTrain_fit_model)
                     , kernel = svmParameters$KernelFunction
                     , gamma = as.numeric(svmParameters[3])
                     , coef0 = as.numeric(svmParameters[5])
                     , epsilon = as.numeric(svmParameters[2]))
    }
    #modelSVM             
    predSVM_train = predict(modelSVM, dataTrain_fit_model)
    #plot.ts(dataTrain_fit_model$nStepAhead); lines(predSVM_train, col=2)
    dataValid_fit_model = dataValid_fit[,c(1:w,w+6)]
    predSVM_valid = predict(modelSVM, dataValid_fit_model)
    #plot.ts(dataValid_fit_model$nStepAhead); lines(predSVM_valid, col=2)
    length(dataValid_fit_model$nStepAhead)
    length(predSVM_valid)
    MSE = getMSE(target = dataValid_fit_model$nStepAhead, forecast = predSVM_valid)
    return(MSE)
  }

  # min_phi = 7; max_phi = 30+1-1e-5
  # min_w = 7; max_w = 30+1-1e-5
  # min_alpha = 0.01; max_alpha  = 0.2
  min_cost = 1e-5; max_cost = 1e+2
  min_epsilon = 1e-5; max_epsilon = 1
  min_gamma = 1e-5; max_gamma = 1e+4
  min_degree = 1; max_degree = 5
  min_coef0 = 0; max_coef0 = 1e+1
  max_KernelFunctions = (length(KernelFunctionsLabels)+1-1e-5)
  lowers = c(min_cost, min_epsilon, min_gamma, min_degree, min_coef0)
  uppers = c(max_cost, max_epsilon, max_gamma, max_degree, max_coef0)
  lowers = c(lowers, 1)
  uppers = c(uppers, max_KernelFunctions)
  tol <- 1e-3
  modellingTime = proc.time()[[3]]
  out = 
    tryCatch({
      expr = GenSA(lower = lowers, upper = uppers
                   , fn = fitnessSVR
                   , par = (lowers+uppers)/2
                   , control=list(max.call = pGSA$max.call
                                  , max.time = pGSA$max.time
                                  , maxit = pGSA$maxit
                                  , verbose = FALSE 
                                  , smooth = FALSE
                                  , seed = 123
                                  , nb.stop.improvement = pGSA$nb.stop.improvement
                                  , temperature = pGSA$temperature))
    }, 
    error = function(e){
      message(paste("Error in GenSA.", e)); 
      return(return(NA))
    } 
    #, finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
    )
  modellingTime = proc.time()[[3]] - modellingTime#time in seconds
  modellingTime = as.numeric(modellingTime) 
  
  optimal = list()
  optimal$modellingTime = modellingTime
  #optimal$forecastingTime = NA
  optimal$GenSA_output = out
  optimal$svmParameters = out$par
  return(optimal)
  }

getModelSVR_MKCD = function(dataTrain, dataValid, dataTest, w, phi, 
                            , nStepAhead=1, alpha=0.05){
  #dataTrain = normTrain; dataValid = normValid;  
  #w = 7; phi = 14; alpha = 0.05; nStepAhead = 1

  nonePar = getSVR_MKCD_par(dataTrain, dataValid, w = w, phi = phi
                            , Class = "None", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , Class = "None"
                            , phi = phi, w = w
                            , alpha = alpha
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:w){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelNone = svm(formula(formula_svm), data = dataTrain_final
                  , cost = nonePar$svmParameters[1], epsilon = nonePar$svmParameters[2]
                  , gamma = nonePar$svmParameters[3], degree = nonePar$svmParameters[4]
                  , coef0 = nonePar$svmParameters[5]
                  , kernel = KernelFunctionsLabels[floor(nonePar$svmParameters[6])])
  #plot.ts(dataTrain_final$nStepAhead); lines(modelNone$fitted, col=2)
  print("modelNone MKCD: OK")
  
  # Positive model
  positivePar = getSVR_MKCD_par(dataTrain, dataValid, w = w, phi = phi
                                , Class = "Positive", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , phi = phi
                            , w = w
                            , alpha = alpha
                            , Class = "Positive"
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:w){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelPositive = svm(formula(formula_svm), data = dataTrain_final
                  , cost = positivePar$svmParameters[1], epsilon = positivePar$svmParameters[2]
                  , gamma = positivePar$svmParameters[3], degree = positivePar$svmParameters[4]
                  , coef0 = positivePar$svmParameters[5]
                  , kernel = KernelFunctionsLabels[floor(positivePar$svmParameters[6])])
  #plot.ts(dataTrain_final$nStepAhead); lines(modelPositive$fitted, col=2)
  print("modelPositive MKCD: OK")
  #length(dataTrain_final$nStepAhead); length(modelPositive$fitted)
  # Model Negative
  #modelNegative= svm(formula(formula), data = negativeTrendTrain)
  negativePar = getSVR_MKCD_par(dataTrain, dataValid, w = w, phi = phi
                                , Class = "Negative", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , phi = phi
                            , w = w
                            , alpha = alpha
                            , Class = "Negative"
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:w){#i=2
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelNegative = svm(formula(formula_svm), data = dataTrain_final
                  , cost = negativePar$svmParameters[1], epsilon = negativePar$svmParameters[2]
                  , gamma = negativePar$svmParameters[3], degree = negativePar$svmParameters[4]
                  , coef0 = negativePar$svmParameters[5]
                  , kernel = KernelFunctionsLabels[floor(negativePar$svmParameters[6])])
  #plot.ts(dataTrain_final$nStepAhead); lines(modelNegative$fitted, col=2)
  print("modelNegative MKCD: OK")
  
  # Model Normal 
  normalPar = getSVMPar_GenSA(dataTrain, dataValid, nStepAhead) #View(normTrain)
  finalData  = getFinalDataAll(time_series_train = dataTrain
                                  , time_series_test = dataValid
                                  , phi = phi
                                  , w = w
                                  , alpha = alpha
                                  , nStepAhead = 1)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:w){#i=2
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  modelNormal = svm(formula(formula_svm), data = dataTrain_final
                    , cost = as.numeric(normalPar$svmParameters[1])
                    , epsilon = as.numeric(normalPar$svmParameters[2])
                    , gamma = as.numeric(normalPar$svmParameters[3])
                    , degree = as.numeric(normalPar$svmParameters[4])
                    , coef0 = as.numeric(normalPar$svmParameters[5])
                    , kernel = KernelFunctionsLabels[floor(normalPar$svmParameters[6])])
  #plot.ts(dataTrain_final$nStepAhead); lines(modelNormal$fitted, col=2)
  print("modelNormal: OK")
  
  print('ATÉ AQUI FOI')
  
  predNormal_out = predict(modelNormal, dataValid_final)
  predNormal_outTrain = predict(modelNormal, dataTrain_final)
  
  #wFinal = floor(max(nonePar$svmParameters[2]
  #                   , positivePar$svmParameters[2]
  #                   , negativePar$svmParameters[2]))
  # 
   
  finalData  = getFinalDataAll(time_series_train = dataTrain
                               , time_series_test = dataValid
                               , phi = phi
                               , w = w
                               , alpha = alpha
                               , nStepAhead = 1)
  # MKCD - TRAIN SET
  pred_out_Train = NULL
  for(i in 1:length(finalData$dataTrain$nStepAhead)){#i=1
    if(finalData$dataTrain$Class[i] == "None"){
      pred_out_Train[i] = modelNone %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
    }else{
      if(finalData$dataTrain$Class[i] == "Positive"){
        pred_out_Train[i] = modelPositive %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
      }else{
        pred_out_Train[i] = modelNegative %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
      }
    }
  } #length(pred_out)
  # pred_out_Train = NULL
  # for(i in 1:length(normTrain$nStepAhead)){#i=1
  #   if(normTrain$Class[i] == "None"){
  #     pred_out_Train[i] = modelNone %>% predict(as.matrix(normTrain[i,1:w]))
  #   }else{
  #     if(normTrain$Class[i] == "Positive"){
  #       pred_out_Train[i] = modelPositive %>% predict(as.matrix(normTrain[i,1:w]))
  #     }else{
  #       pred_out_Train[i] = modelNegative %>% predict(as.matrix(normTrain[i,1:w]))
  #     }
  #   }
  # } #length(pred_out)
  # 
  
  # MKCD - TEST SET
  pred_out = NULL
  for(i in 1:length(finalData$dataTrain$nStepAhead)){#i=1
    if(finalData$dataTrain$Class[i] == "None"){
      pred_out_Train[i] = modelNone %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
    }else{
      if(finalData$dataTrain$Class[i] == "Positive"){
        pred_out_Train[i] = modelPositive %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
      }else{
        pred_out_Train[i] = modelNegative %>% predict(as.matrix(finalData$dataTrain[i,1:w]))
      }
    }
  } #length(pred_out)
  

  plot(finalData$dataTrain$nStepAhead, type="l", ylab = paste("Rolling ",phi,"-day average", sep=''),
       lwd = 3)#, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
  #       1.1*max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
  lines(pred_out_Train, col=2, lwd=2)
  points(pred_out_Train, col=2, pch=15)
  lines(predNormal_out, col=3, lwd=2)
  points(predNormal_out, col=3, pch=16)
  legend("topleft", c("MKCD - SVM", "SVM"),
         col=c(2,3),  pch=c(15,16), lty=1, lwd=3, cex = 0.9,
         box.col = "white", inset = 0.01, horiz = T)
  dev.off()
  
  
  
  #png(paste("Results/Figures/", country, "_", nStepAhead ,"sta_"
  #          , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
  plot(finalData_train$dataTest$nStepAhead, type="l", ylab = paste("Rolling ",phi,"-day average", sep=''),
       lwd = 3)#, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
                #       1.1*max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
  lines(pred_out, col=2, lwd=2)
  points(pred_out, col=2, pch=15)
  lines(predNormal_out, col=3, lwd=2)
  points(predNormal_out, col=3, pch=16)
  legend("topleft", c("MKCD - SVM", "SVM"),
         col=c(2,3),  pch=c(15,16), lty=1, lwd=3, cex = 0.9,
         box.col = "white", inset = 0.01, horiz = T)
  dev.off()
  
  metricsTable = as.data.frame(matrix(ncol=2, nrow=4))
  colnames(metricsTable) = c("MKCD_SVM", "SVM")
  rownames(metricsTable) = c("RMSE", "MAE", "MAPE", "ARV")
  metricsTable$SVM[1] = getRMSE(normTest$nStepAhead, predNormal_out) #Normal
  metricsTable$MKCD_SVM[1] = getRMSE(normTest$nStepAhead, pred_out)
  metricsTable$SVM[2] = getMAE(normTest$nStepAhead, predNormal_out) #Normal
  metricsTable$MKCD_SVM[2] = getMAE(normTest$nStepAhead, pred_out)
  metricsTable$SVM[3] = getMAPE(normTest$nStepAhead, predNormal_out) #Normal
  metricsTable$MKCD_SVM[3] = getMAPE(normTest$nStepAhead, pred_out)
  metricsTable$SVM[4] = getARV(normTest$nStepAhead, predNormal_out) #Normal
  metricsTable$MKCD_SVM[4] = getARV(normTest$nStepAhead, pred_out)
  options(scipen=999)
  print(round(metricsTable,3))
  
  write.csv(metricsTable, file = paste("Results/", country, "_", nStepAhead ,"sta_"
                                       , "w-", w, "_phi-", phi, "_alpha-", alpha,".csv", sep=""))
  
  return(list("MKCD_SVM_Test" = pred_out, "SVM_Test" = predNormal_out
              , "MKCD_SVM_Train" = pred_out_Train, "SVM_Train" = predNormal_outTrain
              , "Target_Test" = normTest$nStepAhead, "Target_Train" = normTrain$nStepAhead))
}




#model = getSVMmodel_GenSA(noneTrendTrain)

