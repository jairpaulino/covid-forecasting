KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")
pGSA = list(); pGSA$max.call = 10000; pGSA$max.time = 20
pGSA$maxit = 10; pGSA$nb.stop.improvement = 5; 
pGSA$temperature = 10

getSVR_MKCD_par = function(dataTrain, dataValid, class, nStepAhead){
  #data = normTrain; nStepAhead = 7
  
  set.seed(123)

  fitnessSVR = function(parameters){
    #data = normTrain; parameters = c(15, 15, 0.03, 10, 10, 10, 1, 0.5, 1)
    #class = "None"
    
    phi = floor(parameters[1])
     
    runningMeanincDia = getRunningMean(countryTimeSeries, phi)
    #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
    
    w = floor(parameters[2])
    alpha = parameters[3]
    
    timeSeriesnName = "runningMeanincDia"
    timeSeries = runningMeanincDia
    title = paste("Rolling ",phi,"-day average", sep="")
    trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
    #View(trendAnalysis_df)

    # Create Sliding window matrix
    trainTrendAnalysis_df = getTrendAnalysis(timeSeries_df = dataTrain, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = nStepAhead)
    
    validTrendAnalysis_df = getTrendAnalysis(timeSeries_df = dataValid, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = nStepAhead) 

    dataTrain = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == class),]
    dataValid = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == class),]
    #View(formula_svm)
    
    formula_svm = "nStepAhead ~ "
    for(i in 1:w){
      if(i == 1){
        formula_svm = paste(formula_svm, paste(colnames(dataTrain)[i], sep=""), sep="") 
      }else{
        formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain)[i], sep=""), sep="") 
      }
    }
    
    #formula_svm <<- formula_svm
    
    svmParameters = list()
    svmParameters$phi = (parameters[1])
    svmParameters$w = (parameters[2])
    svmParameters$alpha = (parameters[3])
    svmParameters$cost = (parameters[4])#integer in [1, 15]
    svmParameters$epsilon = (parameters[5])#integer in [1, 15]
    svmParameters$gamma = (parameters[6])#integer in [1, 15]
    svmParameters$degree = (parameters[7])#integer in [1, 15]
    svmParameters$coef0 = (parameters[8])#integer in [1, 15]
    index = floor(parameters[9])#in [-.5+1e-10, 5.5-1e-10]
    svmParameters$KernelFunction = KernelFunctionsLabels[index]
    
    modelSVM = svm(formula = formula(formula_svm)
                   , data = dataTrain
                   , cost = svmParameters[4]
                   , epsilon = svmParameters[5]
                   , gamma = svmParameters[6]
                   , degree = svmParameters[7]
                   , coef0 = svmParameters[8]
                   , kernel = svmParameters$KernelFunction)
    
    
    predSVM = predict(modelSVM, dataValid)
    MSE = getMSE(target = dataValid$nStepAhead, forecast = predSVM)
    return(MSE)
  }

  min_phi = 7; max_phi = 30+1-1e-5
  min_w = 7; max_w = 30+1-1e-5
  min_alpha = 0.01; max_alpha  = 0.2
  min_cost = 1e-5; max_cost = 1e+2
  min_epsilon = 1e-5; max_epsilon = 1
  min_gamma = 1e-5; max_gamma = 1e+4
  min_degree = 1; max_degree = 5
  min_coef0 = 0; max_coef0 = 1e+1
  max_KernelFunctions = (length(KernelFunctionsLabels)+1-1e-5)
  lowers = c(min_phi, min_w, min_alpha, min_cost, min_epsilon, min_gamma, min_degree, min_coef0)
  uppers = c(max_phi, max_w, max_alpha, max_cost, max_epsilon, max_gamma, max_degree, max_coef0)
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

getSVMPar_GenSA = function(dataTrain, dataValid, class, nStepAhead){
  #data = normTrain; nStepAhead = 7
  
  set.seed(123)
  
  fitnessSVR = function(parameters){
    #data = normTrain; parameters = c(15, 15, 0.03, 10, 10, 10, 1, 0.5, 1)
    #class = "None"
    nStepAhead = floor(parameters[10])
    phi = floor(parameters[1])
    
    runningMeanincDia = getRunningMean(countryTimeSeries, phi)
    #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
    
    w = floor(parameters[2])
    alpha = parameters[3]
    
    timeSeriesnName = "runningMeanincDia"
    timeSeries = runningMeanincDia
    title = paste("Rolling ",phi,"-day average", sep="")
    trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
    #View(trendAnalysis_df)
    
    # Create Sliding window matrix
    trainTrendAnalysis_df = getTrendAnalysis(timeSeries_df = dataTrain, 
                                             w = w, 
                                             alpha = alpha,
                                             nStepAhead = nStepAhead)
    
    validTrendAnalysis_df = getTrendAnalysis(timeSeries_df = dataValid, 
                                             w = w, 
                                             alpha = alpha,
                                             nStepAhead = nStepAhead) 
    
    #dataTrain = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == class),]
    #dataValid = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == class),]
    #View(formula_svm)
    
    formula_svm = "nStepAhead ~ "
    for(i in 1:w){
      if(i == 1){
        formula_svm = paste(formula_svm, paste(colnames(dataTrain)[i], sep=""), sep="") 
      }else{
        formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain)[i], sep=""), sep="") 
      }
    }
    
    #formula_svm <<- formula_svm
    
    svmParameters = list()
    svmParameters$phi = (parameters[1])
    svmParameters$w = (parameters[2])
    svmParameters$alpha = (parameters[3])
    svmParameters$cost = (parameters[4])#integer in [1, 15]
    svmParameters$epsilon = (parameters[5])#integer in [1, 15]
    svmParameters$gamma = (parameters[6])#integer in [1, 15]
    svmParameters$degree = (parameters[7])#integer in [1, 15]
    svmParameters$coef0 = (parameters[8])#integer in [1, 15]
    index = floor(parameters[9])#in [-.5+1e-10, 5.5-1e-10]
    svmParameters$KernelFunction = KernelFunctionsLabels[index]
    
    modelSVM = svm(formula = formula(formula_svm)
                   , data = dataTrain
                   , cost = svmParameters[4]
                   , epsilon = svmParameters[5]
                   , gamma = svmParameters[6]
                   , degree = svmParameters[7]
                   , coef0 = svmParameters[8]
                   , kernel = svmParameters$KernelFunction)
    
    
    predSVM = predict(modelSVM, dataValid)
    MSE = getMSE(target = dataValid$nStepAhead, forecast = predSVM)
    return(MSE)
  }
  
  min_phi = 7; max_phi = 30+1-1e-5
  min_w = 7; max_w = 30+1-1e-5
  min_alpha = 0.01; max_alpha  = 0.2
  min_cost = 1e-5; max_cost = 1e+2
  min_epsilon = 1e-5; max_epsilon = 1
  min_gamma = 1e-5; max_gamma = 1e+4
  min_degree = 1; max_degree = 5
  min_coef0 = 0; max_coef0 = 1e+1
  max_KernelFunctions = (length(KernelFunctionsLabels)+1-1e-5)
  lowers = c(min_phi, min_w, min_alpha, min_cost, min_epsilon, min_gamma, min_degree, min_coef0)
  uppers = c(max_phi, max_w, max_alpha, max_cost, max_epsilon, max_gamma, max_degree, max_coef0)
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

getModelSVR_MKCD = function(dataTrain, dataValid, nStepAhead){
  #dataTrain = normTrain; dataValid = normValid;  nStepAhead = 7
  
  nonePar = getSVR_MKCD_par(dataTrain, dataValid, class = "None", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , phi = nonePar$svmParameters[1]
                            , w = nonePar$svmParameters[2]
                            , alpha = nonePar$svmParameters[3]
                            , class = "None"
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:floor(nonePar$svmParameters[2])){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelNone = svm(formula(formula_svm), data = dataTrain_final
                  , cost = nonePar$svmParameters[4], epsilon = nonePar$svmParameters[5]
                  , gamma = nonePar$svmParameters[6], degree = nonePar$svmParameters[7]
                  , coef0 = nonePar$svmParameters[8]
                  , kernel = KernelFunctionsLabels[floor(nonePar$svmParameters[9])])
  
  print("modelNone MKCD: OK")
  
  # Modelo Positive
  #modelPositive = svm(formula(formula), data = positiveTrendTrain)
  modelPositive = getSVR_MKCD_par(dataTrain, dataValid, class = "Positive", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , phi = nonePar$svmParameters[1]
                            , w = nonePar$svmParameters[2]
                            , alpha = nonePar$svmParameters[3]
                            , class = "Positive"
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:floor(nonePar$svmParameters[2])){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelNone = svm(formula(formula_svm), data = dataTrain_final
                  , cost = nonePar$svmParameters[4], epsilon = nonePar$svmParameters[5]
                  , gamma = nonePar$svmParameters[6], degree = nonePar$svmParameters[7]
                  , coef0 = nonePar$svmParameters[8]
                  , kernel = KernelFunctionsLabels[floor(nonePar$svmParameters[9])])
  
  print("modelPositive MKCD: OK")
  
  # negativeTrendTrain Negative
  #modelNegative= svm(formula(formula), data = negativeTrendTrain)
  modelPositive = getSVR_MKCD_par(dataTrain, dataValid, class = "Negative", nStepAhead) #nonePar$svmParameters
  finalData  = getFinalData(time_series_train = dataTrain
                            , time_series_test = dataValid
                            , phi = nonePar$svmParameters[1]
                            , w = nonePar$svmParameters[2]
                            , alpha = nonePar$svmParameters[3]
                            , class = "Negative"
                            , nStepAhead = nStepAhead)
  
  dataTrain_final = finalData$dataTrain
  dataValid_final = finalData$dataTest
  
  formula_svm = "nStepAhead ~ "
  for(i in 1:floor(nonePar$svmParameters[2])){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(dataTrain_final)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(dataTrain_final)[i], sep=""), sep="") 
    }
  }
  
  print(paste('formula: ', formula_svm, sep=""))
  
  modelNone = svm(formula(formula_svm), data = dataTrain_final
                  , cost = nonePar$svmParameters[4], epsilon = nonePar$svmParameters[5]
                  , gamma = nonePar$svmParameters[6], degree = nonePar$svmParameters[7]
                  , coef0 = nonePar$svmParameters[8]
                  , kernel = KernelFunctionsLabels[floor(nonePar$svmParameters[9])])
  
  print("modelNegative MKCD: OK")
  
  # Normal 
  normalPar = getSVMPar_GenSA(dataTrain, dataValid, nStepAhead) #View(normTrain)
  modelNormal = svm(formula(formula), data = normTrain
                    , cost = normalPar$svmParameters[1], epsilon = normalPar$svmParameters[2]
                    , gamma = normalPar$svmParameters[3], degree = normalPar$svmParameters[4]
                    , coef0 = normalPar$svmParameters[5]
                    , kernel = KernelFunctionsLabels[floor(normalPar$svmParameters[6])])
  #normalPar$modellingTime
  
  print("modelNormal: OK")
  
  predNormal_out = predict(modelNormal, normTest[,1:w])
  predNormal_outTrain = predict(modelNormal, normTrain[,1:w])
  # MKCD - TRAIN SET
  pred_out_Train = NULL
  for(i in 1:length(normTrain$nStepAhead)){#i=1
    if(normTrain$Class[i] == "None"){
      pred_out_Train[i] = modelNone %>% predict(as.matrix(normTrain[i,1:w]))
    }else{
      if(normTrain$Class[i] == "Positive"){
        pred_out_Train[i] = modelPositive %>% predict(as.matrix(normTrain[i,1:w]))
      }else{
        pred_out_Train[i] = modelNegative %>% predict(as.matrix(normTrain[i,1:w]))
      }
    }
  } #length(pred_out)
  
  # MKCD - TEST SET
  pred_out = NULL
  for(i in 1:length(normTest$nStepAhead)){#i=1
    if(normTest$Class[i] == "None"){
      pred_out[i] = modelNone %>% predict(as.matrix(normTest[i,1:w]))
    }else{
      if(normTest$Class[i] == "Positive"){
        pred_out[i] = modelPositive %>% predict(as.matrix(normTest[i,1:w]))
      }else{
        pred_out[i] = modelNegative %>% predict(as.matrix(normTest[i,1:w]))
      }
    }
  } #length(pred_out)
  
  png(paste("Results/Figures/", country, "_", nStepAhead ,"sta_"
            , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
  plot(normTest$nStepAhead, type="l", ylab = paste("Rolling ",phi,"-day average", sep=''),
       lwd = 3, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
                       1.1*max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
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
  
  return(list("MKCD_SVM_Test" = pred_out, "SVM_Test" = predNormal_out,
              "MKCD_SVM_Train" = pred_out_Train, "SVM_Train" = predNormal_outTrain))
}




#model = getSVMmodel_GenSA(noneTrendTrain)

