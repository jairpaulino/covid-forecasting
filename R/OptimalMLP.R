KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")
pGSA = list(); pGSA$max.call = 10000; pGSA$max.time = 20
pGSA$maxit = 1000; pGSA$nb.stop.improvement = 5; 
pGSA$temperature = 500

getSVMPar_GenSA = function(data){
  #data = negativeTrendTrain
  set.seed(123); #definir semente 
  #n = length(data) 
  
  fitnessSVM = function(parameters){
    #data = noneTrendTrain; parameters = c(10, 10, 10, 1, 0.5, 1)
    m = round(length(data[[1]])*0.8, 0)
    m_n = length(data[[1]]) 
    train_df = data[1:m,]; 
    valid_df = data[(m+1):m_n,]; 
    complete_ts = c(train_df, valid_df); 
    
    formula = "nStepAhead ~ "
    for(i in 1:w){
      if(i == 1){
        formula = paste(formula, paste(colnames(data)[i], sep=""), sep="") 
      }else{
        formula = paste(formula, paste(" + ", colnames(data)[i], sep=""), sep="") 
      }
    }
    
    svmParameters = list()
    svmParameters$cost = (parameters[1])#integer in [1, 15]
    svmParameters$epsilon = (parameters[2])#integer in [1, 15]
    svmParameters$gamma = (parameters[3])#integer in [1, 15]
    svmParameters$degree = (parameters[4])#integer in [1, 15]
    svmParameters$coef0 = (parameters[5])#integer in [1, 15]
    index = floor(parameters[6])#in [-.5+1e-10, 5.5-1e-10]
    svmParameters$KernelFunction = KernelFunctionsLabels[index]
    
    modelSVM = svm(formula(formula)
                   , data = train_df
                   , cost = svmParameters[1]
                   , epsilon = svmParameters[2]
                   , gamma = svmParameters[3]
                   , degree = svmParameters[4]
                   , coef0 = svmParameters[5]
                   , kernel = svmParameters$KernelFunction)
    
    predSVM = predict(modelSVM, valid_df)
    MSE = getMSE(target = valid_df$nStepAhead, forecast = predSVM)
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
                   , fn = fitnessSVM 
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
  optimal$forecastingTime = NA
  #optimal$model = optimalSVM$model
  optimal$GenSA_output = out
  optimal$svmParameters = out$par
  #optimal$svmParameters[["nSupportVectors"]] = optimal$model$tot.nSV
  return(optimal)
  
}

getModelSVM = function(dataTrain, dataTest){
  
  noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
  positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
  negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
  #View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)
  
  # get formula
  formula = "nStepAhead ~ "
  for(i in 1:w){
    if(i == 1){
      formula = paste(formula, paste(colnames(noneTrendTrain)[i], sep=""), sep="") 
    }else{
      formula = paste(formula, paste(" + ", colnames(noneTrendTrain)[i], sep=""), sep="") 
    }
  }
  
  # Modelo NONE
  nonePar = getSVMPar_GenSA(noneTrendTrain) #nonePar$svmParameters
  modelNone = svm(formula(formula), data = noneTrendTrain
                  , cost = nonePar$svmParameters[1], epsilon = nonePar$svmParameters[2]
                  , gamma = nonePar$svmParameters[3], degree = nonePar$svmParameters[4]
                  , coef0 = nonePar$svmParameters[5]
                  , kernel = KernelFunctionsLabels[floor(nonePar$svmParameters[6])])
  
  print("modelNone: OK")
  
  #nonePar$modellingTime
  # Modelo Positive
  #modelPositive = svm(formula(formula), data = positiveTrendTrain)
  positivePar = getSVMPar_GenSA(positiveTrendTrain) #nonePar$svmParameters
  modelPositive = svm(formula(formula), data = positiveTrendTrain
                      , cost = positivePar$svmParameters[1], epsilon = positivePar$svmParameters[2]
                      , gamma = positivePar$svmParameters[3], degree = positivePar$svmParameters[4]
                      , coef0 = positivePar$svmParameters[5]
                      , kernel = KernelFunctionsLabels[floor(positivePar$svmParameters[6])])
  
  print("modelPositive: OK")
  
  #positivePar$modellingTime
  # negativeTrendTrain Negative
  #modelNegative= svm(formula(formula), data = negativeTrendTrain)
  negativePar = getSVMPar_GenSA(negativeTrendTrain) #nonePar$svmParameters
  modelNegative = svm(formula(formula), data = negativeTrendTrain
                      , cost = negativePar$svmParameters[1], epsilon = negativePar$svmParameters[2]
                      , gamma = negativePar$svmParameters[3], degree = negativePar$svmParameters[4]
                      , coef0 = negativePar$svmParameters[5]
                      , kernel = KernelFunctionsLabels[floor(negativePar$svmParameters[6])])
  
  print("modelNegative: OK")
  
  #negativePar$modellingTime
  # Normal 
  #modelNormal = svm(formula(formula), data = normTrain)
  #predNormal_out = predict(modelNormal, normTest)
  normalPar = getSVMPar_GenSA(normTrain) #View(normTrain)
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

