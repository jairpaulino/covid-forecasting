KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")

testesmv = AirPassengers

fitnessSVM = function(parameters, data){
  
  #data = noneTrendTrain
  
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
  
  svmParameters  = list()
  svmParameters$cost = (parameters[4])#integer in [1, 15]
  svmParameters$epsilon = (parameters[5])#integer in [1, 15]
  svmParameters$gamma = (parameters[6])#integer in [1, 15]
  svmParameters$degree = (parameters[7])#integer in [1, 15]
  svmParameters$coef0 = (parameters[8])#integer in [1, 15]
  index = floor(parameters[9])#in [-.5+1e-10, 5.5-1e-10]
  svmParameters$KernelFunction = KernelFunctionsLabels[index]
  
  modelSVM = svm(formula(formula), data = train_df)
  predSVM = predict(modelSVM, valid_df)
  MSE = getMSE(target = valid_df$nStepAhead, forecast = predSVM)
  
  return(MSE)
}


getSVMmodel_GenSA = function(){
  
}


fitness=function(parameters){
  
  svmParameters  = list()
  svmParameters$cost = (parameters[4])#integer in [1, 15]
  svmParameters$epsilon = (parameters[5])#integer in [1, 15]
  svmParameters$gamma = (parameters[6])#integer in [1, 15]
  svmParameters$degree = (parameters[7])#integer in [1, 15]
  svmParameters$coef0 = (parameters[8])#integer in [1, 15]
  index = floor(parameters[9])#in [-.5+1e-10, 5.5-1e-10]
  svmParameters$KernelFunction = KernelFunctionsLabels[index]
  # if(calls==1830){
  #   g=3
  # }
  svm.matrix = getSvmMatrix(svmParameters, series)#View(svm.matrix)
  #SvmModel = getSvmModel(svmParameters, svm.matrix)
  n = nrow(svm.matrix) #inicio da implementa??o dataset (training, validation and test)
  v = round(.25*n)
  #set.seed(2)
  validationSetIndexes = sample(x = 1:n, size = v, replace = FALSE)#hist(validationSetIndexes)
  validationSet = svm.matrix[validationSetIndexes, ]#View(validationSet)
  trainingSet = svm.matrix[-validationSetIndexes, ]#View(trainingSet)
  SvmModel = getSvmModel(svmParameters, trainingSet)
  BIC = getSvmBIC(svm =  SvmModel, 
                  svmParameters = svmParameters, 
                  validationSet = validationSet)#fim da implementa??o dataset (training, validation and test)
  #BIC = getSvmBIC (SvmModel, svmParameters)
  if(BIC < optimalSVM$BIC){
    optimalSVM$BIC <<- BIC
    optimalSVM$model <<- SvmModel
    optimalSVM$svmParameters <<- svmParameters
    print(paste(seriesName, "SVM, #calls =", calls, "BIC=", BIC));
    print(svmParameters);
  }
  return(BIC)
}