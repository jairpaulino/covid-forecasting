tryCatch(library("GenSA"), error = function(e){install.packages("GenSA"); library("GenSA")}, finally=paste(":( :( :( :( Error when installing", "GenSA"))
tryCatch(library("e1071"), error = function(e){install.packages("e1071"); library("neuralnet")}, finally=paste(":( :( :( :( Error when installing", "e1071"))

KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")
#TypeLabels = c("eps-regression", "C-classification", "nu-classification", "nu-regression")
#criar matriz para alimentar o algoritmo svm
getSvmMatrix = function(nnParameters, series){
  sazonalityRange = nnParameters$sazonality*nnParameters$pARS
  initialIndex = sazonalityRange+1
  loss = sazonalityRange
  if(loss==0) {
    loss = nnParameters$pAR
    initialIndex = nnParameters$pAR+1
  }
  ncols = (1+nnParameters$pAR+nnParameters$pARS)
  seriesSize = length(series)
  nrows = (seriesSize-loss)
  if(nrows <=0){
    return (NULL)
  }
  else{
    ann.matrix = as.data.frame(matrix(nrow= nrows, ncol= 0))
    for(i in 0:nnParameters$pAR){
      colName_i = paste("ut_", i, sep="")
      ann.matrix[[colName_i]] = series[(initialIndex-i):(seriesSize-i)]
    }
    #View(ann.matrix)
    if(nnParameters$pARS>0){
      for(i in 1:nnParameters$pARS){
        colName_i = paste("ut_", (i*nnParameters$sazonality), sep="")
        ann.matrix[[colName_i]] = series[(initialIndex-i*nnParameters$sazonality):(seriesSize-i*nnParameters$sazonality)]
      }
    }
    #View(ann.matrix)
    return(ann.matrix)
  }
}
#svm.matrix#criar f?rmula para usar em svm()
getSvmFormula = function(varNames){
  formula = paste(varNames[1], "~")
  varNames = varNames[-1]
  aux = paste(varNames, collapse = "+")
  formula = as.formula(paste(formula, aux, sep=""))
  return (formula)  
}
#criar modelo gen?rico svm que receber? os par?metros otimizados da fun??o GenSA
getSvmModel = function(svmParameters, svm.matrix){
  if(is.null(svm.matrix)){
    return (NULL)
  } 
  else if(nrow(svm.matrix)<10){
    return (NULL)
  } 
  else{
    varNames = colnames(svm.matrix)
    formula = getSvmFormula(varNames)
    set.seed(1)
    tryCatch({
      SvmModel = svm(formula, data=svm.matrix,
                     type = "eps-regression",
                     kernel = svmParameters$KernelFunction,
                     degree = svmParameters$degree,
                     gamma = svmParameters$gamma,
                     cost = svmParameters$cost,
                     epsilon = svmParameters$epsilon,
                     coef0 = svmParameters$coef0,
                     probability= TRUE,
                     #fitted = TRUE
      )
    }, 
    error = function(e){
      message(paste("Error in svm.", e)); #duvida fun??o ou pcte?? acho q ? fun??o
      return(return(NA))
    } 
    #, finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
    )
    return(SvmModel)
  }
}
getSvmBIC <- function(svm, svmParameters, validationSet){
  if(is.null(svm)){
    return (Inf)
  } else {
    #forecasts = as.numeric(
    #predict(svm, as.data.frame(validationSet[c(-1)])))
    #mean_validationset <- mean(validationSet, na.rm = TRUE)
    #is.na_replace_mean[is.na(is.na_replace_mean)] <- x_num_mean
    #validationSet<-is.na_replace_mean
    forecasts = as.numeric(predict(svm, as.data.frame(validationSet)[-c(1)]))
    #validationSet <- na.omit(validationSet) #verificar junto ao prof. Paulo
    #forecasts = as.numeric(predict(svm, validationSet))#[c(-1)])
    residuos =  validationSet[,1] - forecasts #hist(residuos)
    residuos = residuos[is.na(residuos)==F]
    #residuos =  svm$residuals
    #residuos = residuos[is.na(residuos)==F]
    # n = length(residuos)
    # LL <- function(residuos,m1,s){
    #   sum(stats::dnorm(residuos,m1,s,log=TRUE))
    # }
    # loglike = LL(residuos,mean(residuos),sd(residuos))
    # if(is.na(loglike)){
    #   return (Inf)
    # }
    # nLag = (svmParameters$pAR+svmParameters$pARS)
    # nSVs = svm$tot.nSV#length(svm$index)
    # nPar = nLag*nSVs + 1
    # if(svmParameters$KernelFunction=="linear"){
    #   nPar = nPar+2
    # } else  if(svmParameters$KernelFunction=="radial"){
    #   nPar = nPar + 3
    # } else  if(svmParameters$KernelFunction=="polynomial"){
    #   nPar = nPar + 5
    # } else if(svmParameters$KernelFunction=="sigmoid") {
    #   nPar = nPar + 4
    # }
    #nPar = ((nnParameters$pAR+nnParameters$pARS) + 1)*(nnParameters$nNodes+1)
    # AIC = -2*(loglike) + 2*3
    # AICc = -2*(loglike) + (2*(3+1)*(3+2)/(n-3-2))
    #BIC = -2*(loglike) + nPar*log(n)
    #if(!is.finite(BIC)){
      #BIC=-BIC
    MSE = mean(residuos^2)
    if(!is.finite(MSE)){
      return (Inf)
    }
    #return(BIC)
    return(MSE)
  }
}
getOptimalSVM = function(series, seriesName = seriesNm_i){
  print(paste("***", seriesName, ": SVM Single Model ***"))
  pGSA <<- tuningParameters[[seriesName]]$GSA
  propAR_ARS <<- tuningParameters[[seriesName]]$propAR_ARS
  modellingTime = proc.time()[[3]]
  calls <<- 0; optimalSVM <<- list()
  optimalSVM$BIC = Inf
  optimalSVM$model = NULL
  optimalSVM$parameters = NULL
  fitness=function(parameters){# = c(2,2,12,3,.5,2)){
    calls <<- calls+1;
    # print(paste("#calls =", calls))
    svmParameters  = list()
    svmParameters$pAR =   floor(parameters[1])#C
    svmParameters$pARS =  floor(parameters[2])#e
    svmParameters$sazonality = svmParameters$pAR + floor(parameters[3])#integer in [0, 13]
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
  set.seed(0); #definir semente 
  n = length(series) #n ? o tamanho da s?rie
  max_pAR = max(1, round(propAR_ARS*n))+1-1e-8; 
  max_pARS = max(0, round(0.5*propAR_ARS*n))+1-1e-8;
  min_sazonality = 2
  max_sazonality = max((min_sazonality+1),(round(2*propAR_ARS*n))) +1-1e-8
  min_cost = 1e-5; max_cost = 1e+2
  min_epsilon = 1e-5; max_epsilon = 1
  min_gamma = 1e-5; max_gamma = 1e+4
  min_degree = 1; max_degree = 5
  min_coef0 = 0; max_coef0 = 1e+1
  max_KernelFunctions = (length(KernelFunctionsLabels)+1-1e-5)
  lowers <- c(1      , 0       , min_sazonality)#vetor comprimento dos par?metro. Lim inf,                                             1)
  uppers <- c(max_pAR, max_pARS, max_sazonality)#vetor comprimento dos par?metro. Lim sup
  lowers = c(lowers, min_cost, min_epsilon, min_gamma, min_degree, min_coef0)
  uppers = c(uppers, max_cost, max_epsilon, max_gamma, max_degree, max_coef0)
  lowers = c(lowers, 1)
  uppers = c(uppers, max_KernelFunctions)
  tol <- 1e-3
  out = 
    tryCatch({
      expr = GenSA(lower = lowers, upper = uppers, fn = fitness 
                   , par = (lowers+uppers)/2
                   , control=list(max.call = pGSA$max.call, max.time=pGSA$max.time
                                  , maxit = pGSA$maxit, verbose = TRUE, smooth = FALSE
                                  , seed=-1, nb.stop.improvement = pGSA$nb.stop.improvement
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
  optimal$model = optimalSVM$model
  optimal$GenSA_output = out
  optimal$svmParameters = optimalSVM$svmParameters
  optimal$svmParameters[["nSupportVectors"]] = optimal$model$tot.nSV
  return(optimal)
} 
getSvmForecasts = function(svm=optSvm_i, series=data.all.norm){
  # SVM one-step forecast  (training and test set)
  #plot(series)
  forecastingTime = proc.time()[[3]]
  svmMatrix = series # in the case of wanting the ANN combined model
  if(!is.matrix(series)){#In the case of wanting the ANN single model
    svmMatrix = getSvmMatrix(svm$svmParameters, series)#;View(svmMatrix)
    svmMatrix = svmMatrix[-c(1)]
  } 
  else{
    svmMatrix = as.data.frame(svmMatrix)
  }
  nCases = nrow(svmMatrix)
  forecasts = numeric(nCases)
  for(t in 1:nCases){
    case_t = as.data.frame(svmMatrix[t,]); 
    colnames(case_t) = colnames(svmMatrix)
    nNAs = length(which(is.na(case_t)))
    if(nNAs > 0){
      forecasts[t] = NA
    }
    else{
      forecasts[t] = predict(object = svm$model, newdata = case_t)
    }
  }
  # forecasts = predict(object=svm$model, newdata=svmMatrix)
  forecastingTime = proc.time()[[3]] - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = as.numeric(forecasts)
  return (ret)
}
getSvmNewForecasts = function(ModelsObjs = Optimum[["CO2"]]
                              , series = ModelsObjs$all.series, start=nrow(series)){
  # SVM one-step forecast  (fixed model and updating time series)
  target = series$target
  #plot(target)
  svm = ModelsObjs$optSvmObj
  forecastingTime = proc.time()[[3]]
  pAR = svm$svmParameters$pAR
  pARS = svm$svmParameters$pARS
  sazonality = svm$svmParameters$sazonality
  colNames = names(svm$model$x.scale$`scaled:center`)
  if(is.null(colNames)){
    colNames = as.character(svm$model$terms)[3]
  }
  nCols = length(colNames)
  newDataMatrix = NULL
  lagIndexes = NULL
  fullRelevantIndexes = NULL
  end = length(target)+1
  for(t in (start+1):end){
    AR_indexes = t-(1:pAR)
    SAR_indexes = NULL
    if(pARS > 0 & sazonality > 0){
      SAR_indexes = t-seq(sazonality, pARS*sazonality, by=sazonality)
    }
    lagIndexes_i = c(AR_indexes, SAR_indexes)
    lagIndexes = union(lagIndexes, lagIndexes_i)
    x = target[lagIndexes_i]
    x = getNormalizedSeries(series = x
                            , min = ModelsObjs$superMinimum
                            , max = ModelsObjs$superMaximum)
    newDataMatrix = rbind(newDataMatrix, x)
    fullRelevantIndexes_i = (min(lagIndexes_i)):t
    fullRelevantIndexes = union(fullRelevantIndexes, fullRelevantIndexes_i)
  }
  newDataMatrix = as.matrix(newDataMatrix)
  dimnames(newDataMatrix)[[2]]= colNames#;View(newDataMatrix)
  forecasts.norm = getSvmForecasts(svm = svm, series = newDataMatrix)$forecasts
  forecasts = getDenormalizedSeries(dataset.norm = forecasts.norm
                                        , min = ModelsObjs$superMinimum
                                        , max = ModelsObjs$superMaximum)
  forecastingTime = proc.time()[[3]] - forecastingTime
  forecastsData = cbind(new_u = forecasts, newDataMatrix)
  forecastsData = as.data.frame(t(forecastsData))#;View(forecastsData)
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = as.numeric(forecasts)
  ret$forecastsData = forecastsData
  ret$fullRelevantIndexes = fullRelevantIndexes
  ret$lagIndexes = lagIndexes
  return (ret)
}