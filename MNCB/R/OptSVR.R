tryCatch(library("GenSA"), error = function(e){install.packages("GenSA"); library("GenSA")}, finally=paste("Error when installing", "GenSA"))
tryCatch(library("e1071"), error = function(e){install.packages("e1071"); library("neuralnet")}, finally=paste("Error when installing", "e1071"))

KernelFunctionsLabels = c("linear", "polynomial", "radial", "sigmoid")
#criar matriz para alimentar o algoritmo svm
getSvmMatrix = function(nnParameters, series){
  #series = incDia$target; svmParameters  = list(); svmParameters$pAR =   floor(5)#C
  #svmParameters$pARS =  floor(2);  svmParameters$sazonality = svmParameters$pAR + floor(12)#integer in [0, 13]
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
  if(nrows <= 0){
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
#svm.matrix_test = getSvmMatrix(svmParameters, series); View(svm.matrix_test)
#criar formula para usar em svm()
getSvmFormula = function(varNames){
  formula = paste(varNames[1], "~")
  varNames = varNames[-1]
  aux = paste(varNames, collapse = "+")
  formula = as.formula(paste(formula, aux, sep=""))
  return (formula)  
}
#criar modelo generico svm que recebera os parametros otimizados da funcao GenSA
getSvmModel = function(svmParameters, svm.matrix){
  #svm.matrix = svm.matrix_test; svmParameters$degree = 4
  #svmParameters$gamma =  3; svmParameters$cost =  5
  #svmParameters$epsilon =  7; svmParameters$coef0 =  0.5; svmParameters
  #index = floor(4)#in [-.5+1e-10, 5.5-1e-10]
  #svmParameters$KernelFunction = KernelFunctionsLabels[index]
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


