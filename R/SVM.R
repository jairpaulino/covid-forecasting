noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
#View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)

#noneTrendTest = normTest[which(normTest$Class == "None"),]
#positiveTrendTest = normTest[which(normTest$Class == "Positive"),]
#negativeTrendTest = normTest[which(normTest$Class == "Negative"),]
#View(noneTrendTest); View(positiveTrendTest); View(negativeTrendTest)

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
#nonePar$modellingTime
# Modelo Positive
#modelPositive = svm(formula(formula), data = positiveTrendTrain)
positivePar = getSVMPar_GenSA(positiveTrendTrain) #nonePar$svmParameters
modelPositive = svm(formula(formula), data = positiveTrendTrain
                , cost = positivePar$svmParameters[1], epsilon = positivePar$svmParameters[2]
                , gamma = positivePar$svmParameters[3], degree = positivePar$svmParameters[4]
                , coef0 = positivePar$svmParameters[5]
                , kernel = KernelFunctionsLabels[floor(positivePar$svmParameters[6])])
#positivePar$modellingTime
# negativeTrendTrain Negative
#modelNegative= svm(formula(formula), data = negativeTrendTrain)
negativePar = getSVMPar_GenSA(negativeTrendTrain) #nonePar$svmParameters
modelNegative = svm(formula(formula), data = negativeTrendTrain
                    , cost = negativePar$svmParameters[1], epsilon = negativePar$svmParameters[2]
                    , gamma = negativePar$svmParameters[3], degree = negativePar$svmParameters[4]
                    , coef0 = negativePar$svmParameters[5]
                    , kernel = KernelFunctionsLabels[floor(negativePar$svmParameters[6])])
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
predNormal_out = predict(modelNormal, normTest[,1:w])

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
}
length(pred_out)
#png("Results/Figures/21sta_nL5_forecast_r14a.png", res = 100)
plot(normTest$nStepAhead, type="l", ylab = paste("Rolling ",w,"-day average", sep=''),
     lwd = 2, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
                     max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
lines(pred_out, col=2, lwd=2)
points(pred_out, col=2, pch=12)
lines(predNormal_out, col=3, lwd=2)
points(predNormal_out, col=3, pch=12)
legend("topleft", c("MKCD - SVM", "SVM"),
       col=c(2,3), lty=1, lwd=3, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()
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
