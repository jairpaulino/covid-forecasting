noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
#View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)

noneTrendTest = normTest[which(normTest$Class == "None"),]
positiveTrendTest = normTest[which(normTest$Class == "Positive"),]
negativeTrendTest = normTest[which(normTest$Class == "Negative"),]
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
modelNone = svm(formula(formula), data = noneTrendTrain)

# Modelo Positive
modelPositive = svm(formula(formula), data = positiveTrendTrain)

# Modelo Negative
modelNegative= svm(formula(formula), data = negativeTrendTrain)

# Normal 
modelNormal = svm(formula(formula), data = normTrain)
predNormal_out = predict(modelNormal, normTest)

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
     lwd = 2, ylim=c(min(min(pred_out),min(normTest$nStepAhead)), 
                     max(max(pred_out),max(normTest$nStepAhead))))
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
print(metricsTable)
