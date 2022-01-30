#Title: MLP, SVR, and LSTM COVID-19 forecasting
#Date: Nov/2021

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 
#sourceMode <<- getSourceMode(isToDebug)

# Libraries
library(tensorflow)
library(keras)
library(e1071)
library(trend)      # Mann-Kendall trend test
library(GenSA)      # Generalized simulated annealing
 
# Importing functions
source("R/linearAnalisys.R")
source("R/Auxiliar.R")
source("R/performanceMetrics.R")
source("R/OptimalSVR.R")
source('R/MkParameters.R')

# Importing data ----
# Argentina, Brazil, China, France, Germany
country = "Argentina" 
data = read.csv(file = paste("../Data/2021-10-26/", country, "_timeSeries.csv", sep=""), sep = "\t")#View(data)
incDia = data.frame(target = na.omit(data$target)); head(incDia, 2)
plot.ts(incDia$target, ylab="Daily incidence")

# Preprocessing ----
# Split data into train - test
m = round(length(incDia$target)*0.8, 0)
m_n = length(incDia$target)
train_valid_ts = incDia$target[1:m]; plot.ts(train_valid_ts)
test_ts = incDia$target[(m+1):m_n]; plot.ts(test_ts)
# Split data into train - valid
z = round(length(train_valid_ts)*0.8, 0)
z_p = round(length(train_valid_ts), 0)
train_ts = train_valid_ts[1:z]; plot.ts(train_ts)
valid_ts = train_valid_ts[(z+1):z_p]; plot.ts(valid_ts)
# MinMax Scaling
normTrain = getNormalizeTS(train_ts, min=min(train_ts), max=max(train_ts))
normValid= getNormalizeTS(valid_ts, min=min(train_ts), max=max(train_ts))
normTest = getNormalizeTS(test_ts, min=min(train_ts), max=max(train_ts))
#plot.ts(normTrain); plot.ts(normValid); plot.ts(normTest)

#mkParameters = get_mk_parameters(normTrain, w_max = 10, phi_max = 10)
mkParameters = list(7, 14)
results = getModelSVR_MKCD(dataTrain = normTrain
                           , dataValid = normValid
                           , dataTest = normTest
                           , w = as.numeric(mkParameters[1])
                           , phi = as.numeric(mkParameters[2])
                           , nStepAhead = 1, alpha = 0.05)
# SVR MODEL--
normalPar = getSVMPar_GenSA(dataTrain, dataValid, nStepAhead) #View(normTrain)
finalData  = getFinalDataAll(time_series_train = dataTrain
                             , time_series_test = dataValid
                             , phi = phi
                             , w = w
                             , alpha = alpha
                             , nStepAhead = 1)

dataTrain_final = finalData$dataTrain
dataValid_final = finalData$dataTest
formula_svm = get_formula(dataTrain_final)
modelNormal = svm(formula(formula_svm), data = dataTrain_final
                  , cost = as.numeric(normalPar$svmParameters[1])
                  , epsilon = as.numeric(normalPar$svmParameters[2])
                  , gamma = as.numeric(normalPar$svmParameters[3])
                  , degree = as.numeric(normalPar$svmParameters[4])
                  , coef0 = as.numeric(normalPar$svmParameters[5])
                  , kernel = KernelFunctionsLabels[floor(normalPar$svmParameters[6])])
#plot.ts(dataTrain_final$nStepAhead); lines(modelNormal$fitted, col=2)
predNormal_out = predict(modelNormal, dataValid_final)
predNormal_outTrain = predict(modelNormal, dataTrain_final)


# Calculate the
metricsTable = as.data.frame(matrix(ncol=2, nrow=4))
colnames(metricsTable) = c("MKCD_SVM", "SVM")
rownames(metricsTable) = c("RMSE", "MAE", "MAPE", "ARV")
metricsTable$SVM[1] = getRMSE(finalData$dataTest$nStepAhead, predNormal_out) #Normal
metricsTable$MKCD_SVM[1] = getRMSE(finalData$dataTest$nStepAhead, pred_out_Test)
metricsTable$SVM[2] = getMAE(finalData$dataTest$nStepAhead, predNormal_out) #Normal
metricsTable$MKCD_SVM[2] = getMAE(finalData$dataTest$nStepAhead, pred_out_Test)
metricsTable$SVM[3] = getMAPE(finalData$dataTest$nStepAhead, predNormal_out) #Normal
metricsTable$MKCD_SVM[3] = getMAPE(finalData$dataTest$nStepAhead, pred_out_Test)
metricsTable$SVM[4] = getARV(finalData$dataTest$nStepAhead, predNormal_out) #Normal
metricsTable$MKCD_SVM[4] = getARV(finalData$dataTest$nStepAhead, pred_out_Test)
#options(scipen=999)
print(round(metricsTable,3))

write.csv(metricsTable, file = paste("Results/", country, "_", nStepAhead ,"sta_"
                                     , "w-", w, "_phi-", phi, "_alpha-", alpha,".csv", sep=""))


#png(paste("Results/Figures/train_", country, "_", nStepAhead ,"sta_"
#          , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
plot(results$Target_Test, type="l")#, ylab = paste("Rolling ",phi,"-day average", sep=''),
#     lwd = 3)#, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
#       1.1*max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
lines(results$MKCD_SVM_Test, col=2, lwd=2)
points(results$MKCD_SVM_Test, col=2, pch=15)
#lines(predNormal_outTrain, col=3, lwd=2)
#points(predNormal_outTrain, col=3, pch=16)
legend("topleft", c("MKCD - SVM", "SVM"),
       col=c(2,3),  pch=c(15,16), lty=1, lwd=3, cex = 0.9,
       box.col = "white", inset = 0.01, horiz = T)
#dev.off()

#png(paste("Results/Figures/test_", country, "_", nStepAhead ,"sta_"
#          , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
plot(results$Target_Train, type="l")#, ylab = paste("Rolling ",phi,"-day average", sep=''),
#     lwd = 3)#, ylim=c(min(min(predNormal_out),min(normTest$nStepAhead),min(pred_out)), 
#       1.1*max(max(predNormal_out),max(normTest$nStepAhead),max(pred_out))))
lines(results$MKCD_SVM_Train, col=2, lwd=2)
points(results$MKCD_SVM_Train, col=2, pch=15)
#lines(predNormal_out, col=3, lwd=2)
#points(predNormal_out, col=3, pch=16)
legend("topleft", c("MKCD - SVM", "SVM"),
       col=c(2,3),  pch=c(15,16), lty=1, lwd=3, cex = 0.9,
       box.col = "white", inset = 0.01, horiz = T)
#dev.off()



#write.csv2(incDia, paste("Data/Country/", country, ".csv", sep="")
#           , row.names = FALSE)

