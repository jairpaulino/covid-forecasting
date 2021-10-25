#Title: COVID-19 forecasting
#Author: Jair Paulino
#Date: --/2021

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Libraries
library(trend)
library(caTools)
library(caret)
library(tensorflow)
library(keras)
library(GenSA)
library(e1071)

# Importing functions
source("R/linearAnalisys.R")
source("R/Auxiliar.R")
source("R/performanceMetrics.R")
source("R/OptimalSVR.R")

# Parallel processing 
#library(doParallel)
#numCores <- detectCores()
#cl = makePSOCKcluster(numCores-1)
#registerDoParallel(cl)

# Importing data ----
data = read.csv(file = "Data/time_series_covid19_confirmed_global.csv")#View(data)
data = data.frame(t(data))
colnames(data) = paste(data[2,], data[1,], sep="_");# View(data)

# Creating time series
country = "US" #"Italy" #"Brazil" #"US"
countryAdj = paste(country, "_", sep="")
# from Feb 01 2020
countryTimeSeries = diff(as.numeric(data[15:length(data[[1]]),countryAdj]))
plot.ts(countryTimeSeries)

# Split data into train - test
m = round(length(countryTimeSeries)*0.8, 0)
m_n = length(countryTimeSeries)
train_valid_ts = countryTimeSeries[1:m]; plot.ts(train_valid_ts)
test_ts = countryTimeSeries[(m+1):m_n]; plot.ts(test_ts)
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

complete_ts = c(train_ts, valid_ts, test_ts); plot.ts(complete_ts)

parameters = getModelSVR_MKCD(normTrain, normValid, nStepAhead = 7)


## End(Not run)

# To create a new ts based on the original and phi parameter
phi = 14
runningMeanincDia = getRunningMean(countryTimeSeries, phi)
plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))

w = 21
alpha = 0.05
nStepAhead = 7

timeSeriesnName = "runningMeanincDia"
timeSeries = runningMeanincDia
title = paste("Rolling ",phi,"-day average", sep="")
trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
#View(trendAnalysis_df)
#write.csv(trendAnalysis_df, paste("Results/", country, "_", timeSeriesnName, "_", w, "_", alpha,"_trendAnalysis_df.csv", sep=""))

#generateGraph(runningMeanincDia, timeSeries = timeSeries, w = w, title = title)


# # Create Sliding window matrix
# 
# trendAnalysis_df = getTrendAnalysis(timeSeries_df = train_ts, 
#                                     w = w, 
#                                     alpha = alpha,
#                                     nStepAhead = nStepAhead) 
# #View(trendAnalysis_df)
# trendAnalysisTest_df = getTrendAnalysis(timeSeries_df = test_ts, 
#                                         w = w, 
#                                         alpha = alpha,
#                                         nStepAhead = nStepAhead)
# 
# # MinMax Scaling
# preProc = preProcess(trendAnalysis_df[,1:w], method=c("range"))
# normTrain = cbind(predict(preProc, trendAnalysis_df[,1:w]), trendAnalysis_df[,(w+1):length(trendAnalysis_df)])
# normTest = cbind(predict(preProc, trendAnalysisTest_df[,1:w]), trendAnalysisTest_df[,(w+1):length(trendAnalysisTest_df)])
# 
# resultSVM = getModelSVM(normTrain, normTest)
# 
# resultSVM_test = data.frame(MKCD_SVM = resultSVM$MKCD_SVM_Test,
#                             SVM = resultSVM$SVM_Test)
# 
# resultSVM_train = data.frame(MKCD_SVM = resultSVM$MKCD_SVM_Train,
#                             SVM = resultSVM$SVM_Train)
# 
# write.csv(resultSVM_test, file = paste("Results/resultsTest_", country, "_", nStepAhead ,"sta_"
#                                      , "w-", w, "_phi-", phi, "_alpha-", alpha,".csv", sep=""), row.names = F)
# 
# write.csv(resultSVM_train, file = paste("Results/resultsTrain_", country, "_", nStepAhead ,"sta_"
#                                        , "w-", w, "_phi-", phi, "_alpha-", alpha,".csv", sep=""), row.names = F)
# 
# #View(resultSVM_test)
# #View(normTrain); View(normTest)
# #plot.ts(normTrain$nStepAhead); plot.ts(normTest$nStepAhead)

#X_train = normTrain
#y_train = trendAnalysis_df$nStepAhead
#X_test = normTest
#y_test = trendAnalysisTest_df$nStepAhead

# model = keras_model_sequential()
# model %>%
#   layer_dense(units=512, activation = 'relu', 
#               input_shape = c(w)) %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=1)
# 
# early_stopping <- callback_early_stopping(monitor = 'val_loss', 
#                                           patience = 5);
# 
# #model
# model %>%
#   compile(loss = 'mse', 
#           metrics ="mae", 
#           optimizer = 'rmsprop')
# 
# history = model %>% fit(as.matrix(X_train), 
#                         y_train,
#                         epochs = 50,
#                         batch_size = 1,
#                         validation_split = 0.2,
#                         callbacks = c(early_stopping))
# 
# 
# pred_out = model %>% predict(as.matrix(X_test))
# length(y_test); length(pred_out)
# #png("Results/Figures/21sta_nL5_forecast_r14a.png", res = 100)
# plot(y_test, type="l", ylab = paste("Rolling ", w, "-day average", sep=""),
#      lwd = 2, ylim = c(min(min(pred_out), min(y_test)), max(max(pred_out), max(y_test))))
# lines(pred_out, col=2, lwd=2)
# points(pred_out, col=2, pch=12)
# legend("topleft", c("Proposed approach"), 
#        col="red", lty=1, lwd=2, cex = 0.9,
#        box.col = "white", inset = 0.01)
# #dev.off()
# 
# getMSE(y_test, pred_out)
# getMAE(y_test, pred_out)
# getMAPE(y_test, pred_out)
# getARV(y_test, pred_out)
#getTheil(y_test, pred_out)

