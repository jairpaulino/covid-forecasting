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

# Importing data ----
data = read.csv(file = "Data/time_series_covid19_confirmed_global.csv")#View(data)
data = data.frame(t(data))
colnames(data) = paste(data[2,], data[1,], sep="_");# View(data)

# Creating time series
country = "Brazil" #"Brazil" #"US"
countryAdj = paste(country, "_", sep="")
# from Feb 01 2020
countryTimeSeries = diff(as.numeric(data[15:length(data[[1]]),countryAdj]))
plot.ts(countryTimeSeries)
#countryTimeSeries=ts(countryTimeSeries, frequency = 365.25, start=c(2020, 02))

# To create a new ts based on the original and phi parameter
phi = 14
runningMeanincDia = getRunningMean(countryTimeSeries, phi)
plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))

w = 21
alpha = 0.1
nStepAhead = 7

timeSeriesnName = "runningMeanincDia"
timeSeries = runningMeanincDia
title = paste("Rolling ",w,"-day average", sep="")
trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
#View(trendAnalysis_df)
#write.csv(trendAnalysis_df, paste("Results/", country, "_", timeSeriesnName, "_", w, "_", alpha,"_trendAnalysis_df.csv", sep=""))

#generateGraph(runningMeanincDia, timeSeries = timeSeries, w = w, title = title)

# Split data
#set.seed(2311) 
#sample = sample.split(runningMeanincDia, SplitRatio = .70)
#train_ts = subset(countryTimeSeries, sample == TRUE); plot.ts(train_ts)
#test_ts  = subset(countryTimeSeries, sample == FALSE); plot.ts(test_ts)

m = round(length(runningMeanincDia)*0.8, 0)
m_n = length(runningMeanincDia)
train_ts = runningMeanincDia[1:m]; plot.ts(train_ts)
test_ts = runningMeanincDia[(m+1):m_n]; plot.ts(test_ts)
complete_ts = c(train_ts, test_ts); plot.ts(complete_ts)

# Create Sliding window matrix

trendAnalysis_df = getTrendAnalysis(timeSeries_df = train_ts, 
                                    w = w, 
                                    alpha = alpha,
                                    nStepAhead = nStepAhead) 
#View(trendAnalysis_df)
trendAnalysisTest_df = getTrendAnalysis(timeSeries_df = test_ts, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = nStepAhead)

# MinMax Scaling
preProc = preProcess(trendAnalysis_df[,1:w], method=c("range"))
normTrain = cbind(predict(preProc, trendAnalysis_df[,1:w]), trendAnalysis_df[,(w+1):length(trendAnalysis_df)])
normTest = cbind(predict(preProc, trendAnalysisTest_df[,1:w]), trendAnalysisTest_df[,(w+1):length(trendAnalysisTest_df)])
#View(normTrain); View(normTest)

X_train = normTrain
y_train = trendAnalysis_df$nStepAhead
X_test = normTest
y_test = trendAnalysisTest_df$nStepAhead

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

