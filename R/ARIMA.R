library(forecast)

noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
#View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)

noneTrendTest = normTest[which(normTest$Class == "None"),]
positiveTrendTest = normTest[which(normTest$Class == "Positive"),]
negativeTrendTest = normTest[which(normTest$Class == "Negative"),]
#View(noneTrendTest); View(positiveTrendTest); View(negativeTrendTest)

# Modelo NONE
modelNone = auto.arima(noneTrendTrain$nStepAhead)
#forecastModeNone = forecast(modelNone, h=21)
onestep_arima = fitted(Arima(noneTrendTest$nStepAhead))

length(noneTrendTrain$nStepAhead); length(forecastModeNone$fitted)
plot(forecastModeNone$fitted)
plot(forecastModeNone)

# Modelo Positive
modelPositive = keras_model_sequential()
modelPositive %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = c(w)) %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

#model
modelPositive %>%
  compile(loss = 'mse', 
          metrics ="mae", 
          optimizer = 'rmsprop')

history = modelPositive %>% 
  fit(as.matrix(positiveTrendTrain[,1:12]),
      positiveTrendTrain$nStepAhead,
      epochs = 50,
      batch_size = 5,
      validation_split = 0.2,
      callbacks = c(early_stopping))
#plot(history)

# Modelo Negative
modelNegative = keras_model_sequential()
modelNegative %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = c(w)) %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

#model
modelNegative %>%
  compile(loss = 'mse', 
          metrics ="mae", 
          optimizer = 'rmsprop')

history = modelNegative %>% fit(as.matrix(negativeTrendTest[,1:12]), 
                                negativeTrendTest$nStepAhead,
                                epochs = 50,
                                batch_size = 5,
                                validation_split = 0.2,
                                callbacks = c(early_stopping))
#plot(history)

# Normal Negative
normalModel = keras_model_sequential()
normalModel %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = c(w)) %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

#model
normalModel %>%
  compile(loss = 'mse', 
          metrics ="mae", 
          optimizer = 'rmsprop')

history = normalModel %>% fit(as.matrix(normTrain[,1:w]), 
                              normTrain$nStepAhead,
                              epochs = 50,
                              batch_size = 5,
                              validation_split = 0.2,
                              callbacks = c(early_stopping))
#plot(history)
predNormal_out = normalModel %>% predict(as.matrix(normTest[,1:w]))


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
plot(normTest$nStepAhead, type="l", ylab = "Rolling 12-day average",
     lwd = 2, ylim=c(min(pred_out), max(pred_out)))
lines(pred_out, col=2, lwd=2)
points(pred_out, col=2, pch=12)
lines(predNormal_out, col=3, lwd=2)
points(predNormal_out, col=3, pch=12)
legend("topleft", c("MKCD - MLP", "MLP"),
       col=c(2,3), lty=1, lwd=3, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()

getRMSE(normTest$nStepAhead, predNormal_out) #Normal
getRMSE(normTest$nStepAhead, pred_out) #MKCD

getMAE(normTest$nStepAhead, predNormal_out)
getMAE(normTest$nStepAhead, pred_out)

getMAPE(normTest$nStepAhead, predNormal_out)
getMAPE(normTest$nStepAhead, pred_out)

getARV(normTest$nStepAhead, predNormal_out)
getARV(normTest$nStepAhead, pred_out)

# library(caTools)
# library(caret)
# 
# # Split data
# set.seed(2311) 
# sample = sample.split(mm14incDia, SplitRatio = .70)
# train_ts = subset(mm14incDia, sample == TRUE); plot.ts(train_ts)
# test_ts = subset(mm14incDia, sample == FALSE); plot.ts(test_ts)
# trainTest_ts = c(train_ts)
# # Create Sliding window matrix
# trendAnalysis_df = getTrendAnalysis(timeSeries_df = train_ts, 
#                                     w = w, 
#                                     alpha = alpha,
#                                     nStepAhead = 21) 
# trendAnalysisTest_df = getTrendAnalysis(timeSeries_df = test_ts, 
#                                         w = w, 
#                                         alpha = alpha,
#                                         nStepAhead = 21) 
# # #View(trendAnalysis_df)
# 
# # MinMax Scaling
# preProc = preProcess(trendAnalysis_df[,1:18], method=c("range"))
# # normTrain = predict(preProc, trendAnalysis_df[,1:18])
# # normTest = predict(preProc, trendAnalysisTest_df[,1:18])
# # #View(norm); View(normTest)
# 
# X_train = normTrain
# y_train = trendAnalysis_df$nStepAhead
# X_test = normTest
# y_test = trendAnalysisTest_df$nStepAhead
# 
# # LSTM model
# model = keras_model_sequential()
# model %>%
#   layer_lstm(units            = 10,
#              input_shape      = c(14),
#              batch_size       = 10,
#              return_sequences = F,
#              stateful         = TRUE) %>%
#   #layer_lstm(units            = 10,
#   #           return_sequences = FALSE,
#   #           stateful         = T) %>%
#   layer_dense(units = 1)
# #model
# model %>%
#   compile(loss = 'mse', optimizer = 'adam')
# 
# history = model %>% fit(X_train, y_train,
#                         epochs = 25,
#                         #batch_size = 5,
#                         validation_split = 0.25)
# plot(history)
# 
# 
# pred_out = model %>% predict(X_test)
# 
# pred_out
# # View(pred_out)  
# model = keras_model_sequential()
# model %>%
#   layer_dense(units=512, activation = 'relu', 
#               input_shape = c(18)) %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=512, activation = 'relu') %>%
#   layer_dense(units=1)
#   
# #model
# model %>%
#   compile(loss = 'mse', 
#           metrics ="mae", 
#           optimizer = 'rmsprop')
# 
# history = model %>% fit(as.matrix(X_train), y_train,
#                         epochs = 50,
#                         batch_size = 5,
#                         validation_split = 0.2)
# #plot(history)
# 
# pred_out = model %>% predict(as.matrix(X_test))
# #png("Results/Figures/21sta_nL5_forecast_r14a.png", res = 100)
# plot(y_test, type="l", ylab = "Rolling 14-day average",
#      lwd = 2, ylim=c(min(pred_out), max(pred_out)))
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
# 
# library(MLmetrics)
# MAPE(y_pred = pred_out, y_true = y_test)
# 
# results_df = data.frame(y_test = y_test, pred_out = as.numeric(pred_out))
# head(results_df)
# 
# plot(results_df$y_test, type="l", ylab = "Rolling 14-day average",
#      lwd = 2, ylim=c(min(pred_out), max(pred_out)))
# lines(results_df$pred_out, col=2, lwd=2)
# points(results_df$pred_out, col=2, pch=12)
# legend("topleft", c("Proposed approach"), 
#        col="red", lty=1, lwd=2, cex = 0.9,
#        box.col = "white", inset = 0.01)
# #dev.off()
# 
# getMSE(results_df$y_test, results_df$pred_out)
# getMAE(results_df$y_test, results_df$pred_out)
# getMAPE(results_df$y_test, results_df$pred_out)
# getARV(results_df$y_test, results_df$pred_out)
# 
# 
# write.csv2(round(results_df,2), "resultsTestSet.csv", row.names = FALSE)
# 
# a = read.csv("resultsTestSet.csv")
# plot.ts(a$y_test)
# lines(a$pred_out, col=2, lwd=2)
# MAPE(y_pred = a$pred_out, y_true = a$y_test)
# 
# 
# fitness = function(parameters){
#   parameters = c(2, 7, 0.01)
#   
#   # Create Sliding window matrix
#   phi = floor(parameters[1]); 
#   w = floor(parameters[2]); 
#   alpha = parameters[3]
#   
#   #phi = 14
#   runningMeanincDia = getRunningMean(countryTimeSeries, phi)
#   plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
#   
#   #w = 14
#   #alpha = 0.1
#   timeSeriesnName = "runningMeanincDia"
#   timeSeries = runningMeanincDia
#   title = "Rolling 14-day average"
#   trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
#   #View(trendAnalysis_df[,c(1,w+5)])
#   #write.csv(trendAnalysis_df, paste("Results/", country, "_", timeSeriesnName, "_", w, "_", alpha,"_trendAnalysis_df.csv", sep=""))
#   
#   #generateGraph(runningMeanincDia, timeSeries = timeSeries, w = w, title = title)
#   
#   # Split data
#   set.seed(2311) 
#   sample = sample.split(runningMeanincDia, SplitRatio = .70)
#   train_ts = subset(countryTimeSeries, sample == TRUE); plot.ts(train_ts)
#   test_ts  = subset(countryTimeSeries, sample == FALSE); plot.ts(test_ts)
#   
#   m = round(length(runningMeanincDia)*0.8, 0)
#   m_n = length(runningMeanincDia)
#   train_ts = runningMeanincDia[1:m]; plot.ts(train_ts)
#   test_ts = runningMeanincDia[(m+1):m_n]; plot.ts(test_ts)
#   complete_ts = c(train_ts, test_ts); plot.ts(complete_ts)
#   
#   trendAnalysis_df = getTrendAnalysis(timeSeries_df = train_ts, 
#                                       w = w, 
#                                       alpha = alpha,
#                                       nStepAhead = nStepAhead) 
#   
#   trendAnalysisTest_df = getTrendAnalysis(timeSeries_df = test_ts, 
#                                           w = w, 
#                                           alpha = alpha,
#                                           nStepAhead = nStepAhead)
#   
#   # MinMax Scaling
#   preProc = preProcess(trendAnalysis_df[,1:w], method=c("range"))
#   normTrain = predict(preProc, trendAnalysis_df[,1:w])
#   normTest = predict(preProc, trendAnalysisTest_df[,1:w])
#   #View(norm); View(normTest)
#   
#   X_train = normTrain
#   y_train = trendAnalysis_df$nStepAhead
#   X_test = normTest
#   y_test = trendAnalysisTest_df$nStepAhead
#   
#   model = keras_model_sequential()
#   model %>%
#     layer_dense(units=512, activation = 'relu', 
#                 input_shape = c(w)) %>%
#     layer_dense(units=512, activation = 'relu') %>%
#     layer_dense(units=512, activation = 'relu') %>%
#     layer_dense(units=512, activation = 'relu') %>%
#     layer_dense(units=512, activation = 'relu') %>%
#     layer_dense(units=1)
#   
#   early_stopping <- callback_early_stopping(monitor = 'val_loss', 
#                                             patience = 2);
#   
#   #model
#   model %>%
#     compile(loss = 'mse', 
#             metrics ="mae", 
#             optimizer = 'rmsprop')
#   
#   history = model %>% fit(as.matrix(X_train), 
#                           y_train,
#                           epochs = 50,
#                           batch_size = 1,
#                           validation_split = 0.2,
#                           callbacks = c(early_stopping))
#   
#   
#   pred_out = model %>% predict(as.matrix(X_test))
#   return(getMSE(y_test, pred_out))
# }
# 
# 
# 
# lower = c(1, 7, 1e-4)
# upper = c(20, 20, 1e-1)
# SA = GenSA(lower = lower,
#            upper = upper,
#            fn = fitness,
#            control = list(max.time = 100,
#                           nb.stop.improvement = 2, 
#                           temperature = 10000,
#                           verbose = T,
#                           seed = 123)
# )
# 
# result = NULL
# result$phi = floor(SA$par[1])
# result$w = floor(SA$par[2])#, 0)) 
# result$alpha = SA$par[3]
# 
