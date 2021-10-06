###
noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
#View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)

noneTrendTest = normTest[which(normTest$Class == "None"),]
positiveTrendTest = normTest[which(normTest$Class == "Positive"),]
negativeTrendTest = normTest[which(normTest$Class == "Negative"),]
#View(noneTrendTest); View(positiveTrendTest); View(negativeTrendTest)

# Modelo NONE
covariates = as.matrix(noneTrendTrain[,1:w])
dim(covariates) = c(dim(covariates), 1)
modelNone = keras_model_sequential()
modelNone %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = dim(covariates)[-1]) %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1,  activation = 'linear')

modelNone %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

modelNone %>%
  fit(x = covariates, 
      y = noneTrendTrain$nStepAhead,
      callbacks = c(early_stopping), 
      batch_size = 1, 
      epochs = 50, 
      validation_split = 0.2)
#plot(history)

# Modelo Positive
covariates = as.matrix(positiveTrendTrain[,1:w])
dim(covariates) = c(dim(covariates), 1)
modelPositive = keras_model_sequential()
modelPositive %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = dim(covariates)[-1]) %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1,  activation = 'linear')

modelPositive %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

modelPositive %>%
  fit(x = covariates, 
      y = positiveTrendTrain$nStepAhead,
      callbacks = c(early_stopping), 
      batch_size = 1, 
      epochs = 50, 
      validation_split = 0.2)

# Modelo Negative
covariates = as.matrix(negativeTrendTrain[,1:w])
dim(covariates) = c(dim(covariates), 1)
modelNegative = keras_model_sequential()
modelNegative %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = dim(covariates)[-1]) %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1,  activation = 'linear')

modelNegative %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

modelNegative %>%
  fit(x = covariates, 
      y = negativeTrendTrain$nStepAhead,
      callbacks = c(early_stopping), 
      batch_size = 1, 
      epochs = 50, 
      validation_split = 0.2)


# Normal 
covariates = as.matrix(normTrain[,1:w])
dim(covariates) = c(dim(covariates), 1)
modelNormal = keras_model_sequential()
modelNormal %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = dim(covariates)[-1]) %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
    layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1,  activation = 'linear')

modelNormal %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 5);

modelNormal %>%
  fit(x = covariates, 
      y = normTrain$nStepAhead,
      callbacks = c(early_stopping), 
      batch_size = 1, 
      epochs = 50, 
      validation_split = 0.2)


covariatesTest = as.matrix(normTest[,1:w])
dim(covariatesTest) = c(dim(covariatesTest), 1)
predNormal_out = predict(modelNone, covariatesTest)
View(covariatesTest)

pred_out = NULL
for(i in 1:length(normTest$nStepAhead)){#i=85
  if(normTest$Class[i] == "None"){
    pred_out[i] = predict(modelNone, covariatesTest)[i]
  }else{
    if(normTest$Class[i] == "Positive"){
      pred_out[i] = predict(modelPositive, covariatesTest)[i]
    }else{
      pred_out[i] = predict(modelNegative, covariatesTest)[i]
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

l = length(countryTimeSeries)
a = length(pred_out)
nts = countryTimeSeries[(l-a):l]#plot.ts(countryTimeSeries[(l-a):l])
plot(nts, type="l", ylab = paste("Rolling ",w,"-day average", sep=''),
     lwd = 2, ylim=c(min(min(pred_out),min(nts)), 
                     max(max(pred_out),max(nts))))
lines(pred_out, col=2, lwd=2)
points(pred_out, col=2, pch=12)
lines(predNormal_out, col=3, lwd=2)
points(predNormal_out, col=3, pch=12)
legend("topleft", c("MKCD - MLP", "MLP"),
       col=c(2,3), lty=1, lwd=3, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()


#model_LSTM = function(X_train, y_train){
# covariates = as.matrix(X_train)
# dim(covariates) = c(dim(covariates), 1)
# model_lstm = keras_model_sequential()
# model_lstm %>%
#   layer_lstm(units = 10, input_shape = dim(covariates)[-1]) %>%
#   #layer_lstm(units = 10) %>%
#   #layer_activation('linear')
#   layer_dense(units = 1, activation = 'linear')
# #layer_dense(units = 1) %>%
# model_lstm %>% compile(
#   optimizer = "rmsprop",
#   loss = "mse",
#   metrics = c("mae")
# )
# early_stopping <- callback_early_stopping(monitor = 'val_loss', 
#                                           patience = 5);
# 
# target = as.matrix(y_train)
# 
# model_lstm %>%
#   fit(x = covariates, 
#       y = target,
#       callbacks = c(early_stopping), 
#       batch_size = 1, 
#       epochs = 50, 
#       validation_split = 0.2)
# 
# prevTrainLSTM=predict(model_lstm, covariates)
# 
# return(list(model_lstm = model_lstm, 
#             Prev_Train_LSTM = Prev_Train_LSTM))
# }
# #prevendo treinamento LSTM
# Prev_Train_LSTM = model_LSTM(X_train, y_train)
# 
# 
# 
# 


