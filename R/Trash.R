noneTrendTrain = normTrain[which(normTrain$Class == "None"),]
positiveTrendTrain = normTrain[which(normTrain$Class == "Positive"),]
negativeTrendTrain = normTrain[which(normTrain$Class == "Negative"),]
#View(noneTrendTrain); View(positiveTrendTrain); View(negativeTrendTrain)

noneTrendTest = normTest[which(normTest$Class == "None"),]
positiveTrendTest = normTest[which(normTest$Class == "Positive"),]
negativeTrendTest = normTest[which(normTest$Class == "Negative"),]
#View(noneTrendTest); View(positiveTrendTest); View(negativeTrendTest)

# Modelo NONE
modelNone = keras_model_sequential()
modelNone %>%
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
modelNone %>%
  compile(loss = 'mse', 
          metrics ="mae", 
          optimizer = 'rmsprop')

history = modelNone %>% fit(as.matrix(noneTrendTest[,1:12]), 
                            noneTrendTest$nStepAhead,
                            epochs = 50,
                            batch_size = 5,
                            validation_split = 0.2,
                            callbacks = c(early_stopping))
#plot(history)

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
legend("topleft", c("Proposed approach"),
       col="red", lty=1, lwd=2, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()

getRMSE(normTest$nStepAhead, pred_out)
getMAE(normTest$nStepAhead, pred_out)
getMAPE(normTest$nStepAhead, pred_out)
getARV(normTest$nStepAhead, pred_out)

