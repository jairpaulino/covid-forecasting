model_LSTM = function(X_train, y_train){
  covariates = as.matrix(X_train)
  dim(covariates) = c(dim(covariates), 1)
  model_lstm = keras_model_sequential()
  model_lstm %>%
    layer_lstm(units = 10, input_shape = dim(covariates)[-1]) %>%
    #layer_lstm(units = 10) %>%
    #layer_activation('linear')
    layer_dense(units = 1, activation = 'linear')
  #layer_dense(units = 1) %>%
  model_lstm %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                            patience = 2);
  
  target = as.matrix(y_train)
  
  model_lstm %>%
    fit(x = covariates, 
        y = target,
        callbacks = c(early_stopping), 
        batch_size = 1, 
        epochs = 10, 
        validation_split = 0.2)
  
  prevTrainLSTM=predict(model_lstm, covariates)
  
  return(list(model_lstm = model_lstm, 
              Prev_Train_LSTM = Prev_Train_LSTM))
}
#prevendo treinamento LSTM
Prev_Train_LSTM = model_LSTM(X_train, y_train)