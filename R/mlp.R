library(caTools)
library(caret)

# Split data
set.seed(2311) 
sample = sample.split(mm14incDia, SplitRatio = .70)
train_ts = subset(mm14incDia, sample == TRUE)
test_ts  = subset(mm14incDia, sample == FALSE)

# Create Sliding window matrix
trendAnalysis_df = getTrendAnalysis(timeSeries_df = train_ts, 
                                    w = w, 
                                    alpha = alpha,
                                    nStepAhead = 21) 
trendAnalysisTest_df = getTrendAnalysis(timeSeries_df = test_ts, 
                                        w = w, 
                                        alpha = alpha,
                                        nStepAhead = 21) 
#View(trendAnalysis_df)

# MinMax Scaling
preProc = preProcess(trendAnalysis_df[,1:18], method=c("range"))
normTrain = predict(preProc, trendAnalysis_df[,1:18])
normTest = predict(preProc, trendAnalysisTest_df[,1:18])
#View(norm); View(normTest)

X_train = normTrain
y_train = trendAnalysis_df$nStepAhead
X_test = normTest
y_test = trendAnalysisTest_df$nStepAhead

# LSTM model
library(keras)
library(tensorflow)

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
# View(pred_out)  
model = keras_model_sequential()
model %>%
  layer_dense(units=512, activation = 'relu', 
              input_shape = c(18)) %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=512, activation = 'relu') %>%
  layer_dense(units=1)
  
#model
model %>%
  compile(loss = 'mse', 
          metrics ="mae", 
          optimizer = 'rmsprop')

history = model %>% fit(as.matrix(X_train), y_train,
                        epochs = 50,
                        batch_size = 5,
                        validation_split = 0.2)
#plot(history)

pred_out = model %>% predict(as.matrix(X_test))
#png("Results/Figures/21sta_nL5_forecast_r14a.png", res = 100)
plot(y_test, type="l", ylab = "Rolling 14-day average",
     lwd = 2, ylim=c(min(pred_out), max(pred_out)))
lines(pred_out, col=2, lwd=2)
points(pred_out, col=2, pch=12)
legend("topleft", c("Proposed approach"), 
       col="red", lty=1, lwd=2, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()

getMSE(y_test, pred_out)
getMAE(y_test, pred_out)
getMAPE(y_test, pred_out)
getARV(y_test, pred_out)

library(MLmetrics)
MAPE(y_pred = pred_out, y_true = y_test)

results_df = data.frame(y_test = y_test, pred_out = as.numeric(pred_out))
head(results_df)

plot(results_df$y_test, type="l", ylab = "Rolling 14-day average",
     lwd = 2, ylim=c(min(pred_out), max(pred_out)))
lines(results_df$pred_out, col=2, lwd=2)
points(results_df$pred_out, col=2, pch=12)
legend("topleft", c("Proposed approach"), 
       col="red", lty=1, lwd=2, cex = 0.9,
       box.col = "white", inset = 0.01)
#dev.off()

getMSE(results_df$y_test, results_df$pred_out)
getMAE(results_df$y_test, results_df$pred_out)
getMAPE(results_df$y_test, results_df$pred_out)
getARV(results_df$y_test, results_df$pred_out)


write.csv2(round(results_df,2), "resultsTestSet.csv", row.names = FALSE)

a = read.csv("resultsTestSet.csv")
plot.ts(a$y_test)
lines(a$pred_out, col=2, lwd=2)
MAPE(y_pred = a$pred_out, y_true = a$y_test)
