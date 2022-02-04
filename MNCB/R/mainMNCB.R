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
library(parallel)
 
# Importing functions
source("R/linearAnalisys.R")
source("R/Auxiliar.R")
source("R/performanceMetrics.R")
#source("R/OptimalSVR.R")
source('R/MkParameters.R')
source("R/OptimalSVR_gridSearch.R")
source("R/gridSearch.R")

#no_cores = detectCores()
#clust = makeCluster(no_cores-1)

# Importing data ----
# Argentina, Brazil, France, Germany, India, Iran
# Italy, Japan, Korea, South, Spain, United Kingdom, US
#countries = Argentina, Brazil, France, Germany
#for(i in length(countries)){
#        country = countries[i]
#}

country = "France" 
if(!dir.exists(file.path(getwd(), paste0("Results/", country)))){
        dir.create(file.path(getwd(), paste0("Results/", country)))
}
data = read.csv(file = paste("../Data/2021-10-26/", country, "_timeSeries.csv", sep=""), sep = "\t")#View(data)
incDia = data.frame(target = na.omit(data$target)); head(incDia, 2)
plot.ts(incDia$target, ylab="Daily incidence")

# Preprocessing ----
# Split data into train - test
m = round(length(incDia$target)*0.8, 0)
m_n = length(incDia$target)
train_valid_ts = incDia$target[1:m]; #plot.ts(train_valid_ts)
test_ts = incDia$target[(m+1):m_n]; #plot.ts(test_ts)
# Split data into train - valid
z = round(length(train_valid_ts)*0.625, 0)
z_p = round(length(train_valid_ts), 0)
train_ts = train_valid_ts[1:z]; #plot.ts(train_ts)
valid_ts = train_valid_ts[(z+1):z_p]; #plot.ts(valid_ts)
# MinMax Scaling
normTrain = getNormalizeTS(train_ts, min=min(train_ts), max=max(train_ts))
normValid= getNormalizeTS(valid_ts, min=min(train_ts), max=max(train_ts))
normTest = getNormalizeTS(test_ts, min=min(train_ts), max=max(train_ts))
#plot.ts(normTrain); plot.ts(normValid); plot.ts(normTest)

#mkParameters = get_mk_parameters(normTrain, w_max = 30, phi_max = 30)
#write.csv(mkParameters, file = paste("Results/", country, "/", country , "_mkParameters.csv", sep=""), row.names = F)
mkParameters = list(7, 10)
rslt_SVRMKCD = getModelSVR_MKCD_gridSearch(dataTrain = normTrain
                           , dataValid = normValid
                           , dataTest = normTest
                           , w = as.numeric(mkParameters[1])
                           , phi = as.numeric(mkParameters[2])
                           , nStepAhead = 1, alpha = 0.05)
#length(normTest); length(rslt_SVRMKCD$MKCD_SVM_Test)
# SVR MODEL - phi + w
rslt_SVR_wp = getModelSVR_w_phi_GS(dataTrain = normTrain
                             , dataValid = normValid
                             , dataTest = normTest
                             , w = as.numeric(mkParameters[1])
                             , phi = as.numeric(mkParameters[2])
                             , nStepAhead = 1, alpha = 0.05)
#length(normTest); length(rslt_SVR_wp$svr_Test)
# SVR MODEL - Simple
rslt_SVR_normal = getModelSVR_Normal_GS(dataTrain = normTrain
                                 , dataValid = normValid
                                 , dataTest = normTest
                                 , w = as.numeric(mkParameters[1])
                                 , nStepAhead = 1, alpha = 0.05)
#length(normTest); length(rslt_SVR_normal$svr_Test)

w = as.numeric(mkParameters[1]); phi = as.numeric(mkParameters[2])
alpha = 0.05; nStepAhead = 1
# Calculate the
metricsTable = as.data.frame(matrix(ncol=3, nrow=5))
colnames(metricsTable) = c("SVM", "SVM_WP", "SVM_MKCD")
rownames(metricsTable) = c("RMSE", "MAE", "MAPE", "ARV", "Theil")
#(length(c(normTrain, normValid))-w-phi); length(rslt_SVR$svr_Train)
normTrainValid_metric = c(normTrain[(phi+w+1):length(normTrain)], normValid)
len = length(rslt_SVR_normal$svr_Train)
metricsTable$SVM[1] = getRMSE(normTrainValid_metric, rslt_SVR_normal$svr_Train) #Normal
metricsTable$SVM_WP[1] = getRMSE(normTrainValid_metric, rslt_SVR_wp$svr_Train[phi:len])
metricsTable$SVM_MKCD[1] = getRMSE(normTrainValid_metric, rslt_SVRMKCD$MKCD_SVM_Train)
#
metricsTable$SVM[2] = getMAE(normTrainValid_metric, rslt_SVR_normal$svr_Train) #Normal
metricsTable$SVM_WP[2] = getMAE(normTrainValid_metric, rslt_SVR_wp$svr_Train[phi:len])
metricsTable$SVM_MKCD[2] = getMAE(normTrainValid_metric, rslt_SVRMKCD$MKCD_SVM_Train)
#
metricsTable$SVM[3] = getMAPE(normTrainValid_metric, rslt_SVR_normal$svr_Train) #Normal
metricsTable$SVM_WP[3] = getMAPE(normTrainValid_metric, rslt_SVR_wp$svr_Train[phi:len])
metricsTable$SVM_MKCD[3] = getMAPE(normTrainValid_metric, rslt_SVRMKCD$MKCD_SVM_Train)
#
metricsTable$SVM[4] = getARV(normTrainValid_metric, rslt_SVR_normal$svr_Train) #Normal
metricsTable$SVM_WP[4] = getARV(normTrainValid_metric, rslt_SVR_wp$svr_Train[phi:len])
metricsTable$SVM_MKCD[4] = getARV(normTrainValid_metric, rslt_SVRMKCD$MKCD_SVM_Train)
#
metricsTable$SVM[5] = getTheil(normTrainValid_metric, rslt_SVR_normal$svr_Train) #Normal
metricsTable$SVM_WP[5] = getTheil(normTrainValid_metric, rslt_SVR_wp$svr_Train[phi:len])
metricsTable$SVM_MKCD[5] = getTheil(normTrainValid_metric, rslt_SVRMKCD$MKCD_SVM_Train)

#options(scipen=999)
print(round(metricsTable,3))

# nStepAhead=1; w=mkParameters[1]; phi=mkParameters[2]; alpha=0.05
# write.csv(metricsTable, paste("Results/train_metrics", country, "_", nStepAhead ,"sta_"
#                                    , "w-", w, "_phi-", phi, "_alpha-", alpha
#                                    ,".csv", sep=""), row.names = FALSE)

#rst_test_plot = c(rep(NA, (phi+w)), rslt_SVR$svr_Train)
#plot.ts(c(normTrain, normValid), col=1, lwd=1)
#lines(rst_test_plot, col=2, lwd = 2, lty=2)
#length(rslt_SVR$svr_Train); length(normTrainValid_metric)
nStepAhead=1
write.csv(metricsTable, file = paste("Results/", country, "/", "train_", country, "_", nStepAhead ,"sta_"
                                     , "w-", w, "_phi-", phi, "_alpha-", alpha,".csv", sep=""))


metricsTableTest = as.data.frame(matrix(ncol=3, nrow=5))
colnames(metricsTableTest) = c("SVM", "SVM_WP", "SVM_MKCD")
rownames(metricsTableTest) = c("RMSE", "MAE", "MAPE", "ARV", "Theil")
#(length(c(normTest, normValid))-w-phi); length(rslt_SVR$svr_Test)
normTestValid_metric = c(normTest[(phi+w+1):length(normTest)], normValid)
len = length(rslt_SVR_normal$svr_Test)
metricsTableTest$SVM[1] = getRMSE(normTestValid_metric, rslt_SVR_normal$svr_Test) #Normal
metricsTableTest$SVM_WP[1] = getRMSE(normTestValid_metric, rslt_SVR_wp$svr_Test[phi:len])
metricsTableTest$SVM_MKCD[1] = getRMSE(normTestValid_metric, rslt_SVRMKCD$MKCD_SVM_Test)
#
metricsTableTest$SVM[2] = getMAE(normTestValid_metric, rslt_SVR_normal$svr_Test) #Normal
metricsTableTest$SVM_WP[2] = getMAE(normTestValid_metric, rslt_SVR_wp$svr_Test[phi:len])
metricsTableTest$SVM_MKCD[2] = getMAE(normTestValid_metric, rslt_SVRMKCD$MKCD_SVM_Test)
#
metricsTableTest$SVM[3] = getMAPE(normTestValid_metric, rslt_SVR_normal$svr_Test) #Normal
metricsTableTest$SVM_WP[3] = getMAPE(normTestValid_metric, rslt_SVR_wp$svr_Test[phi:len])
metricsTableTest$SVM_MKCD[3] = getMAPE(normTestValid_metric, rslt_SVRMKCD$MKCD_SVM_Test)
#
metricsTableTest$SVM[4] = getARV(normTestValid_metric, rslt_SVR_normal$svr_Test) #Normal
metricsTableTest$SVM_WP[4] = getARV(normTestValid_metric, rslt_SVR_wp$svr_Test[phi:len])
metricsTableTest$SVM_MKCD[4] = getARV(normTestValid_metric, rslt_SVRMKCD$MKCD_SVM_Test)
#
metricsTableTest$SVM[5] = getTheil(normTestValid_metric, rslt_SVR_normal$svr_Test) #Normal
metricsTableTest$SVM_WP[5] = getTheil(normTestValid_metric, rslt_SVR_wp$svr_Test[phi:len])
metricsTableTest$SVM_MKCD[5] = getTheil(normTestValid_metric, rslt_SVRMKCD$MKCD_SVM_Test)
#options(scipen=999)
print(round(metricsTableTest,3))


write.csv(metricsTableTest, paste("Results/", country, "/", "test_metrics", country, "_", nStepAhead ,"sta_"
                                   , "w-", w, "_phi-", phi, "_alpha-", alpha
                                   ,".csv", sep=""), row.names = T)
           
#length(normTest); length(rslt_SVR$svr_Test)
#plot.ts(rslt_SVR$svr_Test)
#lines(rslt_SVRMKCD$MKCD_SVM_Test, col=2, lwd=2, lty=2)

png(paste("Results/", country, "/","train_", country, "_", nStepAhead ,"sta_"
          , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
min_ = min(normTrain, rslt_SVR_normal$svr_Train, rslt_SVR_wp$svr_Train, rslt_SVRMKCD$MKCD_SVM_Train)
max_ = 1.1*max(normTrain, rslt_SVR_normal$svr_Train, rslt_SVR_wp$svr_Train, rslt_SVRMKCD$MKCD_SVM_Train)
plot(rslt_SVRMKCD$Target_Train, type="l", ylab = paste("Rolling ", as.numeric(mkParameters[2]),"-day average", sep='')
     , lwd = 3, ylim=c(min_, max_))
len = length(rslt_SVR_normal$svr_Train)
lines(rslt_SVR_normal$svr_Train[(phi+1):len], col=3, lwd=2)
#points(rslt_SVR$svr_Train, col=3, pch=15)
lines(rslt_SVR_wp$svr_Train, col=4, lwd=2)
#points(rslt_SVRMKCD$MKCD_SVM_Train, col=2, pch=15)
lines(rslt_SVRMKCD$MKCD_SVM_Train, col=2, lwd=2)
#points(rslt_SVRMKCD$MKCD_SVM_Train, col=2, pch=15)
legend("topleft", c("TS", "SVM", "SVM-phi", "SVM-MKCD"),
       col=c(1,3,4,2), lty=1, lwd=3, cex = 0.8,
       box.col = "white", inset = 0.01, horiz = F)
dev.off()

png(paste("Results/", country, "/", "test_", country, "_", nStepAhead ,"sta_"
          , "w-", w, "_phi-", phi, "_alpha-", alpha, ".png", sep=""), res = 100, width = 800, height = 600)
min_ = min(normTest, rslt_SVR_normal$svr_Test, rslt_SVR_wp$svr_Test, rslt_SVRMKCD$MKCD_SVM_Test)
max_ = 1.1*max(normTest, rslt_SVR_normal$svr_Test, rslt_SVR_wp$svr_Test, rslt_SVRMKCD$MKCD_SVM_Test)
plot(normTest, type="l", ylab = paste("Rolling ",as.numeric(mkParameters[2]),"-day average", sep='')
     , lwd = 2)#, ylim=c(min_, max_))
lines(rslt_SVR_normal$svr_Test, col=3, lwd=2)
lines(rslt_SVR_wp$svr_Test, col=4, lwd=2)
lines(rslt_SVRMKCD$MKCD_SVM_Test, col=2, lwd=2)
legend("topright", c("TS", "SVM", "SVM-phi", "SVM-MKCD"),
       col=c(1,3,4,2), lty=1, lwd=3, cex = 0.8,
       box.col = "white", inset = 0.01, horiz = F)
dev.off()

#stopCluster(clust)

