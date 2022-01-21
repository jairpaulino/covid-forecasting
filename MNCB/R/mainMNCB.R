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
library(GenSA)      # Generalized dimulated annealing
 
# Importing functions
source("R/linearAnalisys.R")
source("R/Auxiliar.R")
source("R/performanceMetrics.R")
source("R/OptimalSVR.R")

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

mkParameters = 

results = getModelSVR_MKCD(normTrain, normValid, nStepAhead = 1)


#write.csv2(incDia, paste("Data/Country/", country, ".csv", sep="")
#           , row.names = FALSE)

