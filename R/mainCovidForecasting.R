#Title: COVID-19 forecasting
#Author: Jair Paulino
#Date: Jul/2021

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Libraries
library(trend)
#library(mice)
library(tensorflow)
library(keras)

# Importing functions
source("R/linearAnalisys.R")
source("R/Auxiliar.R")

# Importing data ----
data = read.csv(file = "Data/UK_210319.csv", sep = ";")#View(data)
incDia = data$inc_dia; plot.ts(incDia)
incDia[incDia == 0] = 1
logIncDia = log(incDia); plot.ts(logIncDia) 
mm14incDia = getRunningMean(incDia, 14); plot.ts(mm14incDia)
mm14LogIncDia= getRunningMean(logIncDia, 14); plot.ts(mm14LogIncDia)

# Trend Analysis ----
# Analysis considering the daily time series incidence with a w = 14
#options(scipen = 999)
w = 14
trendAnalysis_df = getTrendAnalysis(mm14LogIncDia, w = w, alpha = 0.05) 
View(trendAnalysis_df[,c(1,w+5)])
write.csv(trendAnalysis_df, "Results/trendAnalysis_df.csv")
generateGraph

# Create data sets to 'None', 'Positive', and 'Negative' instances
noneTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "None"),]
positiveTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "Positive"),]
negativeTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "Negative"),]
#View(noneTrend); View(noneTrend); View(negativeTrend)

