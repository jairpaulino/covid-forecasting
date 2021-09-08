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
source("R/performanceMetrics.R")

# Importing data ----
country = "US" 
data = read.csv(file = paste("Data/", country, "_210319.csv", sep=""), sep = ";")#View(data)
incDia = data$inc_dia; plot.ts(incDia, ylab="Daily incidence"); length(incDia)
incDia[incDia == 0] = 1
logIncDia = log(incDia); plot.ts(logIncDia, ylab="Daily incidence (Log)"); length(logIncDia)
mm14incDia = getRunningMean(incDia, 14); plot.ts(mm14incDia, ylab="Rolling 14-day average"); length(mm14incDia)
mm14LogIncDia= getRunningMean(logIncDia, 14); plot.ts(mm14LogIncDia, ylab="Rolling 14-day average (Log)"); length(mm14LogIncDia)

# Trend Analysis ----
# Analysis considering the daily time series incidence with a w = 14
#options(scipen = 999)
w = 14
alpha = 0.01
timeSeriesnName = "mm14incDia"
timeSeries = mm14incDia
title = "Rolling 14-day average"
trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
#View(trendAnalysis_df[,c(1,w+5)])
write.csv(trendAnalysis_df, paste("Results/", country, "_", timeSeriesnName, "_", w, "_", alpha,"_trendAnalysis_df.csv", sep=""))
#View(trendAnalysis_df)
#png(paste("Results/Figures/", timeSeriesnName, "_", w, "_", alpha, ".png", sep=""))
generateGraph(trendAnalysis_df, timeSeries = timeSeries, w = w, title = title)
#dev.off()

# Create data sets to 'None', 'Positive', and 'Negative' instances
noneTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "None"),]
positiveTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "Positive"),]
negativeTrend = trendAnalysis_df[which(trendAnalysis_df$Class == "Negative"),]
#View(noneTrend); View(positiveTrend); View(negativeTrend)

length(positiveTrend$Class)
length(negativeTrend$Class)
length(noneTrend$Class)

