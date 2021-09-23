data = read.csv(file = "Data/time_series_covid19_confirmed_global.csv")#View(data)
data = data.frame(t(data))
colnames(data) = paste(data[2,], data[1,], sep="_"); View(data)

country = "US" #"US"
countryAdj = paste(country, "_", sep="")
# from Feb 01 2020
countryTimeSeries = diff(as.numeric(data[15:length(data[[1]]),countryAdj]))
plot.ts(countryTimeSeries)
#countryTimeSeries=ts(countryTimeSeries, frequency = 365.25, start=c(2020, 02))


# To create a new ts based on the original and phi parameter
phi = 14
runningMeanincDia = getRunningMean(countryTimeSeries, phi)
plot.ts(mm14incDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))

w = 14
alpha = 0.1
timeSeriesnName = "runningMeanincDia"
timeSeries = runningMeanincDia
title = "Rolling 14-day average"
trendAnalysis_df = getTrendAnalysis(timeSeries_df = timeSeries, w = w, alpha = alpha) 
#View(trendAnalysis_df[,c(1,w+5)])
#write.csv(trendAnalysis_df, paste("Results/", country, "_", timeSeriesnName, "_", w, "_", alpha,"_trendAnalysis_df.csv", sep=""))

generateGraph(trendAnalysis_df, timeSeries = timeSeries, w = w, title = title)

