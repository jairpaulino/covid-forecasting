getFinalData = function(time_series_train, time_series_test, phi, w
                        , alpha, nStepAhead, class){
 #phi = 7; w = 7; alpha = 0.1
  
  runningMean_train = getRunningMean(time_series_train, phi)
  runningMean_test = getRunningMean(time_series_test, phi)
  #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))

  trendAnalysis_df = getTrendAnalysis(timeSeries_df = runningMean_train
                                      , w = w
                                      , alpha = alpha) 
  #View(trendAnalysis_df)
  
  # Create Sliding window matrix
  trainTrendAnalysis_df = getTrendAnalysis(timeSeries_df = runningMean_test, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead)
  
  validTrendAnalysis_df = getTrendAnalysis(timeSeries_df = time_series_test, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead) 
  
  dataTrain = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == class),]
  dataTest = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == class),]

  data = NULL
  data$dataTrain = dataTrain
  data$dataTest = dataTest
  return(data)
}

#' getRunningMean
#' 
#' Calculate the running mean of a numeric vector.
#'
#' @param timeSeries A vector of numeric values.
#' @param n An integer indicating the running mean length. 
#'
#' @return An Array of running mean values.
#' @export
#'
#' @examplesa ts = 1:100; n = 3; getRunningMean(ts, n = 3)
getRunningMean = function(timeSeries, n){
  #a = 1:100;n = 3
  runningMean = NULL
  for(i in (n+1):(length(timeSeries))){#i=23
    runningMean[i] = mean(timeSeries[(i-n):(i-1)])
  }
  return(as.numeric(na.omit(runningMean)))
}

getNormalizeTS = function(array, min, max, lim_inf=0, lim_sup=1){
  #Normalize to [0, 1]
  range = max - min
  norm1 = (array - min) / range
  
  #Then scale to [x,y]
  range2 = lim_sup - lim_inf
  normalized = (norm1*range2) + lim_sup
  return(normalized)
}

denormalize = function(array, min, max, x, y){
  
  range2 = y - x
  norm1 = (array-x)/range2
  return(round(norm1*(max-min)+min, 1))
  #   return(nv*(max-min)+min)
}

getNormalizedData =  function(split.data, lim_inf = 0.2, lim_sup = 0.8){
  
  training_set = normalize(split.data[[1]], lim_inf, lim_sup)
  test_set = normalize(split.data[[2]], lim_inf, lim_sup)
  
  normalized.data = list()
  normalized.data$training_set = training_set
  normalized.data$test_set = test_set
  return(normalized.data)
}

# split.data = getSplitData(AirPassengers)
# normalized.data = getNormalizedData(split.data)



getSplitData = function(data, training_set_size=0.8){
  training_set = data[1:round(length(data)*(training_set_size))]
  test_set = data[(round((length(data)*training_set_size))+1):length(data)]
  split_data = list()
  split_data$training_set = training_set
  split_data$test_set = test_set
  return(split_data)
}

