get_formula = function(matrix, w){
  formula_svm = "nStepAhead ~ "
  for(i in 1:w){#i=1
    if(i == 1){
      formula_svm = paste(formula_svm, paste(colnames(matrix)[i], sep=""), sep="") 
    }else{
      formula_svm = paste(formula_svm, paste(" + ", colnames(matrix)[i], sep=""), sep="") 
    }
  }
  return(formula_svm)
}

get_matrix_mk_all = function(time_series_train, time_series_valid, phi, w
                        , alpha=0.05, nStepAhead=1){
  #phi = 7; w = 7; alpha = 0.1; nStepAhead=1
  #time_series_train = normTrain; time_series_valid = normValid
  
  runningMean_train = getRunningMean(time_series_train, phi)
  runningMean_valid= getRunningMean(time_series_valid, phi)
  #plot.ts(runningMeanincDia, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
  
  trendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_train
                                      , w = w
                                      , alpha = alpha) 
  #View(trendAnalysis_df)
  
  # Create Sliding window matrix
  trainTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_valid, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead)
  
  validTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_valid, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead) 
  
  dataTrain = trainTrendAnalysis_df
  dataValid = validTrendAnalysis_df
  
  data = NULL
  data$dataTrain = dataTrain
  data$dataValid = dataValid
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

getFinalData = function(time_series_train, time_series_test, phi, w
                        , alpha=0.05, nStepAhead, Class){
  #time_series_train = dataTrain; time_series_test = dataValid
  #phi = 7; w = 14; alpha = 0.05; Class = 'Positive'
  
  runningMean_train = getRunningMean(time_series_train, phi)
  runningMean_test = getRunningMean(time_series_test, phi)
  #plot.ts(runningMean_train, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))
  
  #trendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_train
  #                                    , w = w
  #                                    , alpha = alpha) 
  #View(trendAnalysis_df)
  
  # Create Sliding window matrix
  trainTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_train, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead)
  
  validTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = time_series_test, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead) 
  
  dataTrain = trainTrendAnalysis_df[which(trainTrendAnalysis_df$Class == Class),]
  dataTest = validTrendAnalysis_df[which(validTrendAnalysis_df$Class == Class),]
  
  data = NULL
  data$dataTrain = dataTrain
  data$dataTest = dataTest
  return(data)
}
getFinalDataAll = function(time_series_train, time_series_test, phi, w
                        , alpha=0.05, nStepAhead){
  #time_series_train = dataTrain; time_series_test = dataValid
  #phi = 7; w = 14; alpha = 0.05; Class = 'Positive'
  
  runningMean_train = getRunningMean(time_series_train, phi)
  runningMean_test = getRunningMean(time_series_test, phi)
  #plot.ts(runningMean_train, ylab=paste("Rolling ", phi, "-day average (", country ,")", sep=""))

  trainTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = runningMean_train, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead)
  
  validTrendAnalysis_df = getTrendAnalysis(timeSeries_ts = time_series_test, 
                                           w = w, 
                                           alpha = alpha,
                                           nStepAhead = nStepAhead) 
  
  dataTrain = trainTrendAnalysis_df
  dataTest = validTrendAnalysis_df
  
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
# To determine whether the source must run under debug level of details or not.
#
# @param isToDebug A boolean. If TRUE, the 'debugSource' is consdireed, instead of 'source'.
#
# @return The desired source mode to be executed.
