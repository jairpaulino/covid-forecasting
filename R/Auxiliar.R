#' getRunningMean
#'
#' @param timeSeries A vector of numeric values.
#' @param n A integer indicating the running mean length. 
#'
#' @return A vector of running mean values.
#' @export
#'
#' @examplesa ts = 1:100; n = 3; getRunningMean(ts, n = 3)
getRunningMean = function(timeSeries, n){
  #a = 1:100;n = 3
  runningMean = NULL
  for(i in (n+1):(length(timeSeries))){#i=23
    runningMean[i] = mean(timeSeries[(i-n):(i-1)])
  }
  return(runningMean)
}

