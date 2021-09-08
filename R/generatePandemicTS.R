generateTS = function(){
  
  none_1 = round(abs(rnorm(n = 30, mean = 1, 1)), 0)
  pos_1 = sort(rexp(60, 0.0005))
  pos_1 = sort(rexp(60, 0.0005))
  
  none_2 = sort(rexp(10, 0.01), decreasing = T)
  pos_2= sort(rexp(10, 0.01), decreasing = T)
  
  plot.ts(c(none_1, pos_1, none_2))
  
   #a = rweibull(100, shape = 5, scale = 35)
   #plot.ts(sort(a))
   
}