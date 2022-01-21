get_pocid_from_mk = function(w, phi, alpha=0.05){
  matrix_mk_all = get_matrix_mk_all(normTrain
                                    , normValid, w = w, phi = phi, alpha = alpha)
  POCID_ls = NULL
  for (k in 1:nrow(matrix_mk_all$dataTrain)){#i=1
    dataTrain_mk_df = data.frame((matrix_mk_all$dataTrain[k,c(1:w, w+6)]))
    mk_w = as.numeric(dataTrain_mk_df[1,c(1:w)])
    indice = c(1:w)
    dataTrain_lm_df = data.frame("mk_w" = mk_w, "indice" = indice)
    model_lm_k = lm(dataTrain_lm_df$mk_w ~ dataTrain_lm_df$indice)
    pred = predict(model_lm_k, dataTrain_lm_df)
    #plot.ts(mk_w, lwd = 2, ylim=c(min(mk_w, pred), max(mk_w, pred))); lines(pred, col = 2, lwd = 2)
    POCID = getPOCID(target = mk_w, forecast = pred)
    POCID_ls[k] = POCID
  }  
  return(mean(POCID_ls))
} #get_pocid_from_mk(7, 13)

get_mk_parameters = function(normTrain, w_max=20, phi_max=20, alpha = 0.05){
  #w_max=20; phi_max=20; alpha = 0.05
  begin = proc.time()
  max_POCID = 0; w_best = NULL; phi_best = NULL
  for (i in 7:w_max){
    for (j in 7:phi_max){
      w = i; phi = j
      mean_POCID = get_pocid_from_mk(w = i, phi = j)
      if(mean_POCID > max_POCID){
        max_POCID = mean_POCID
        print(paste('w = ', w, ' phi = ', phi
                    , ' POCID = ', round(max_POCID)))
        w_best = i; phi_best = j
      }else{print(paste('w = ', w, ' phi = ', phi, ' --'))}
    }
  }#
  end = proc.time() - begin
  return(list('w' = w, 'phi' = phi
              , 'POCID' = max_POCID, 'Time' = end[3]))
}

