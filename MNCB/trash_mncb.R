w_best; phi_best; max_POCID

w = w_best; phi = phi_best
matrix_mk_all = get_matrix_mk_all(normTrain, normValid, phi = phi
                                  , w = w, alpha = alpha)
#View(matrix_mk_all$dataTrain); View(matrix_mk_all$dataValid) 
#plot.ts(normValid); lines(as.numeric(dataTrain_mk_df[1:w]), col=2)

POCID_ls = NULL; MAPE_ls = NULL; newMetric_ls = NULL
for (i in 1:nrow(matrix_mk_all$dataTrain)){#i=1
  dataTrain_mk_df = data.frame((matrix_mk_all$dataTrain[i,c(1:w, w+6)]))
  mk_w = as.numeric(dataTrain_mk_df[1,c(1:w)])
  indice = c(1:w)
  dataTrain_lm_df = data.frame("mk_w" = mk_w, "indice" = indice)
  model_lm_i = lm(dataTrain_lm_df$mk_w ~ dataTrain_lm_df$indice)
  pred = predict(model_lm_i, dataTrain_lm_df)
  #plot.ts(mk_w, lwd = 2, ylim=c(min(mk_w, pred), max(mk_w, pred))); lines(pred, col = 2, lwd = 2)
  POCID = getPOCID(target = mk_w, forecast = pred); POCID
  MAPE = getMAPE(target = mk_w, forecast = pred); MAPE
  newMetric = POCID/(1+MAPE)
  POCID_ls[i] = POCID
  MAPE_ls[i] = MAPE
  newMetric_ls[i] = newMetric
} #POCID_ls

summary_POCID = summary(POCID_ls); summary_POCID
summary_MAPE = summary(MAPE_ls); summary_MAPE
summary_newMetric = summary(newMetric_ls); summary_newMetric

