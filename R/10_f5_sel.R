# Por: Carlos MuÃ±oz-22 de agosto de 2017
# objetivo: automatizar la eleccion de los modelos, a traves
# del 'balance' de la suma euclidiana entre la distancia de
# las metricas estandarizadas de 0 a 1: AICc, tasa de omision,
# area predicha.

# funcion eleccion:'balance' de la suma euclidiana entre la distancia de
# las metricas estandarizadas de 0 a 1: AICc, tasa de omision TTP,
# area predicha.
# @ x=numero o nombre de la especie de la que se eligiran los modelos
# @ path.eval=ruta de la carpeta en donde estan las diferentes replicas
# a evaluar
# @ path.model= ruta de la carpeta en donde estan los modelos
# @ graf= valor logico por default False, si es True genera grafica
# de dispersion 3d (en el eje x [ancho] AICc, en el eje y
# [profundo] tasa de omision, en el z [alto] el area predicha)

library(data.table)

selection <- function(sp, path.eval="output/10_eval_all/",
                      graf=F, pattern.type) {
  eval <- read.csv(file = paste0(path.eval, sp, pattern.type))
  eval <- na.omit(eval)
  # Mejores en areas de calibraci?n
  proc_cal <- eval[, "pROC_mean_cal"]
  index_proc <- which(proc_cal > 1)
  eval_1 <- eval[index_proc, ] 
  
  p_proc_cal <- eval_1[, "p_valor_mean_cal"]
  index_p_proc <- which(p_proc_cal <= 0.05)
  eval_2 <- eval_1[index_p_proc, ]
  
  OR_cal <- eval_2[, "TTP_OR_cal"]
  index_OR_cal <- which(OR_cal <= 0.10)
  eval_3 <- eval_2[index_OR_cal, ]
  
  pbin_OR_cal <- 1-(eval_3[, "TTP_bin_cal"])
  index_pbin_OR_cal <- which(pbin_OR_cal <= 0.05)
  eval_4 <- eval_3[index_pbin_OR_cal, ]
  
  AICc_15 <- summary(eval[, "AICc"])[2]
  index_aicc <- which(eval_4[, "AICc"] < AICc_15)
  eval_5 <- eval_4[index_aicc, ]
  
  proc_proy <- eval_5[, "pRoc_proy"]
  index_proc_proy <- which(proc_proy > 1)
  eval_6 <- eval_5[index_proc_proy, ] 
  
  p_proc_proy <- eval_6[, "p_valor_proy"]
  index_p_proc_proy <- which(p_proc_proy <= 0.05)
  eval_7 <- eval_6[index_p_proc_proy, ]
  
  OR_proy <- eval_7[, "TTP_OR_proy"]
  index_OR_proy <- which(OR_proy <= 0.10)
  eval_8 <- eval_7[index_OR_proy, ]
  
  pbin_OR_proy <- 1-(eval_8[, "TTP_bin_proy"])
  index_pbin_OR_proy <- which(pbin_OR_proy <= 0.05)
  eval_9 <- eval_8[index_pbin_OR_proy, ]
  
  index_best <- which(min(eval_9[, "TTP_area_proy"])== eval_9[, "TTP_area_proy"])
  best <- eval_9[index_best, ]
  
  return(best)
}
