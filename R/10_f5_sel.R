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
  # extraer el AICc y estandarizarlo de 0 a 1 (entre mas pequeÃ±o mejor)
  AICc_ <- (eval[, "AICc"] - (min(eval[, "AICc"]))) / (max(eval[, "AICc"]) - min(eval[, "AICc"]))
  # extraer la tasa de omision TTP y estandarizarlo de 0 a 1 (entre mas pequeÃ±o mejor)
  OR_cal <- (eval[, "TTP_OR_cal"] - (min(eval[, "TTP_OR_cal"]))) / (max(eval[, "TTP_OR_cal"] - min(eval[, "TTP_OR_cal"])))
  # extraer el area predicha y estandarizarla de 0 (mas grande) a 1(mas pequeÃ±o) (entre mas pequeÃ±o mejor)
  area_cal <- (eval[, "TTP_area_cal"] - (min(eval[, "TTP_area_cal"]))) / (min(eval[, "TTP_area_cal"] - max(eval[, "TTP_area_cal"]))) * -1
  # extraer proc y estandarizarla de 0 (mas grande) a 1 (mas bajos) (entre mas grande mejor)
  proc_cal <- ((max(eval[, "pROC_mean_cal"])) - eval[, "pROC_mean_cal"]) / (max(eval[, "pROC_mean_cal"] - min(eval[, "pROC_mean_cal"])))
  
  # Mejores en areas de proyeccion

  # extraer la tasa de omision TTP y estandarizarlo de 0 a 1 (entre mas pequeÃ±o mejor)
  OR_ <- (eval[, "TTP_OR_proy"] - (min(eval[, "TTP_OR_proy"]))) / (max(eval[, "TTP_OR_proy"] - min(eval[, "TTP_OR_proy"])))
  # extraer el area predicha y estandarizarla de 0 (mas grande) a 1(mas pequeÃ±o) (entre mas pequeÃ±o mejor)
  area_ <- (eval[, "TTP_area_proy"] - (min(eval[, "TTP_area_proy"]))) / (min(eval[, "TTP_area_proy"] - max(eval[, "TTP_area_proy"]))) * -1
  # extraer proc y estandarizarla de 0 (mas grande) a 1 (mas bajos) (entre mas grande mejor)
  proc_ <- ((max(eval[, "pRoc_proy"])) - eval[, "pRoc_proy"]) / (max(eval[, "pRoc_proy"] - min(eval[, "pRoc_proy"])))

  # calcular la distancia euclidiana de cada metrica al 0 y sumarla
  if (all(is.nan(OR_))) {
    dist <- (AICc_ * 0.5) + (proc_ * 0.5)
  } else {
    dist <- (OR_ * 0.4) + (AICc_ * 0.3) + (proc_ * 0.3)
  }
  
  # mejores modelos (minima distancia sumada)
  minimo <- which(dist == min(dist))

  best_x <- eval[minimo, ]

  # generar grafica de dispersion 3d (en el eje x [ancho] AICc,
  # en el eje y [profundo] tasa de omision, en el z [alto] el area
  # predicha)
  if (isTRUE(graf)) {
    library("scatterplot3d")
    windows()
    s3d <- scatterplot3d(
      x = AICc_, y = OR_,
      z = proc_, pch = 16, grid = TRUE, box = FALSE,
      type = "h", xlab = "AICc", ylab = "OR_cal", zlab = "ROCp_cal",
      color="gray52", main = "Espacio metrico de los modelos"
    )
  }
  return(best_x)
}
