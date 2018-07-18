# 25 de julio de 2017

Eval_bin <- function(path.thresholds = NA, Test_Occur = NA, Model = NA) {
  # Calcula metricas dependientes de umbral seg?f?'?,ºn el parametro E definido
  # siendo este la cantidad de error que pretendemos aceptar en un modelo.
  # Args:
  #   MTP: threshold of minimun training presence
  #   TTP: threshold of ten percentage presence
  #   Test_Occur: datos de evaluación
  #   Model: ruta de los archivos raster del modelo

  InRast <- raster(Model)
  
  data.threshold <- read.csv(path.thresholds)
  data <- data.threshold$data
  names(data) <- data.threshold$var
  
  MTP <- data["MTP"]
  TTP <- data["x10"]
  
  # MTP
  ## Convertir a binario
  MTP_bin <- InRast > MTP
  ## Numero de pixeles en la clase 1 y 0
  clases <- freq(MTP_bin)
  # Numero de pixeles totales (diferentes de NA)
  if (length(clases[which(clases[, 1] == 0), 2]) >= 1) {
    npixels <- unname(clases[which(clases[, 1] == 1), 2]) + clases[which(clases[, 1] == 0), 2]
  } else {
    npixels <- unname(clases[which(clases[, 1] == 1), 2])
  }
  # Proporcion del Area predicha
  MTP_area <- (unname(clases[which(clases[, 1] == 1), 2]) / npixels)
  # Prediccion
  prediccion <- extract(MTP_bin, Test_Occur)
  # Correctamente predicho positivamente
  exitos <- length(which(prediccion == 1))
  # Falsos negativos
  fracasos <- length(which(prediccion == 0))
  # Prueba binomial
  MTP_pbin <- pbinom(exitos, size = exitos + fracasos, prob = MTP_area)
  MTP_OR <- ((fracasos) / (fracasos + exitos))

  MTP_res <- cbind(MTP_area, MTP_pbin, MTP_OR, MTP)

  # TTP
  ## Convertir a binario
  TTP_bin <- InRast > TTP
  ## Numero de pixeles en la clase 1 y 0
  clases <- freq(TTP_bin)
  # Numero de pixeles totales (diferentes de NA)
  if (length(clases[which(clases[, 1] == 0), 2]) >= 1) {
    npixels <- clases[which(clases[, 1] == 1), 2] + clases[which(clases[, 1] == 0), 2]
  } else {
    npixels <- unname(clases[which(clases[, 1] == 1), 2])
  }
  # Proporcion del Area predicha
  TTP_area <- (unname(clases[which(clases[, 1] == 1), 2]) / npixels)
  # Prediccion
  prediccion <- extract(TTP_bin, Test_Occur)
  # Correctamente predicho positivamente
  exitos <- length(which(prediccion == 1))
  # Falsos negativos
  fracasos <- length(which(prediccion == 0))
  # Prueba binomial
  TTP_pbin <- pbinom(exitos, size = exitos + fracasos, prob = TTP_area)
  TTP_OR <- ((fracasos) / (fracasos + exitos))

  TTP_res <- cbind(TTP_area, TTP_pbin, TTP_OR, TTP)
  results <- cbind(MTP_res, TTP_res)


  return(results)
}
