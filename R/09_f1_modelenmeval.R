model.ENMeval <- function(folder_occ,
                          pattern_train,
                          pattern_bg,
                          dir_envcal,
                          dir_envproy,
                          dir_occ,
                          pattern_prec,
                          pattern_tem,
                          long_lat,
                          rm, dir_write,
                          onlyclim,
                          pattern_factor) {
  sp <- substr(folder_occ, 1, nchar(folder_occ) - 2)
  sp <- sub(
    pattern = "[[:punct:]]", replacement = "",
    basename(sp)
  )
  sp <- sub(
    pattern = "[[:digit:]]+", replacement = "",
    basename(sp)
  )

  # datos de entrenamiento y background
  occ_train <- read.csv(paste0(
    dir_occ, folder_occ, "/",
    sp, pattern_train
  ))
  occ_bg <- read.csv(paste0(
    dir_occ, folder_occ, "/",
    sp, pattern_bg
  ))

  # directorios de variables transformadas de la region de calibracion
  env_tem <- list.files(paste0(dir_envcal, folder_occ),
    pattern = pattern_tem, full.names = T
  )
  env_prec <- list.files(paste0(dir_envcal, folder_occ),
    pattern = pattern_prec, full.names = T
  )

  # directorios de variables transformadas de la region de proyeccion
  envproy_tem <- list.files(dir_envproy,
    pattern = pattern_tem, full.names = T
  )[1:length(env_tem)]
  envproy_prec <- list.files(dir_envproy,
    pattern = pattern_prec, full.names = T
  )[1:length(env_prec)]

  # leyendo las variables de calibracion y proyeccion
  # los directorios se dividen para poder leer el numero correcto de
  # variables de proyeccion segun las usadas en la calibracion
  # ya que maxent necesita las mismas variables con los mismos nombres
  # asi sea con diferentes extensiones y resolucion

  if (isTRUE(onlyclim)) {
    env_proy <- stack(c(envproy_prec, envproy_tem))
    env_cal <- stack(c(env_tem, env_prec))
  }
  else {
    env_hfp <- list.files(paste0(dir_envcal, folder_occ),
      pattern = pattern_factor, full.names = T
    )
    envproy_hfp <- list.files(dir_envproy,
      pattern = pattern_factor, full.names = T
    )
    env_proy <- stack(c(envproy_prec, envproy_tem, envproy_hfp))
    env_cal <- stack(c(env_tem, env_prec, env_hfp))
  }

  # crear modelos con enmeval
  mod <- ENMevaluate(
    occ = occ_train[, long_lat], env = env_cal,
    bg.coords = occ_bg[, long_lat],
    occ.grp = occ_train$bin, bg.grp = occ_bg$bin, method = "user",
    RMvalues = rm, overlap = F,
    clamp = F, bin.output = F, rasterPreds = T
  )

  # crear carpeta dentro del folder dir_write con el nombre de la
  # especie y el area
  dir.create(paste0(dir_write, folder_occ))
  dir_write2 <- paste0(dir_write, folder_occ, "/")

  # escribir los resultados de la evaluacion de ENMeval
  write.csv(mod@results, file = paste0(
    dir_write2,
    folder_occ, "_ENMeval.csv"
  ), row.names = F)

  # predecir los modelos logisticos en las areas de calibracion
  cal_log <- lapply(mod@models, function(x) predict(x, env_cal))

  # escribir los modelos logisticos
  for (i in 1:nrow(mod@results))
  {
    writeRaster(cal_log[[i]], file.path(paste0(
      dir_write2,
      folder_occ, "_",
      names(mod@predictions[[i]])
      , "_cal"
    )),
    format = "GTiff",
    overwrite = T
    )
  }

  # predecir los modelos logisticos en las areas de proyeccion
  proy_log <- lapply(mod@models, function(x) predict(x, env_proy))

  # escribir los modelos logisticos de las areas de proyeccion
  for (i in 1:nrow(mod@results))
  {
    writeRaster(proy_log[[i]], file.path(paste0(
      dir_write2,
      folder_occ, "_",
      names(mod@predictions[[i]])
      , "_proy"
    )),
    format = "GTiff",
    overwrite = T
    )
  }

  # escribir los data.frame de la importancia de las variables
  for (i in 1:nrow(mod@results))
  {
    write.csv(var.importance(mod@models[[i]]),
      file = paste0(
        dir_write2,
        folder_occ, "_", mod@results[i, 1], "_varimp.csv"
      ),
      row.names = F
    )
  }

  # escribir otros datos de interes como entropia, y los umbrales
  # binarios de Minimun Trainning presence y 10 percent omission
  # calculados por Maxent
  for (i in 1:nrow(mod@results))
  {
    write.csv(other.results(mod@models[[i]]),
      file = paste0(
        dir_write2,
        folder_occ, "_", mod@results[i, 1], "_other.csv"
      ),
      row.names = F
    )
  }

  rm(list = ls())
  gc()
}
