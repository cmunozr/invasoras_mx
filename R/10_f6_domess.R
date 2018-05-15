domess <- function(folder_occ,
                   pattern_train,
                   dir_envcal,
                   dir_envproy,
                   dir_occ,
                   pattern_prec,
                   pattern_tem,
                   long_lat,
                   dir_write,
                   onlyclim,
                   pattern_factor,
                   type) {
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
  
  # extraer la información de las variables ambientales en las zonas de
  # calibracion
  reference_points <- extract(env_cal, occ_train[, long_lat])
  # mess
  mess.out <- mess(x = env_proy, v = reference_points, full = F)
  
  # crear carpeta dentro del folder dir_write con el nombre de la
  # especie y el area

  writeRaster(mess.out, file.path(paste0(
      dir_write,
      folder_occ,"_", type, "_","mess"
    )),
    format = "GTiff",
    overwrite = T
    )
  
  rm(list = ls())
  gc()
}
