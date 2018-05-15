# 8 de diciembre de 2017
# Aim: to calculate a pca for each accesible area by species
# Objetivo: calcular un pca para cada area accesible por especie sobre
# las variables climaticas
# @ dir_shp: character, specify the path of accesible area shapefile
# @ dir_Prec: character, specify the path of precipitation variables
# @ dir_save: character, specify the path to write the output
# @ dir_erase: chacter, specify the path of projection area
# value
# principal components with eigenvalues or variance above 1 in tif
# format
# summary table of analisys
# Brocken stick graph
# Loadings graph
# All output is written at dir_save path at least if argument show is F
# as default, when show is T you get a list with those results

env.M <- function(dir_shp, dir_Prec, dir_Tem, dir_hfp, dir_save,
                  erase= T, dir_erase, allcomp = F) {

  # nombre de la especie y el area M, o del area geografica a trabajar
  # este se extrae del directorio
  sp_dist <- sub(
    pattern = "(.*)\\..*$", replacement = "\\1",
    basename(dir_shp)
  )

  # cargar el directorio shp de M o area
  shp <- shapefile(dir_shp)

  # condicional para borrar partes del area accesible, en este
  # caso, zonas de mexico, ya que se solaparian el area de
  # calibracion y proyeccion confundiendo e inflando las metricas de
  # evaluacion
  if (isTRUE(erase)) {
    erase_dat <- shapefile(dir_erase)
    shp_M <- erase(shp, erase_dat)
  }

  # sin borrar zonas del area accesible
  else {
    shp_M <- shp
  }

  vars_P <- stack(dir_Prec)
  vars_T <- stack(dir_Tem)
  vars_hfp <- stack(dir_hfp)


  # cortes de las variables

  vars_P <- crop(x = vars_P, y = shp_M)
  vars_P <- mask(x = vars_P, mask = shp_M)

  vars_T <- crop(x = vars_T, y = shp_M)
  vars_T <- mask(x = vars_T, mask = shp_M)

  vars_hfp <- crop(x = vars_hfp, y = shp_M)
  vars_hfp <- mask(x = vars_hfp, mask = shp_M)

  # crear carpeta en donde se guarden los componentes, analisis e imagenes

  dir.create(paste0(dir_save, sp_dist))
  dir_write <- paste0(dir_save, sp_dist, "/")

  # guardar el indice de huella humana

  writeRaster(vars_hfp, file.path(
    dir_write,
    "human"
  ), format = "GTiff", overwrite = T)

  # PCA precipitacion

  # Analisis de componentes sobre 10000 puntos, no es posible
  # con toda la variable

  sampT <- randomPoints(vars_P, 10000)
  data <- extract(vars_P, sampT)

  # generar PCA con princomp ya que es un analisis tipo R y permite
  # generarlo sin scores, se usa la matriz de correlación en vez de
  # varianza porque la covarianza esta distorsionada por las
  # diferentes unidades de las variables
  pca_prec_5 <- princomp(data, scores = F, cor = T)
  map_pca_prec <- raster::predict(vars_P,
    model = pca_prec_5,
    index = 1:length(dir_Prec)
  )

  # darle nombre a los componentes
  names(map_pca_prec) <- paste0(
    rep("PC", length(dir_Prec)),
    seq(1, length(dir_Prec), 1)
  )

  # Resumen de varianza
  summary_pca <- summary(pca_prec_5)
  prop <- summary_pca$sdev^2 / sum(summary_pca$sdev^2)
  table_pca <- rbind(
    summary_pca$sdev, summary_pca$sdev^2,
    prop, cumsum(prop)
  )
  rownames(table_pca) <- c("sdve", "Eigenvalue", "Proporcion", "Prop. Acum")
  names(table_pca) <- paste0(
    rep("PC", length(dir_Prec)),
    seq(1, length(dir_Prec), 1)
  )
  write.csv(table_pca, paste0(dir_write, "summary_prec_est.csv"),
    row.names = T
  )

  # Guardando las variables raster del PCA

  if (isTRUE(allcomp)) {
    eigenvalue <- 0.0001
  }
  else {
    eigenvalue <- 1
  }

  index <- length(which(table_pca[1, ] > eigenvalue))
  for (j in 1:index) {
    names(map_pca_prec[[j]]) <- paste0(
      names(map_pca_prec[[j]]),
      "_prec_est"
    )
    writeRaster(map_pca_prec[[j]], file.path(
      dir_write,
      names(map_pca_prec[[j]])
    ), format = "GTiff", overwrite = T)
  }

  # Analisis grafico

  pdf(paste0(dir_write, "pca_pre_loadings.pdf"))
  coord_pca <- pca_prec_5$loadings[, 1:2]
  plot(coord_pca,
    asp = 1, t = "n", xlab = paste(round(prop[1], 3) * 100, "% de varianza explicada"),
    ylab = paste(round(prop[2], 3) * 100, "% de varianza explicada"), main = "PCA preciptacion"
  )
  abline(h = 0, v = 0, lty = 2)
  arrows(rep(0, length(dir_Prec)), rep(0, length(dir_Prec)),
    coord_pca[, 1], coord_pca[, 2],
    length = 0.05
  )
  text(coord_pca, labels = rownames(coord_pca), cex = 0.8, col = "red")
  dev.off()

  # "Diagrama de Broken para el PCA de precipitacion"
  # Diagrama de particion de varianza de broken

  pdf(paste0(dir_write, "pca_pre_broken.pdf"))
  screeplot(pca_prec_5, bstick = T, main = "", ylab = "Eigenvalue")
  dev.off()

  # PCA Temperatura

  # Analisis de componentes sobre 10000 puntos

  sampT <- randomPoints(vars_T, 10000)
  data <- na.omit(extract(vars_T, sampT))
  pca_tem_5 <- princomp(data, scores = F, cor = T)
  map_pca_tem <- raster::predict(vars_T, pca_tem_5,
    index = 1:length(dir_Tem)
  )
  names(map_pca_tem) <- paste0(
    rep("PC", length(dir_Tem)),
    seq(1, length(dir_Tem), 1)
  )

  # Resumen de varianza
  summary_pca <- summary(pca_tem_5)
  prop <- summary_pca$sdev^2 / sum(summary_pca$sdev^2)
  table_pca <- rbind(
    summary_pca$sdev, summary_pca$sdev^2,
    prop, cumsum(prop)
  )
  rownames(table_pca) <- c("sdve", "Eigenvalue", "Proporcion", "Prop. Acum")
  names(table_pca) <- paste0(
    rep("PC", length(dir_Tem)),
    seq(1, length(dir_Tem), 1)
  )
  write.csv(table_pca, paste0(dir_write, "summary_tem_est.csv"),
    row.names = T
  )
  # Guardando las variables raster del PCA

  index <- length(which(table_pca[1, ] > eigenvalue))
  for (j in 1:index) {
    names(map_pca_tem[[j]]) <- paste0(
      names(map_pca_tem[[j]]),
      "_tem_est"
    )
    writeRaster(map_pca_tem[[j]], file.path(
      dir_write,
      names(map_pca_tem[[j]])
    ), format = "GTiff", overwrite = T)
  }

  # Analisis grafico

  # Analisis grafico

  pdf(paste0(dir_write, "pca_tem_loadings.pdf"))
  coord_pca <- pca_tem_5$loadings[, 1:2]
  plot(coord_pca,
    asp = 1, t = "n", xlab = paste(round(prop[1], 3) * 100, "% de varianza explicada"),
    ylab = paste(round(prop[2], 3) * 100, "% de varianza explicada"), main = ""
  )
  abline(h = 0, v = 0, lty = 2)
  arrows(rep(0, length(dir_Prec)), rep(0, length(dir_Tem)),
    coord_pca[, 1], coord_pca[, 2],
    length = 0.05
  )
  text(coord_pca, labels = rownames(coord_pca), cex = 0.8, col = "red")
  dev.off()

  # "Diagrama de Broken para el PCA de precipitacion"
  # Diagrama de particion de varianza de broken

  pdf(paste0(dir_write, "pca_tem_broken.pdf"))
  screeplot(pca_tem_5, bstick = T, main = "", ylab = "Eigenvalue")
  dev.off()

  rm(list = ls())
  gc()

  return("ok")
}
