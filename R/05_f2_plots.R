# 13 de abril de 2017-- Carlos Jair Munoz Rodriguez
## fue modificada la funci?f³n pointsworld del paquete Biogeo
## Esta funcion permite graficar puntos de presencia de diferentes
## especies guardadas en una lista y luego la creacion de archivos PDF
## individuales con el nombre de la especie y el numero de puntos de
## presencia.
# world <- un mapa del mundo, region o localidad donde se quieren graficar
# dat <- base de datos con las columnas 'x' (longitud) y 'y' (latitud)

plot_points <-
  function(world, dat, lon, lat, sp) {
    cn <- names(dat) # names of columns
    xn <- (dat[, lon])
    yn <- (dat[, lat])
    xy <- cbind(xn, yn) # combine
    sp <- dat[, sp]

    g <- sp::SpatialPoints(xy) # convert points into SpatialPoints format
    maps::map(world, fill = TRUE, col = "white", bg = "lightblue", mar = c(0, 0, 0, 0))
    # plot map of world and set limits to bbox of points
    points(xn, yn, col = "red", pch = 16, cex = 0.5)
    # agrega el nombre de la especie y el n?fºmero de ocurrencias
    mtext(text = paste(sp[1], "_", length(sp), "_occ"), side = 3, adj = 0, line = 1.2, cex = 2, font = 2)
  }

# Esta funci?f³n modifica a plot_points, guarda los bosquejos realizados
# como un pdf de cada especie
# save_dir <- directorio a donde se quieren guardar los mapas
# type <- tipo de limpieza realizada

plot_save <-
  function(world, dat, lon, lat, sp, save_dir, type) {
    cn <- names(dat) # names of columns
    xn <- (dat[, lon])
    yn <- (dat[, lat])
    xy <- cbind(xn, yn) # combine
    sp <- dat[, sp]

    g <- sp::SpatialPoints(xy) # convert points into SpatialPoints format
    pdf(paste0(save_dir, type, sp[1], "_", length(sp), "_occ.pdf"), width = 11, height = 8.5)
    maps::map(world, fill = TRUE, col = "white", bg = "lightblue", mar = c(0, 0, 0, 0))
    # plot map of world and set limits to bbox of points
    points(xn, yn, col = "red", pch = 16, cex = 0.5)
    mtext(text = paste(sp[1], "_", length(sp), "_occ"), side = 3, adj = 0, line = 1.2, cex = 2, font = 2)
    # points(xn[f1],yn[f1],pch=22,col="black")
    dev.off()
  }
