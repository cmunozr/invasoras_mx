# compare of maps

map_comp <- function(path.a = clim,
                     path.b = climhum,
                     path.c = clim_plushum,
                     mod.proy = "ModelFile_proy",
                     threshold = "TTP_cal",
                     path.fig = "figs/",
                     path.maps = "output/11_comp_map/",
                     name.a = "clim",
                     name.b = "climhum",
                     name.c = "clim_plushum",
                     path.mx = "data/geo_shp/mex_4km_shp/conto4mgw.shp",
                     spp_model = comp) {
  
  comm_sp <- as.character(comp[which(!is.na(spp_model[, "comp"])), "Especie"])
  
  a <- read.csv(path.a)
  
  i <- seq(1,nrow(a), by = 1)
  for(j in 1:nrow(a)) {
    if (length(grep(comm_sp[j], a$ModelFile_proy)) != 0) 
    {i[j] <- (comm_sp[j])}else{i[j] <- NA}
  }
  a <- a[!is.na(i),]
  
  b <- read.csv(path.b)
  i2 <- seq(1,nrow(b), by = 1)
  for(j in 1:nrow(b)) {
    if (length(grep(comm_sp[j], b$ModelFile_proy)) != 0) 
    {i2[j] <- (comm_sp[j])}else{i2[j] <- NA}
  }
  b <- b[!is.na(i2), ]
  
  c <- read.csv(path.c)
  i3 <- seq(1,nrow(c), by = 1)
  for(j in 1:nrow(c)) {
    if (length(grep(comm_sp[j], c$ModelFile_proy)) != 0) 
    {i3[j] <- (comm_sp[j])}else{i3[j] <- NA}
  }
  c <- c[!is.na(i3), ]
  
  shp.mx <- shapefile(path.mx)
  dir.create(paste0(path.fig, "11_comp_map"), showWarnings = F)
  dir.write <- paste0(path.fig, "/11_comp_map/")

  objects_ <- list(a, b, c)
  objects_results <- rep(list(1), length(objects_))
  for (i in 1:length(objects_))
  {
    index <- c("a", "b", "c")
    mod <- objects_[[i]][, c(mod.proy, threshold)]
    namekm <- basename(dirname(as.character(mod[, mod.proy])))
    sp <- substr(namekm, 1, nchar(namekm) - 2)
    sp <- sub(
      pattern = "[[:punct:]]", replacement = "",
      basename(sp)
    )
    sp <- sub(
      pattern = "[[:digit:]]+", replacement = "",
      basename(sp)
    )
    mod$sp <- sp
    mod <- mod[order(mod[, "sp"]), ]

    stackras_ <- stack()
    for (j in 1:nrow(mod))
    {
      ras <- raster(as.character(mod[, mod.proy][j]))
      stackras_ <- stack(stackras_, ras)
    }
    objects_results[[i]] <- stackras_
    names(objects_results) <- c(
      "stackras_a", "stackras_b",
      "stackras_c"
    )
  }

  diffmap_ab <- stack()
  diffmap_ac <- stack()
  cors_ <- rep(list(1), nrow(mod))

  for (i in 1:nrow(mod))
  {
    a_map <- objects_results[[1]][[i]]
    b_map <- objects_results[[2]][[i]]
    c_map <- objects_results[[3]][[i]]

    cor_ab <- cbind(as.character(sp[i]), cor_T(a_map, b_map))
    cor_ac <- cbind(cor_T(a_map, c_map))
    cors <- cbind(cor_ab, cor_ac)
    colnames(cors) <- c("sp", "Cor.ab", "Var", "Cor.ac", "Var")
    cors_[[i]] <- cors

    diffmap_1 <- b_map - a_map
    diffmap_2 <- c_map - a_map
    diffmap_ab <- stack(diffmap_ab, diffmap_1)
    diffmap_ac <- stack(diffmap_ac, diffmap_2)

    # writting raster

    writeRaster(diffmap_1, paste0(
      path.maps, as.character(sp[i]),
      "_", name.a, "_", name.b, ".tif"
    ), overwrite = T)
    writeRaster(diffmap_2, paste0(
      path.maps, as.character(sp[i]),
      "_", name.a, "_", name.c, ".tif"
    ), overwrite = T)


    # plotting

    cuts_idon <- seq(0, 1, by = 0.1)
    pal_idon <- colorRampPalette(c("white", "forestgreen"))


    # centering white color in 0
    nHalf <- length(a_map) / 2
    Min <- -1
    Max <- 1
    Thresh <- 0

    pal1 <- colorRampPalette(colors = c("blue", "white"))(nHalf)
    pal2 <- colorRampPalette(colors = c("white", "red"))(nHalf)
    pal_diff <- c(pal1, pal2)
    pal_diff[c(nHalf, nHalf + 1)] <- rgb(t(col2rgb("white")), maxColorValue = 256)
    cuts1 <- seq(Min, Thresh, length.out = nHalf + 1)
    cuts2 <- seq(Thresh, Max, length.out = nHalf + 1)[-1]
    cuts_diff <- c(cuts1, cuts2)

    pdf(paste0(dir.write, "11_", sp[i], "_comp_map.pdf"), width = 10, height = 8)
    par(mfrow = c(2, 3), oma = c(3, 0, 0, 0), mar = c(5, 2, 1, 1))
    plot(a_map,
      breaks = cuts_idon, col = pal_idon(10),
      legend = F
    )
    plot(shp.mx, add = TRUE)
    leg <- legend(-95, 32, name.a, box.lwd = 0, box.col = "white")
    plot(b_map,
      breaks = cuts_idon, col = pal_idon(10),
      legend = F
    )
    plot(shp.mx, add = TRUE)
    leg <- legend(-95, 32, name.b, box.lwd = 0, box.col = "white")
    plot(c_map,
      breaks = cuts_idon, col = pal_idon(10),
      legend.only = F, legend.width = 1, legend.shrink = 0.75,
      axis.args = list(
        at = seq(0, 1, 0.2),
        labels = seq(0, 1, 0.2), cex.axis = 0.6
      ),
      legend.args = list(
        text = "Idoneidad", side = 1, font = 2,
        line = 2.5, cex = 0.8
      ), horizontal = TRUE
    )
    plot(shp.mx, add = TRUE)
    leg <- legend(-95, 32, name.c, box.lwd = 0, box.col = "white")
    plot.new()
    title(main = sp[i])
    plot(diffmap_1,
      breaks = cuts_diff, col = pal_diff,
      legend = F
    )
    plot(shp.mx, add = TRUE)

    plot(diffmap_2,
      breaks = cuts_diff, col = pal_diff,
      legend.only = F, legend.width = 1, legend.shrink = 0.75,
      axis.args = list(
        at = seq(-1, 1, 0.2),
        labels = seq(-1, 1, 0.2), cex.axis = 0.8
      ),
      legend.args = list(
        text = paste0("Diferencia "), side = 1, font = 2,
        line = 2.5, cex = 0.8
      ), horizontal = T
    )
    plot(shp.mx, add = TRUE)
    dev.off()
  }

  sum_diffab <- sum(diffmap_ab)
  sum_diffac <- sum(diffmap_ac)

  nHalf <- length(sum_diffab) / 2
  Min <- -8
  Max <- 8
  Thresh <- 0

  pal1 <- colorRampPalette(colors = c("blue", "white"))(nHalf)
  pal2 <- colorRampPalette(colors = c("white", "red"))(nHalf)
  pal_diff <- c(pal1, pal2)
  pal_diff[c(nHalf, nHalf + 1)] <- rgb(t(col2rgb("white")), maxColorValue = 256)
  cuts1 <- seq(Min, Thresh, length.out = nHalf + 1)
  cuts2 <- seq(Thresh, Max, length.out = nHalf + 1)[-1]
  cuts_diff <- c(cuts1, cuts2)

  pdf(paste0(dir.write, "all_sp.pdf"), width = 9, height = 5)
  par(mfrow = c(1, 2))
  plot(sum_diffab,
    breaks = cuts_diff, col = pal_diff,
    legend.only = F, legend.width = 1, legend.shrink = 0.75,
    axis.args = list(
      at = seq(-8, 8, 2),
      labels = seq(-8, 8, 2), cex.axis = 0.8
    ),
    legend.args = list(
      text = paste0("Diferencia "), side = 4, font = 2,
      line = 1.5, cex = 0.8
    ), horizontal = F
  )
  plot(shp.mx, add = TRUE)
  title(main = paste0(name.b, "-", name.a))

  plot(sum_diffac,
    breaks = cuts_diff, col = pal_diff,
    legend = F
  )
  title(main = paste0(name.c, "-", name.a))
  plot(shp.mx, add = TRUE)

  dev.off()
  return(cors_)
}

# Tjoshteim

cor_T <- function(x, y) {
  xv <- values(x)
  yv <- values(y)
  i <- which(is.na(xv) != is.na(yv))
  xv <- xv[-i]
  yv <- yv[-i]
  coordx <- coordinates(x)[-i, ]
  coordy <- coordinates(y)[-i, ]

  dfx <- na.omit(data.frame(cbind(coordx, xv)))
  dfy <- na.omit(data.frame(cbind(coordy, yv)))
  cor_df <- data.frame(cbind(1, 1))
  cor_ <- cor.spatial(
    x = dfx$xv, y = dfy$yv,
    coords = coordinates(dfx[, 1:2])
  )
  cor_df[1, 1] <- cor_[1]
  cor_df[1, 2] <- attributes(cor_)
  colnames(cor_df) <- c("Cor", "Var")
  return(cor_df)
}
