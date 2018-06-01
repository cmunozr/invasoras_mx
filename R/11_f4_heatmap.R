heatmap <- function(path.a = clim,
                     path.b = climhum,
                     path.c = clim_plushum,
                     mod.proy = "ModelFile_proy",
                     threshold = "TTP_cal",
                     path.fig = "figs/",
                     path.heat = "output/11_heatmap/",
                     shp.anp = "data/geo_shp/ANP_shp/181ANP_Geo_ITRF08_Enero_2017.shp",
                     path.mx = "data/geo_shp/mex_4km_shp/conto4mgw.shp",
                     name.a = "clim",
                     name.b = "climhum",
                     name.c = "clim_plushum") {
  a <- read.csv(path.a)
  b <- read.csv(path.b)
  c <- read.csv(path.c)

  conditions <- function(x=a) {
    index <- c(
      which(x[, "pRoc_proy"] < 1.01),
      which(x["p_valor_proy"] > 0.05),
      which(x["TTP_OR_proy"] > 0.10)
    )
    data <- x[-index, ]
  }

  a <- conditions(a)
  b <- conditions(b)
  c <- conditions(c)

  objects_ <- list(a, b, c)
  objects_bin <- rep(list(1), length(objects_))
  objects_cont <- rep(list(1), length(objects_))

  for (i in 1:length(objects_))
  {
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

    bin_ <- stack()
    con_ <- stack()
    for (j in 1:nrow(mod))
    {
      con_sp <- raster(as.character(mod[, mod.proy][j]))
      nc <- paste0(sp[j], "_con")
      names(con_sp) <- nc
      con_ <- stack(con_, con_sp)
      bin_sp <- con_sp > as.numeric(mod[, threshold][j])
      nb <- paste0(sp[j], "_bin")
      names(bin_sp) <- nb
      bin_ <- stack(bin_, bin_sp)
    }
    objects_cont[[i]] <- con_
    objects_bin[[i]] <- bin_
  }

  for (f in 1:length(objects_bin)) {
    for (i in 1:nlayers(objects_bin[[f]])) {
      type <- c(name.a, name.b, name.c)
      ras <- objects_bin[[f]][[i]]
      writeRaster(ras, paste0(path.heat, names(ras),
                  "_", type[f], ".tif"), overwrite = T)
    }
  }

  anp <- shapefile(shp.anp)
  a_map <- sum(objects_bin[[1]])
  crs(anp) <- crs(a_map)
  writeRaster(a_map, paste0(path.heat,"_summ_", name.a,".tif"), overwrite = T)

  anp <- crop(anp, a_map)
  a_extract <- mask(a_map, anp)
  val_aextract <- na.omit(values(a_extract))

  b_map <- sum(objects_bin[[2]])
  anp <- crop(anp, b_map)
  writeRaster(b_map, paste0(path.heat,"_summ_", name.b,".tif"), overwrite = T)
  b_extract <- mask(b_map, anp)
  val_bextract <- na.omit(values(b_extract))

  c_map <- sum(objects_bin[[3]])
  anp <- crop(anp, c_map)
  writeRaster(c_map, paste0(path.heat,"_summ_", name.c,".tif"), overwrite = T)
  c_extract <- mask(c_map, anp)
  val_cextract <- na.omit(values(c_extract))

  type <- c(
    rep(name.a, length(val_aextract)),
    rep(name.b, length(val_bextract)),
    rep(name.c, length(val_cextract))
  )
  values <- c(val_aextract, val_bextract, val_cextract)
  df <- data.frame(values, type)
  colnames(df) <- c("num.sp", "type")
  write.csv(df, paste0(path.heat, "_ANPdata.csv"), row.names = F)
  
  dir.create(paste0(path.fig, "11_heatmap"), showWarnings = F)
  dir.write <- paste0(path.fig, "11_heatmap", "/")
  
  pdf(paste0(dir.write, "_comparisson_", ".pdf"))
  plot1 <- ggplot(data = df, mapping = aes(type, values))
  plot1 <- plot1 + geom_boxplot(outlier.shape = NA)
  plot1
  dev.off()

  shp.mx <- shapefile(path.mx)
  
  pdf(paste0(dir.write, name.a, "_heatmap.pdf"), width = 6, height = 5)
  plot(a_map)
  plot(shp.mx, add = T)
  title(name.a)
  dev.off()
  
  pdf(paste0(dir.write, name.b, "_heatmap.pdf"), width = 6, height = 5)
  plot(b_map)
  plot(shp.mx, add = T)
  title(name.b)
  dev.off()
  
  pdf(paste0(dir.write, name.c, "_heatmap.pdf"), width = 6, height = 5)
  plot(c_map)
  plot(shp.mx, add = T)
  title(name.c)
  dev.off()
  
  #https://rcompanion.org/rcompanion/d_06.html

  stat_data <- Summarize(num.sp ~ type, data = df)
  write.csv(stat_data, paste0(path.heat, "stat_data.csv"), row.names = F)
  krus1 <- kruskal.test(num.sp ~ type, data = df)
  krus1_df <- data.frame(cbind(krus1$statistic, krus1$parameter, krus1$p.value))
  colnames(krus1_df) <- c("Estadistico", "g.l", "p.value")
  write.csv(krus1_df, paste0(path.heat, "stat_krus.csv"), row.names = F)
  df$type <- factor(df$type, levels = c("clim", "clim_plushum", "climhum"))
  dun_t <- dunnTest(num.sp ~ type, data = df, method = "bh")
  df_dun <- dun_t[[2]]
  write.csv(df_dun, paste0(path.heat, "stat_dun.csv"), row.names = F)
  
}
