# compare of maps

map_comp <- function(path.a = clim,
                     path.b = climhum,
                     path.c = clim_plushum,
                     mod.proy = "ModelFile_proy",
                     threshold = "TTP_cal",
                     path.fig = "figs/",
                     path.maps = "output/11_mapcompare/",
                     name.a = "clim",
                     name.b = "climhum",
                     name.c = "clim_plushum",
                     path.mx = "data/geo_shp/mex_4km_shp/conto4mgw.shp") {
  a <- read.csv(path.a)
  b <- read.csv(path.b)
  c <- read.csv(path.c)
  shp.mx <- shapefile(path.mx)

  objects_ <- list(a, b, c)
  objects_results <- rep(list(1), length(objects_))
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
    extract_ <- rep(list(1), nrow(mod))
    for (j in 1:nrow(mod))
    {
      extract_[[j]] <- raster(as.character(mod[, mod.proy][j]))
    }
    objects_results[[i]] <- extract_
  }

  maps_1 <- rep(list(1), nrow(mod))
  maps_2 <- rep(list(1), nrow(mod))

  for (i in 1:nrow(mod))
  {
    a_map <- objects_results[[1]][[1]]
    b_map <- objects_results[[2]][[1]]
    c_map <- objects_results[[3]][[1]]

    diffmap_1 <- b_map - a_map
    diffmap_2 <- c_map - a_map

    cuts_idon <- seq(0, 1, by = 0.1)
    cuts_diff <- seq(-1, 1, by = 0.1)
    pal_idon <- colorRampPalette(c("white", "forestgreen"))
    pal_diff <- colorRampPalette(c("blue", "white", "red"))

    pdf(paste0(path.fig, "11_mapcomp_", sp[i], ".pdf"))
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
    plot(diffmap_1,
      breaks = cuts_diff, col = pal_diff(20),
      legend = F
    )
    plot(shp.mx, add = TRUE)

    plot(diffmap_2,
      breaks = cuts_diff, col = pal_diff(20),
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
}
