# similitud
# idon vs freq

comp_idon <- function(path.a = clim,
                      path.b = climhum,
                      path.c = clim_plushum,
                      mod.proy = "ModelFile_proy",
                      mod.cal = "ModelFile_cal",
                      path.fig = "figs/",
                      path.sim = "output/11_comp_idon/",
                      name.a = "clim",
                      name.b = "climhum",
                      name.c = "clim_plushum",
                      path.occ = "output/08_datasplit/",
                      pattern.train = "train.csv",
                      pattern.test = "test.csv",
                      pattern.hfp = "human.tif",
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
  

  dir.create("figs/11_comp_idon", showWarnings = F) 
  dir.write <- "figs/11_comp_idon/"
  
  objects_ <- list(a, b, c)
  objects_results <- rep(list(1), length(objects_))
  objects_results2 <- rep(list(1), length(objects_))
  for (i in 1:length(objects_))
  {
    mod <- objects_[[i]][, c(mod.proy, mod.cal)]
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
    path.train <- list.files(paste0(path.occ, namekm, "/"),
      pattern = pattern.train,
      full.names = T
    )
    path.test <- list.files(paste0(path.occ, namekm, "/"),
      pattern = pattern.test,
      full.names = T
    )
    path.humancal <- list.files(paste0("output/07_env_vars/", namekm,
      "/"), pattern = pattern.hfp,
      full.names = T
    )
    path.humanmx <- "output/07_mexconto4mgw/human.tif"
    
    extract_ <- rep(list(1), length(path.train))
    extract_hfp <- rep(list(1), length(path.train))
    for (j in 1:length(path.train))
    {
      train_df <- read.csv(path.train[j])[, 2:3]
      train_raster <- raster(as.character(mod[, mod.cal][j]))
      train_extract <- extract(train_raster, train_df)
      test_df <- read.csv(path.test[j])[, 2:3]
      test_raster <- raster(as.character(mod[, mod.proy][j]))
      test_extract <- extract(test_raster, test_df)
      hfpmx <- raster(path.humanmx)
      hfpmx_extract <- extract(hfpmx, test_df)
      hfpcal <- raster(path.humancal[j])
      hfpcal_extract <- extract(hfpcal, train_df)
      extract_[[j]] <- c(test_extract, train_extract)
      extract_hfp[[j]] <- c(hfpcal_extract, hfpmx_extract)
    }
    objects_results[[i]] <- extract_
    objects_results2[[i]] <- extract_hfp
  }
  
  summ_1 <- rep(list(1), length(path.train))
  summ_2 <- rep(list(1), length(path.train))
  for (i in 1:length(path.train))
  {
    aex <- objects_results[[1]][[i]]
    bex <- objects_results[[2]][[i]]
    cex <- objects_results[[3]][[i]]
    hfp <- objects_results2[[1]][[i]]
    
    aexdf <- data.frame(aex)
    bexdf <- data.frame(bex)
    cexdf <- data.frame(cex)

    plot1 <- ggplot(data = aexdf, aes(aexdf$aex)) + geom_histogram(binwidth = 0.05, fill = "black", col = "grey")
    plot1 <- plot1 + xlim(0, 1)
    plot1 <- plot1 + xlab("Idoneidad") + ylab("Puntos de presencia")

    plot2 <- ggplot(data = bexdf, aes(bexdf$bex)) + geom_histogram(binwidth = 0.05, fill = "black", col = "grey")
    plot2 <- plot2 + xlim(0, 1)
    plot2 <- plot2 + xlab("Idoneidad") + ylab("Puntos de presencia")

    diff_1b <- rowSums(cbind.fill(bex, aex * -1), na.rm = T)
    group <- data.frame(cbind(diff_1b, cut(aex, breaks = seq(0, 1, by = 0.05))))
    grouped_df1 <- group_by(group, V2)
    summarised_df1 <- summarise_all(grouped_df1, funs(mean = mean, var = var, std.dv = sd, std_error = std.error, n = length))
    summarised_df1$V2 <- summarised_df1$V2 * 0.05
    index <- seq(1, length(path.train), by = 1)[i]
    summarised_df1$numsp_ <- rep(index, nrow(summarised_df1))
    summ_1[[i]] <- data.frame(summarised_df1)
    limits <- aes(ymax = summarised_df1$mean + summarised_df1$std_error, ymin = summarised_df1$mean - summarised_df1$std_error)

    plot4 <- ggplot(summarised_df1, aes(V2, mean)) + xlim(0, 1) + ylim(-0.5, 0.8)
    plot4 <- plot4 + xlab("Idoneidad") + ylab(paste0("Dif (", name.b, " - ", name.a))
    plot4 <- plot4 + geom_bar(stat = "identity", position = position_dodge())
    plot4 <- plot4 + geom_errorbar(limits, width = 0.025, position = position_dodge(width = 0.9))
    plot4 <- plot4 + geom_hline(yintercept = 0)

    plot3 <- ggplot(data = cexdf, aes(cexdf$cex)) + geom_histogram(binwidth = 0.05, fill = "black", col = "grey")
    plot3 <- plot3 + xlim(0, 1)
    plot3 <- plot3 + xlab("Idoneidad") + ylab("Puntos de presencia")

    diff_1c <- rowSums(cbind.fill(
      cex,
      aex * -1
    ), na.rm = T)
    group <- data.frame(cbind(diff_1c, cut(aex, breaks = seq(0, 1, by = 0.05))))
    grouped_df2 <- group_by(group, V2)
    summarised_df2 <- summarise_all(grouped_df2, funs(mean = mean, var = var, std.dv = sd, std_error = std.error, n = length))
    summarised_df2$V2 <- summarised_df2$V2 * 0.05
    index <- seq(1, length(path.train), by = 1)[i]
    summarised_df2$numsp_ <- rep(index, nrow(summarised_df2))
    summ_2[[i]] <- data.frame(summarised_df2)
    limits <- aes(ymax = summarised_df2$mean + summarised_df2$std_error, ymin = summarised_df1$mean - summarised_df1$std_error)

    plot5 <- ggplot(summarised_df2, aes(V2, mean)) + xlim(0, 1) + ylim(-0.5, 0.8)
    plot5 <- plot5 + xlab("Idoneidad") + ylab(paste0("Dif (", name.c, " - ", name.a))
    plot5 <- plot5 + geom_bar(stat = "identity", position = position_dodge())
    plot5 <- plot5 + geom_errorbar(limits, width = 0.025, position = position_dodge(width = 0.9))
    plot5 <- plot5 + geom_hline(yintercept = 0)

    pdf(paste0(dir.write, "11_", as.character(mod$sp[i]), "_comp_idon.pdf"))
    grid.arrange(
      arrangeGrob(plot1, ncol = 1),
      arrangeGrob(plot2, plot4, plot3, plot5, ncol = 2, nrow = 2)
    )
    dev.off()
    df <- cbind.fill(aex, bex, diff_1b, cex, diff_1c, hfp)
    colnames(df) <- c(
      paste0("idon_", name.a),
      paste0("idon_", name.b),
      "idon_diff1",
      paste0("idon_", name.c),
      "idon_diff2", "hfp"
    )
    
    write.csv(df, paste0(path.sim, "idon_comp_", as.character(mod$sp[1]), ".csv"), row.names = F)
    write.csv(summarised_df1, paste0(path.sim, "idon_comp_", as.character(mod$sp[i]), "_", name.b, "_", name.a, ".csv"), row.names = F)
    write.csv(summarised_df2, paste0(path.sim, "idon_comp_", as.character(mod$sp[i]), "_", name.c, "_", name.a, ".csv"), row.names = F)
  }

  summ_1 <- rbindlist(summ_1)
  summ_2 <- rbindlist(summ_2)

  pooled <- function(dat.summary=summ_1, index) {
    f <- which(as.character(dat.summary$V2) == as.character(index))
    summ_extract <- dat.summary[f, ]
    # pooled mean
    denominator <- sum(na.omit(summ_extract$n * summ_extract$mean))
    numerator <- sum(summ_extract$n)
    mean_pooled <- denominator / numerator
    # pooled variance
    denominator <- sum(na.omit((summ_extract$n - 1) * summ_extract$var))
    numerator <- sum(summ_extract$n) - length(summ_extract$numsp_)
    var_pooled <- denominator / numerator
    sd_pooled <- sqrt(var_pooled)
    std_error <- sd_pooled / sqrt(numerator)
    df <- data.frame(cbind(index, mean_pooled, var_pooled, sd_pooled, std_error, numerator))
    colnames(df) <- c("V2", "mean", "var", "std.dv", "std_error", "n")
    return(df)
  }


  sequence <- round(seq(from = 0.05, to = 1.00, by = 0.05), digits = 2)
  summ_1pooled <- lapply(sequence, function(x) pooled(dat.summary = summ_1, index = x))
  summ_1pooled <- rbindlist(summ_1pooled)

  summ_2pooled <- lapply(sequence, function(x) pooled(dat.summary = summ_2, index = x))
  summ_2pooled <- rbindlist(summ_2pooled)

  write.csv(summ_1pooled, paste0(path.sim, name.b, "_", name.a, "all_spp.csv"), row.names = F)
  write.csv(summ_1pooled, paste0(path.sim, name.c, "_", name.a, "all_spp.csv"), row.names = F)

  limits <- aes(ymax = summ_1pooled$mean + summ_1pooled$std_error, ymin = summ_1pooled$mean - summ_1pooled$std_error)
  plot6 <- ggplot(summ_1pooled, aes(V2, mean)) + xlim(0, 1.05) + ylim(-0.5, 0.5)
  plot6 <- plot6 + xlab("Idoneidad") + ylab(paste0("Diferencia (modelo ", name.b, " - ", name.a))
  plot6 <- plot6 + geom_bar(stat = "identity", position = position_dodge())
  plot6 <- plot6 + geom_errorbar(limits, width = 0.025, position = position_dodge(width = 0.9))
  plot6 <- plot6 + geom_hline(yintercept = 0)

  limits <- aes(ymax = summ_2pooled$mean + summ_2pooled$std_error, ymin = summ_2pooled$mean - summ_2pooled$std_error)
  plot7 <- ggplot(summ_2pooled, aes(V2, mean)) + xlim(0, 1.05) + ylim(-0.5, 0.5)
  plot7 <- plot7 + xlab("Idoneidad") + ylab(paste0("Diferencia (modelo ", name.c, " - ", name.a))
  plot7 <- plot7 + geom_bar(stat = "identity", position = position_dodge())
  plot7 <- plot7 + geom_errorbar(limits, width = 0.025, position = position_dodge(width = 0.9))
  plot7 <- plot7 + geom_hline(yintercept = 0)

  pdf(paste0(dir.write, "11_", "allspp", "_comp_idon.pdf"), height = 4)
  grid.arrange(
    arrangeGrob(plot6, plot7, ncol = 2)
  )
  dev.off()
}