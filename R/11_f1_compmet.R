# Metric comparation
# The performance metrics to compare depends on column names of the csv, is
# for that reason appears arguments name.a ...

metric_comp <- function(path.a = clim,
                        path.b = climhum,
                        path.c = clim_plushum,
                        spp_model = comp,
                        compare,
                        path.fig = "figs/",
                        path.perf = "output/11_comp_performance/",
                        name.a = "clim",
                        name.b = "climhum",
                        name.c = "clim_plushum",
                        spp) {
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
  
  # performance
  a_extract <- a[, compare]
  b_extract <- b[, compare]
  c_extract <- c[, compare]
  
  diff_1 <- b_extract - a_extract
  diff_2 <- c_extract - a_extract
  
  df <- cbind(b_extract, a_extract, diff_1, c_extract, a_extract, diff_2)
  colnames(df) <- c(
    paste0(compare, "_", name.b),
    paste0(compare, "_", name.a),
    "diff_1",
    paste0(compare, "_", name.c),
    paste0(compare, "_", name.a),
    "diff_2"
  )
  write.csv(df, paste0(path.perf, "comp_met_", compare, ".csv"))
  
  # test
  if(grepl("pRoc", compare)){
    wil_test1 <- wilcox.test(diff_1, alternative = "greater", mu = 0,
                exact = F)
    p1 <- wil_test1$p.value
    sta1 <- wil_test1$statistic
    wil_test2 <- wilcox.test(diff_2, alternative = "greater", mu = 0,
                            exact = F)
    p2 <- wil_test2$p.value
    sta2 <- wil_test2$statistic
  }else{
    wil_test1 <- wilcox.test(diff_1, alternative = "less", mu = 0,
                            exact = F)
    p1 <- wil_test1$p.value
    sta1 <- wil_test1$statistic
    
    wil_test2 <- wilcox.test(diff_2, alternative = "less", mu = 0,
                             exact = F)
    p2 <- wil_test2$p.value
    sta2 <- wil_test2$statistic
  }
  
  # Confidence intervale at 0.95
  
  mdn1 <- median(diff_1)
  i_inf1 <- ceiling((length(diff_1)/2)-((1.96*(sqrt(length(diff_1))))/2))
  i_sup1 <- ceiling(1+((length(diff_1)/2)+((1.96*(sqrt(length(diff_1))))/2)))
  inf1 <- sort(diff_1)[i_inf1]
  sup1 <- sort(diff_1)[i_sup1]
  
  mdn2 <- median(diff_2)
  i_inf2 <- ceiling((length(diff_2)/2)-((1.96*(sqrt(length(diff_2))))/2))
  i_sup2 <- ceiling(1+((length(diff_2)/2)+((1.96*(sqrt(length(diff_2))))/2)))
  inf2 <- sort(diff_2)[i_inf2]
  sup2 <- sort(diff_2)[i_sup2]
  
  wil_test3 <- wilcox.test(x = diff_1, y = diff_2, alternative = "two.sided",
              paired = F, exact = F)
  p3 <- wil_test3$p.value
  sta3 <- wil_test3$statistic
  
  df_test <- data.frame(cbind(compare, sta1, p1, sta2, p2, mdn1,
                        inf1, sup1, mdn2, inf2, sup2, sta3, p3))
  colnames(df_test) <- c("metrica", "sta_1", "p.value1",
                         "sta_2", "p.value2", "Mdn1", "IC_inf1",
                         "IC_sup1", "Mdn2", "IC_inf2", "IC_sup2",
                         "sta_3", "p.value3")
  dir.create("figs/11_comp_met", showWarnings = F) 
  dir.write <- "figs//11_comp_met/"

  pdf(paste0(dir.write, compare, "_comp_met_", ".pdf"))
  par(mfrow = c(3, 2), oma = c(0, 0, 2, 0))
  barplot(a_extract,
    ylab = paste(compare),
    names.arg = seq(1, length(a_extract), by = 1)
  )
  plot.new()
  barplot(b_extract,
    ylab = paste(compare),
    names.arg = seq(1, length(a_extract), by = 1)
  )
  plot(diff_1, type = "l", ylab = paste(expression(Delta), compare),
       xlab = "", main = paste0(name.b, " - ", name.a))
  abline(h = 0)
  barplot(c_extract,
    ylab = compare,
    names.arg = seq(1, length(a_extract), by = 1), xlab = "sp"
  )
  plot(diff_2, type = "l", ylab = paste(expression(Delta), compare),
       xlab = "sp", main = paste0(name.c, " - ", name.a))
  abline(h = 0)
  dev.off()
  return(df_test)
}
