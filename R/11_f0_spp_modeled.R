spp_modeled <- function(path.a = clim,
                        path.b = climhum,
                        path.c = clim_plushum,
                        path.perf = "output/11_comp_performance/",
                        spp) {
  a <- read.csv(path.a)
  i <- seq(1,length(spp), by = 1)
  for(j in 1:length(spp)) {
    if (length(grep(spp[j], a$ModelFile_proy)) != 0) 
    {i[j] <- (spp[j])}else{i[j] <- NA}
  }
  
  b <- read.csv(path.b)
  i2 <- seq(1,length(spp), by = 1)
  for(j in 1:length(spp)) {
    if (length(grep(spp[j], b$ModelFile_proy)) != 0) 
    {i2[j] <- (spp[j])}else{i2[j] <- NA}
  }
  
  c <- read.csv(path.c)
  i3 <- seq(1,length(spp), by = 1)
  for(j in 1:length(spp)) {
    if (length(grep(spp[j], c$ModelFile_proy)) != 0) 
    {i3[j] <- (spp[j])}else{i3[j] <- NA}
  }
  
  x <- data.frame(spp, !is.na(i), !is.na(i2), i == i2)
  colnames(x) <- c("Especie", "clim", "clim*ant", "comp")
  write.csv(x, paste0(path.perf,"spp_modelling.csv"), row.names = F)
  return(x)
}  
