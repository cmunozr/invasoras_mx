# 1 de diciembre de 2017
# Aim: to construct M or study area through gbuffer function at several
# distances from occurrences (100, 200 , 400, 500 km)
# Objetivo: construir la M o area de estudio a traves de la función
# gbuffer a diversas distacias de los registros de presencia

library(rgeos)
library(raster)

source("R/06_f1_Mextent.R")

reg_path <- list.files("output/05_occ/",
  pattern = "*.csv$",
  full.names = T
)
reg_list <- lapply(reg_path, function(x) read.csv(x))

dir.create("output/06_M")

lapply(reg_list, function(x) M_extent(
    dat = x, country = "Mexico",
    dif_area = T, area = c(50, seq(100, 600, by = 100)),
    write = T, dir_write = "output/06_M/"
  ))
