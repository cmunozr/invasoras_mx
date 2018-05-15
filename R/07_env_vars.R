# 12 de diciembre de 2017
# Aim: loading bioclimatical
# variables from worldclim and apply a # ordination method (pca) to
# reduce dimensionality, correlation and improve model prediction,
# the pca will be aplicated in precipitation and temperature data
# individually
# Objetivo: cargar
# variables bioclimaticas de worldclim y aplicar
# un metodo de ordenacion para reducir la dimensionalidad, correlaci?f³n
# y mejorar la prediccion del modelo, el pca sera realizado
# en los datos de precipitacion y temperatura individualmente

library(dismo)
library(raster)
library(rgeos)
library(vegan)

source("R/07_f1_envM.R")

# Cargar los directorios de las variables bioclimaticas (worldclim 2)-----

list_bios <- list.files("data/layers_env/wc2.0_5m_bio/",
  pattern = "*.tif$", full.names = T
)

# Metodo de ordenacion por M---------------------------------------------

# Se separan las variables de temperatura y precipitacion,
# ademas no se usaran variables que combinen temperatura y precipitaci?f³n
# simultaneamente tiempo para no confundir el analisis
# tem bios 1:7 y 10:11
# prec bios 12:17
# prec y tem simultaneamente 8:9, 17:19

nom_tem <- list_bios[c(1:7, 10:11)]
nom_prec <- list_bios[12:17]

# Cargar las M o areas accesibles

dir_shp_vector <- list.files("output/06_M/",
  pattern = "*.shp$",
  full.names = T
)
dir.create("output/07_env_vars")

# generar un pca para las variables ambientales en cada area accesible
# o M, se imprimen los componentes con eigenvalue mayores a 1 
# (allcomp = F)

lapply(dir_shp_vector, function(x) env.M(
    dir_shp = x,
    dir_Prec = nom_prec, dir_Tem = nom_tem,
    dir_hfp = "output/04_resampledhfp/hfp_93_5.tif",
    dir_save = "output/07_env_vars/",
    dir_erase = "data/geo_shp/mex_4km_shp/conto4mgw.shp",
    erase = T, allcomp = F
  ))

# generar un pca para las variables ambientales en Mexico para poder
# proyectar, se imprimen todos los componentes (allcomp = T) y no se
# borra ninguna area (erase=F)

env.M(
  dir_shp = "data/geo_shp/mex_4km_shp/conto4mgw.png",
  dir_Prec = nom_prec, dir_Tem = nom_tem,
  dir_hfp = "output/04_resampledhfp/hfp_93_5.tif",
  dir_save = "output/07_mex",
  erase = F, allcomp = T
)

rm(list = ls())
gc()
