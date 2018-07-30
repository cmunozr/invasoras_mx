# 14 de diciembre de 2017
# Aim: to divide occurences in test (Mexico), train and background
# data sets. Train data is divide in two bins with function 
# get.checkerboard1() from ENMeval package to use in ENMevaluate function.
# The function writes the data directly at hard disk in save_dir path 
# Objetivo: dividir las presencias en los conjuntos de datos de test, 
# train y background. El conjunto de train es dividido en dos grupos
# con la funcion get.checkerboard1() del paquete ENMeval para 
# ser usados en la funcion ENMevaluate. La funcion escribe los datos 
# directamente al disco duro en el directorio save_dir

library(rlist)
library(ENMeval)
library(rowr)
library(data.table)

source("R/08_f1_split.data.R")
source("R/08_f2_envSample.R")

# dividiendo los datos de presencia------------------------------

# directorio de los registros limpios

dir_regclean <- list.files("output/05_occ/",
  pattern = "*.csv",
  full.names = T
)

# directorio donde se encuentran las variables ambientales por especie
# transformadas en componentes

dir_envM <- list.dirs("output/07_env_vars", recursive = F) 

# Generar una tabla de datos con los directorios de datos de presencia
# completos y de capas ambientales transformadas con el PCA
# para poder automatizar la funci?n split.data en todas las especies
# y areas

rep_dirreg <- rep(x = dir_regclean, each = 7)
data_table <- cbind(rep_dirreg, dir_envM)

# crear un directorio de salida general
dir.create("output/08_datasplit")

mapply(function(a, b) split.data(
    dir_dat = a, dir_envM = b, dir_save = "output/08_datasplit/",
    country = "Mexico", factor = 10, data.ses = T
  ), a = data_table[ , 1], b = data_table[ , 2])

rm(list=ls())
gc()

## resumen

spp <- read.csv("output/02_sel_spp_inv.csv")
spp <- sort(as.character(spp$sp))

dirs <- list.dirs("output/08_datasplit/", full.names = T, recursive = F)
test_1 <- unlist(lapply(dirs, function(x) list.files(path = x, pattern = "test", full.names = T, recursive = F)))

ndata_test <- rep(1, length(spp))
for(i in 1:length(spp)){
data <- test_1[grep(spp[i], test_1)]
data2 <- lapply(data, function(x) read.csv(x))
data3 <- unlist(lapply(data2, function(x) nrow(x)))
data3 <- mean(data3) 
ndata_test[i] <- data3
}

train_1 <- unlist(lapply(dirs, function(x) list.files(path = x, pattern = "train.csv$", full.names = T, recursive = F)))

ndata_train_1 <- rep(1, length(spp))
for(i in 1:length(spp)){
  data <- train_1[grep(spp[i], train_1)]
  data2 <- lapply(data, function(x) read.csv(x))
  data3 <- unlist(lapply(data2, function(x) nrow(x[x[,"bin"] == 1,])))
  data3 <- round(mean(data3), digits = 0) 
  ndata_train_1[i] <- data3
}

ndata_train_2 <- rep(1, length(spp))
for(i in 1:length(spp)){
  data <- train_1[grep(spp[i], train_1)]
  data2 <- lapply(data, function(x) read.csv(x))
  data3 <- unlist(lapply(data2, function(x) nrow(x[x[,"bin"] == 2,])))
  data3 <- round(mean(data3), digits = 0) 
  ndata_train_2[i] <- data3
}

resumen <- data.frame(ndata_test, ndata_train_1, ndata_train_2)
write.csv(resumen, "output/08_summary.csv", row.names = F)
