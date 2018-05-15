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

dir_envM <- list.dirs("output/07_vars_env_pca", recursive = F) 

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

test_1 <- list.files("cleaned_data/occ_data/",
  pattern = "*test_1.csv",
  full.names = T
)
test_1 <- lapply(test_1, function(x) read.csv(x))

train <- list.files("cleaned_data/occ_data/",
  pattern = "*occ_train.csv",
  full.names = T
)
train <- lapply(train, function(x) read.csv(x))



mex <- lapply(reg_clean, function(x) nrow(x[x$country == "Mexico", ]))
mex <- list.rbind(mex)
no_mex <- lapply(reg_clean, function(x) nrow(x[!x$country == "Mexico", ]))
no_mex <- list.rbind(no_mex)
test_2a <- lapply(test_2, function(x) nrow(x)) %>% list.rbind()
traina <- lapply(train, function(x) nrow(x)) %>% list.rbind()