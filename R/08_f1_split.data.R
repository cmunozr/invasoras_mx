# 18 de mayo de 2017
# Aim: to split the basedata in subsets test and bins train of each
# species
# Objetivo: dividir las bases de datos en los conjuntos de datos de
# test y grupos de entrenamiento de cada especie
# Por Carlos Munoz

# @dir_dat = path of data cleaned in csv format
# @dir_envM = character, path in where are founded the environmental
# variables inside the accesible area without correlation
# @save_dir = character, the directory where the output will be written
# @country = character, string of area to project
# @factor = integer. Aggregation factor expressed as number of cells in
# each direction (horizontally and vertically). This factor is to
# divide into test_2 and train data
# data.ses = option to write biased train data to construct a figure of
# biased and unbiased dataset

# Value
# train: data.frame of coordinates to calibrate the model
# train_bg: data.frame of background coordinates to calibrate the model
# test_1: data.frame of coordinates that are falling in the projecting
# area
# test_2: data.frame of coordinates to test the model in calibration
# area
# test_2bg: data.frame of background coordinates to test the model in
# calibration area
# train_ses: data.frame of biased coordinates to make a graphic and
# demostrate the power of env.sample to exclude biased data

split.data <- function(dir_dat, dir_envM, dir_save, country = "Mexico",
                       factor, data.ses = T) {

  sp_dist <- sub(
    pattern = "(.*)\\..*$",
    replacement = "\\1", basename(dir_envM)
  )

  dir.create(paste0(dir_save, sp_dist))
  dir_write <- paste0(dir_save, sp_dist, "/")

  
  occ <- read.csv(dir_dat)
  nomx <- occ[!occ$country == country, ]
  test_1 <- occ[occ$country == country, ]
  test_1$country <- NULL

  coords_nomx <- nomx[2:3]
  env <- list.files(paste0(dir_envM, "/"),
    pattern = "*.tif",
    full.names = T
  )[1:2]
  env <- stack(env)

  train_env <- na.omit(cbind(coords_nomx, extract(env, coords_nomx)))
  
  train_sample <- envSample(
    coord = train_env[, 1:2],
    filters = list(train_env[, names(env[[1]])], train_env[, names(env[[2]])]),
    res = list(
      diff(range(train_env[, names(env[[1]])])) / 250,
      diff(range(train_env[, names(env[[2]])]) / 250)
    ), do.plot = F
  )
  
  
  bg.coords <- randomPoints(env, 10000)
  
  bins <- get.checkerboard1(train_sample, env, bg.coords,
    aggregation.factor = factor
  )
  
  bg_grp <- bins[["bg.grp"]]
  bg_complete <- cbind(data.frame(rep(occ[1, 1], 
    length(bg_grp))), bg.coords, bg_grp)
  names(bg_complete) <- c("sp", "longitude", "latitude", "bin")
  
  train_grp <- bins[["occ.grp"]]
  train_complete <- cbind(data.frame(rep(occ[1, "sp"], 
     nrow(train_sample)),train_sample, train_grp))
  names(train_complete) <- c("sp", "longitude", "latitude", "bin")


  write.csv(train_complete, paste0(
    dir_write, occ[1, "sp"],
    "_train", ".csv"
  ), row.names = F)
  write.csv(test_1, paste0(
    dir_write, occ[1, "sp"],
    "_test", ".csv"
  ),
  row.names = F
  )
  write.csv(bg_complete, paste0(
    dir_write, occ[1, "sp"],
    "_bg", ".csv"
  ), row.names = F)
  
  if (isTRUE(data.ses)) {
    train_ses <- cbind(rep(occ[1, "sp"], nrow(coords_nomx)), coords_nomx)
    names(train_ses) <- c("sp", "longitude", "latitude")
    write.csv(train_ses, paste0(
      dir_write, occ[1, "sp"],
      "_train_ses", ".csv"
    ), row.names = F)
  }

  return("ok")
}