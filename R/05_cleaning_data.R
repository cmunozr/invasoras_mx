# 25 de julio de 2017
# Aim: to clean occurence data of selected species that were
# downloaded
# Objetivo: limpiar los datos de registros de presencia de las
# especies seleccionadas que fueron descargados
# Carlos Munoz

library(rlist)
library(rgeos)
library(ntbox)
library(scrubr)

source("R/05_f1_err.geo.R")
source("R/05_f2_plots.R")
source("R/05_f3_env.outliers.R")


sp_occ <- read.csv("output/03_sp_occ.csv")


# Limpieza clasica "geografica"--------------------------------------

# Cambiar los nombres de las especies y paises de factor a caracter
# esto sucede al escribir y luego volver a leer los diferentes
# datos a partir de un formato .csv

sp_occ[, "sp"] <- as.character(sp_occ[, "sp"])

# Dividir el data.frame de registros de presencias en data.frames individuales
# por especie y guardarla en una lista para que pasar las funciones sea
# mas sencillo

sp_occ_list <- split(x = sp_occ, f = sp_occ$sp, drop = T)

# Coordenadas imposibles

dat_imp <- lapply(sp_occ_list, function(x) (impossible(dat = x, lon = "longitude", lat = "latitude")))

# Puntos con 0,0 en lat y long

dat_zeros <- lapply(dat_imp, function(x) err_zeros(dat = x, lon = "longitude", lat = "latitude"))

# registros imprecisos

dat_impre <- lapply(dat_zeros, function(x) (coord_imprecise(x)))

# Puntos que caen en el mar siendo que son especies terrestres

world <- shapefile("data/geo_shp/world_shp/TM_WORLD_BORDERS-0.3.shp")
dat_err_env <- lapply(dat_impre, function(x) err_env(world, dat = x, lon = "longitude", lat = "latitude"))

# Puntos tienen las mismas coordenadas que el centroide de masa o
# distancia

# Hallando el centro de masa del pologino de cada pais
cent_mass <- gCentroid(spgeom = world, byid = T)
cent_mass_1km <- gBuffer(spgeom = cent_mass, byid = T, width = 0.5 / 111.2)

# Hallando el centro geografico, distancia.
cent_geo <- SpatialPoints(coordinates(world), proj4string = crs(world))
cent_geo_1km <- gBuffer(spgeom = cent_geo, byid = T, width = 0.5 / 111.2)

dat_cent <- lapply(dat_err_env, function(x) err_cent(
    cent_mass = cent_mass_1km,
    cent_geo = cent_geo_1km, dat = x, lon = "longitude", lat = "latitude"
  ))


# Valores atipicos y outliers---------------------------------------------------

# Cargar las capas ambientales bio 1 (temp media anual) y bio 12 (prec
# media anual), asi como el hfp (indice de huella humana)

stfiles_clim <- list.files("data/layers_env/wc2.0_5m_bio/",
  pattern = "*.tif$", full.name = TRUE
)[c(1, 12)]
stfiles_hfp <- "output/04_resampledhfp/hfp_93_5.tif"
st_files <- c(stfiles_clim, stfiles_hfp)

vars <- stack(st_files)

# Extraer el espacio ambiental de cada especie

dat_env <- lapply(dat_cent, function(x) env_space(
    dat = x,
    x = "longitude", y = "latitude", vars = vars
  ))

# Pasar la funcion de outliers ambientales, pasar solo una vez

dat_env_out <- lapply(dat_env, function(x) all_outliers(
    dat = x,
    env = x[, 5:ncol(x)]
  ))


# Sesgos geograficos y ambientales-------------------------------

# Adelgazando la base de datos, puntos unicos para 2.5 min

dat_dup <- lapply(dat_env_out, function(x) clean_dup(
    data = x,
    longitude = "longitude", latitude = "latitude", threshold = 0.08333333
  ))

# (opcional)
# Graficar e imprimir (guardar) el mapa con puntos de ocurrencia

dir.create("figs/05_clean_maps/")
lapply(dat_dup, function(x) plot_save(world,
    dat = x, lon = "longitude",
    lat = "latitude", sp = "sp", save_dir = "figs/05_clean_maps/",
    type = ""
  ))

# escribir csv de los registros de distribución

dir.create("output/05_occ/")
for (i in 1:length(dat_dup)) {
  write.csv(dat_dup[[i]], paste0("output/05_occ/", dat_dup[[i]][1, 1], ".csv"), row.names = F)
}


# Tabla de resumen limpieza -------------------------------------------


registros <- lapply(sp_occ_list, function(x) nrow(x)) %>% list.rbind()
imp <- lapply(dat_imp, function(x) nrow(x)) %>% list.rbind()
zeros <- lapply(dat_zeros, function(x) nrow(x)) %>% list.rbind()
impre <- lapply(dat_impre, function(x) nrow(x)) %>% list.rbind()
mar <- lapply(dat_err_env, function(x) nrow(x)) %>% list.rbind()
cent <- lapply(dat_cent, function(x) nrow(x)) %>% list.rbind()
out_env <- lapply(dat_env_out, function(x) nrow(x)) %>% list.rbind()
unic <- lapply(dat_dup, function(x) nrow(x)) %>% list.rbind()

inf_clean <- data.frame(cbind(
  registros, imp, zeros, impre, mar, cent,
  out_env, unic
))
names(inf_clean) <- c(
  "Registros", "Improbables", "Ceros", "Imprecisos", "Oceano",
  "Centroide", "Atipicos", "Unicos"
)

write.csv(inf_clean, "output/05_data_clean.csv", row.names = T)

rm(list = ls())
gc()
