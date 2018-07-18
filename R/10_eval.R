library(raster)
library(sqldf)
library(data.table)
library(doSNOW)
library(foreach)
library(dismo)
library(scatterplot3d)

source("R/10_f1_pROC.R")
source("R/10_f2_evalbin.R")
source("R/10_f3_evalALL.R")
source("R/10_f4_compile.R")
source("R/10_f5_sel.R")
source("R/10_f6_domess.R")
source("R/10_f7_selclim_plushum.R")


# enlistar las carpetas donde se encuentran los modelos
spmo_list <- list.dirs("output/09_mod_climhum",
  full.names = T,
  recursive = F
)
# enlistar las capertas donde se encuentran los puntos de ocurrencia
occ_list <- list.dirs("output/08_datasplit",
  full.names = T,
  recursive = F
)

# vector de especies que actua como index para varios loop de las
# funciones con el fin de automatizar el proceso
spp <- as.character(read.csv("output/02_sel_spp_inv.csv")[, "sp"])

# crear una carpeta para la evaluaci?n de los modelos climaticos
# y climaticos-humanos
dir.create("output/10_eval_clim")
dir.create("output/10_eval_climhum")

# 
cl <- makeCluster(7)
registerDoSNOW(cl)

foreach(i = 119:153, .packages = c("raster", "sqldf", "data.table")) %dopar% {
  evaluation(
    spmo_path = spmo_list[i], occ_path = occ_list[i],
    path.write = "output/10_eval_climhum/", write = T,
    type = "cal", pattern.models = "cal.tif",
    pattern.occ = "train.csv", pattern.thresholds = "other.csv"
  )
}

stopCluster(cl)

# compilar todas las evaluaciones de cada modelo de cada especie
dir.create("output/10_eval_all")
spp <- as.character(read.csv("output/02_sel_spp_inv.csv")[, "sp"])

for (i in 1:length(spp)) {
  compile(
    sp = spp[i],
    path.enmeval = "output/09_mod_climhum/",
    path.proc.bin = "output/10_eval_climhum/",
    pattern.enmeval = "_ENMeval.csv",
    pattern.cal = "cal_eval.csv",
    pattern.proy = "kmeval.csv",
    path.write = "output/10_eval_all/", write = T,
    type = "climhum"
  )
}

# elegir el mejor modelo, a criterio del investigador
#             cal     proy
# rocparcial  NULL     0.3
# AICc        0.3     NULL   
# OR_         NULL     0.4

selection_ <- rep(list(1), length(spp))
for (i in 1:length(spp)) {
  selection_[[i]] <- selection(
    sp = spp[i],
    path.eval = "output/10_eval_all/",
    pattern.type = "_climhum.csv"
  )
}
selection_ <- rbindlist(selection_)
selection_ <- na.omit(selection_)
write.csv(selection_, "output/10_modsel_climhum.csv", row.names = F)

#Buscar el mejor climatico que se le agrega la hfp

search_ <- rep(list(1), length(spp))
for (i in 1:length(spp)) {
  search_[[i]] <- search_clim_plushum(
    sp = spp[i],
    clim = "output/10_modsel_clim.csv",
    eval = "output/10_eval_all/",
    pattern.mix = "_climhum"
  )
}
search_ <- rbindlist(search_, fill = T)
write.csv(search_, "output/10_modsel_clim_plushum.csv", row.names = F)


# hacer analisis mess para todos los modelos
folder_occ <- list.dirs("output/08_datasplit/",
  full.names = F,
  recursive = F
)

for (i in 1:length(folder_occ)) {
  domess(
    folder_occ = folder_occ[i],
    pattern_train = "_train.csv",
    dir_envcal = "output/07_env_vars/",
    dir_envproy = "output/07_mexconto4mgw/",
    dir_occ = "output/08_datasplit/",
    pattern_prec = "_prec_est.tif$",
    pattern_tem = "_tem_est.tif$",
    long_lat = c("longitude", "latitude"),
    dir_write = "output/10_eval_climhum/",
    onlyclim = F, pattern_factor = "human.tif$",
    type = "climhum"
  )
}

