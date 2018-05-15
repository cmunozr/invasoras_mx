library(raster)
library(sqldf)
library(data.table)
library(doSNOW)
library(foreach)
source("R/10_f1_pROC.R")
source("R/10_f2_evalbin.R")
source("R/10_f3_evalALL.R")
source("R/10_f4_compile.R")
source("R/10_f5_select.R")
source("R/10_f6_domess.R")

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

# crear una carpeta para la evaluación de los modelos climaticos
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
    path.enmeval = "output/09_mod_clim/",
    path.proc.bin = "output/10_eval_clim/",
    pattern.enmeval = "_ENMeval.csv",
    pattern.cal = "cal_eval.csv",
    pattern.proy = "kmeval.csv",
    path.write = "output/10_eval_all/", write = T,
    type = "clim"
  )
}

# elegir el mejor modelo, a criterio del investigador
#             cal     proy
# rocparcial  0.3     0.4
# AICc        0.3     NULL   
# OR_         0.2     0.4
# Area_       0.2     0.2

selection_ <- rep(list(1), length(spp))
for (i in 1:length(spp)) {
  selection_[[i]] <- selection(
    sp = spp[i],
    path.eval = "output/10_eval_all/",
    graf = F, pattern.type = "_clim.csv"
  )
}
selection_ <- rbindlist(selection_)
write.csv(selection_, "output/10_modsel_clim.csv", row.names = F)

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
