# 12 de febrero de 2018
# modelling with ENMeval

library(ENMeval)

source("R/09_f1_modelenmeval.R")
source("R/09_f2_otherresults.R")

dir.create("output/09_mod_climhum")

folder_occ <- list.dirs("output/08_datasplit/",
  full.names = F,
  recursive = F
)

a <- 1:175

for(i in a)
{
  model.ENMeval(folder_occ = folder_occ[i],
  pattern_train = "_train.csv", pattern_bg = "_bg.csv",
  dir_envcal = "output/07_env_vars/", 
  dir_envproy = "output/07_mexconto4mgw/",
  dir_occ = "output/08_datasplit/", 
  pattern_prec = "_prec_est.tif$",
  pattern_tem = "_tem_est.tif$",
  long_lat = c("longitude", "latitude"),
  rm = seq(1, 5, by = 1), 
  dir_write = "output/09_mod_climhum/", 
  onlyclim = F, pattern_factor = "human.tif$"   
  )
}






  