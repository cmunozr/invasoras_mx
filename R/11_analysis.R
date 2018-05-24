library(raster)
library(rowr)
library(data.table)
library(gridExtra)
library(plotrix) # for std error function
library(dplyr) # for group_by and summarise_each function
library(ggplot2) # for creating ggplot
library(colorRamps)

source("R/11_f1_compmet.R")
source("R/11_f2_compidon.R")
source("R/11_f3_compmaps.R")

# mejores modelos climaticos
clim <- "output/10_modsel_clim.csv"
# mejores modelos climaticos y humano
climhum <- "output/10_modsel_climhum.csv"
# mejores modelos climaticos + hfp2
clim_plushum <- "output/10_modsel_clim_plushum.csv"
col_compare <- c(
  "AICc", "nparam", "pRoc_proy", "TTP_area_proy",
  "TTP_OR_proy", "TTP_proy", "pROC_mean_cal",
  "TTP_area_cal", "TTP_OR_cal", "TTP_cal"
)
# b - a
# c - a

dir.create("output/11_comp_performance")
dir.create("output/11_comp_idon")
dir.create("output/11_comp_map")
dir.create("output/11_heatmap")

lapply(col_compare, function(x) metric_comp(compare = x,
       path.perf = "output/11_comp_performance/"))
idon_comp(path.a = clim, path.b = climhum, path.c = clim_plushum,
          path.sim = "output/11_comp_idon")
map_comp(path.a = clim, path.b = climhum, path.c = clim_plushum)
