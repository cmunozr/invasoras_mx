library(SpatialPack)
library(FSA)
library(raster)
library(rowr)
library(data.table)
library(gridExtra)
library(plotrix) # for std error function
library(dplyr) # for group_by and summarise_each function
library(ggplot2) # for creating ggplot
library(colorRamps)

source("R/11_f0_spp_modeled.R")
source("R/11_f1_compmet.R")
source("R/11_f2_compidon.R")
source("R/11_f3_compmaps.R")
source("R/11_f4_heatmap.R")

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

spp <- as.character(read.csv("output/02_sel_spp_inv.csv")[, "sp"])

comp <- spp_modeled(spp=spp, path.a = clim, path.b = climhum, path.c = clim_plushum)

list_ <- lapply(col_compare, function(x) metric_comp(compare = x,
       path.perf = "output/11_comp_performance/"))
df_test <- rbindlist(list_)
write.csv(df_test, "output/11_comp_performance/comp_test.csv",
          row.names = F)
dev.off()
comp_idon(path.a = clim, path.b = climhum, path.c = clim_plushum,
          path.sim = "output/11_comp_idon/")
dev.off()
list_2 <- map_comp(path.a = clim, path.b = climhum, path.c = clim_plushum,
         path.maps = "output/11_comp_map/" 
        )
cor_df <- rbindlist(list_2)
write.csv(cor_df, "output/11_comp_map/allsp_comp_map.csv",
          row.names = F)
dev.off()
heatmap(path.a = clim, path.b = climhum, path.c = clim_plushum,
        path.heat = "output/11_heatmap/")
dev.off()
