# 10 de octubre de 2017
# Aim: to resample and reprojecting human footprint index created by 
# Venter et al 2016, from 0.5 arcmin to 5 arcmin resolution and moll 
# project to planar projection
# Objetivo: remuestrear y reproyectar el indice de huella humana creado
# por Venter et al 2016, de una resolucion de 0.5 minutos de arco a 
# 5 minutos de arco, y de una proyeccion de moll a planar.

library(raster)

hfp_0.5 <- raster("data/layers_env/hfp_wrld_93_09/Maps/1994/HFP1993.tif")
clim <- raster("data/layers_env/wc2.0_5m_bio/wc2.0_bio_5m_01.tif")

hfp_extentclim <- projectExtent(hfp_0.5, crs(clim))
hfp_projectclim <- projectRaster(hfp_0.5, hfp_extentclim)

hfp_resclim <- resample(x = hfp_projectclim, y = clim)

compareRaster(hfp_resclim, clim)

writeRaster(hfp_res, "output/04_resampledhfp/hfp_93_5.tif")