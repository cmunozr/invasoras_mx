library(raster)
library(ggplot2)

# Example of calibration and proyection models

cal_raster <- raster("output/09_mod_clim/Albizia lebbeck_600km/Albizia lebbeck_600km_H_1_cal.tif")
windows()
plot(cal_raster)
world_shp <- shapefile("data/geo_shp/world_shp/TM_WORLD_BORDERS-0.3.shp")
plot(world_shp, add=T)
windows()
proy_raster <- raster("output/09_mod_clim/Albizia lebbeck_600km/Albizia lebbeck_600km_H_1_proy.tif")
plot(proy_raster)
mex_shp <- shapefile("data/geo_shp/mex_4km_shp/conto4mgw.png")
plot(mex_shp, add=T)


# Empirical Density function of suitabilities with threshold of 
# ten percentile
idon <- raster("output/09_mod_clim/Albizia lebbeck_600km/Albizia lebbeck_600km_H_1_cal.tif")
occ <- read.csv("output/08_datasplit/Albizia lebbeck_600km/Albizia lebbeck_train.csv")[,2:3]
extract_ <- sort(extract(idon, occ))
cuter <- max(extract_[1:ceiling(length(extract_)*0.1)])

dat <- with(density(extract_), data.frame(x, y))
windows()
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_line()+
  geom_area(mapping = aes(x = ifelse(x>0 & x< cuter, x, 0)), fill = "red") +
  xlim(0, 1) + ylim(0, 2.5) + xlab("Idoneidad") + ylab("Density")

# occurences over map of suitability

windows()
plot(idon)
points(occ, pch= 19, cex = 0.5)
world_shp <- shapefile("data/geo_shp/world_shp/TM_WORLD_BORDERS-0.3.shp")
plot(world_shp, add=T)

# differences between maps

cli <- raster("output/09_mod_clim/Albizia lebbeck_600km/Albizia lebbeck_600km_H_1_proy.tif")
proj4string(cli)
clihum <- raster("output/09_mod_climhum/Albizia lebbeck_600km/Albizia lebbeck_600km_H_1_proy.tif")
windows()
par(mfrow = c(1, 2))
plot(clihum)
plot(cli)

library(spatialEco)
cli <- as(cli, 'SpatialGridDataFrame')
clihum <- as(clihum, 'SpatialGridDataFrame')
a <- raster.modifed.ttest(x = cli, y = clihum)
plot(cli)
