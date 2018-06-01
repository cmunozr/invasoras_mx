library(raster)
library(rgeos)

dir.create("output/02_terrestrial_anp", showWarnings = F)

shp.anpall <- shapefile("data/geo_shp/ANP_shp/181ANP_Geo_ITRF08_Enero_2017.shp")
shp.mx <- shapefile("data/geo_shp/mex_4km_shp/conto4mgw.shp")
projection(shp.anpall) <- projection(shp.mx)

anp <- gIntersection(shp.anpall, shp.mx)

pdf(file = "figs/02_terrestrial_anp.pdf")
plot(shp.mx)
plot(shp.anpall, col = rgb(1,0,0,0.5), add = T)
plot(anp, col = "blue")
dev.off()
shapefile(anp, "output/02_terrestrial_anp/terrestrial_anp.shp")
