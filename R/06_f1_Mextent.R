M_extent <- function(dat, country, dif_area, area, write = T,
                     dir_write = NULL) {
  occ <- dat
  kmtograde <- area / 111.2
  nocountry <- occ[!occ$country == country, ]
  if (isTRUE(dif_area)) {
    coords <- SpatialPoints(nocountry[, c("longitude", "latitude")])
    inf_geo <- lapply(kmtograde, function(x) {
      a <- gBuffer(spgeom = coords, byid = F, width = x)
      crs(a) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      return(a)
    })
  }
  else {
    coords <- SpatialPoints(nocountry[, c("longitude", "latitude")])
    inf_geo <- gBuffer(spgeom = coords, byid = F, width = kmtograde)
    crs(inf_geo) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  sp <- dat[1, 1]
  names(inf_geo) <- paste0(sp, "_", area, "km")

  if (isTRUE(write)) {
    for (i in 1:length(inf_geo))
    {
      shapefile(inf_geo[[i]], paste0(
        filename = dir_write,
        names(inf_geo[i])
      ), overwrite = T)
    }
    return("ok")
    rm(list = ls())
    gc()
  }
  else {
    return(inf_geo)
    rm(list = ls())
    gc()
  }
}
