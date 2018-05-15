# 09 de abril de 2017, Carlos Mu?f±oz Rodriguez
# impossible, funcion creada para excluir datos con longitud mayor a
# 180 de longitud o 90 de latitud.
# err_zeros, excluye datos con long 0 y lat 0
# err_env, funcion que excluye datos que caen en un ambiente que
# no es de la especie, usando los bordes de los paises, no sirve
# para marinas a menos que se modifique la funcion
# fue alterada la funcion pointsworld del paquete biogeo

impossible <- function(dat, lon, lat) {
  cn <- names(dat) # names of columns
  xn <- (dat[, lon])
  yn <- (dat[, lat])

  x1 <- (abs(xn) > 180) * 1
  y1 <- (abs(yn) > 90) * 1

  if (any(x1 + y1 > 0)) {
    ff <- which(x1 + y1 > 0)
    imp <- (dat$ID[ff])
    dat1 <- dat[!(dat$ID %in% imp), ]
  }
  else {
    dat1 <- dat
  }
  return(dat1)
}

err_zeros <- function(dat, lon, lat) {
  nr <- nrow(dat) # number of rows in dat
  cn <- names(dat) # names of columns
  xn <- (dat[, lon])
  yn <- (dat[, lat])
  xy <- cbind(xn, yn) # combine

  x1 <- (abs(xn))
  y1 <- (abs(yn))

  if (any(x1 + y1 == 0)) {
    ff <- which(x1 + y1 == 0)
    nozeros <- (dat[-ff, ])
    dat1 <- nozeros
  }
  else {
    dat1 <- dat
  }
  return(dat1)
}

err_env <- function(world, dat, lon, lat) {
  nr <- nrow(dat) # number of rows in dat
  cn <- names(dat) # names of columns
  xn <- (dat[, lon])
  yn <- (dat[, lat])
  xy <- cbind(xn, yn) # combine

  g <- sp::SpatialPoints(xy, proj4string = crs(world)) # convert points into SpatialPoints format
  s1 <- sp::over(g, world, returnList = FALSE) # overlay the points on the world map
  country_ext <- s1$NAME # country names
  f <- which(is.na(country_ext)) # which points are in the sea (points that do not have a country name)
  f2 <- which(!is.na(country_ext))
  dat1 <- dat[f2, ]
  return(dat1)
}

err_cent <- function(cent_mass, cent_geo, dat, lon, lat) {
  nr <- nrow(dat) # number of rows in dat
  cn <- names(dat) # names of columns
  xn <- (dat[, lon])
  yn <- (dat[, lat])
  xy <- cbind(xn, yn) # combine

  g <- sp::SpatialPoints(xy, proj4string = crs(cent_mass)) # convert points into SpatialPoints format
  g_mass <- sp::over(g, cent_mass, returnList = F) # overlay the points on the centroid map
  f1 <- which(is.na(g_mass)) # which points are outside the centroid (points that have Na)
  dat1 <- dat[f1, ]
  g_geo <- sp::over(g, cent_geo, returnList = F) # overlay the points on the centroid map
  f2 <- which(is.na(g_geo)) # which points are outside the centroid (points that have Na)
  dat2 <- dat1[f2, ]
  cc <- complete.cases(dat2)
  dat2 <- dat2[cc,]
  return(dat2)
}

# coord_imprecise from scrubr github April 04 2018

coord_imprecise <- function(x, which = "both", lat = NULL, lon = NULL, drop = TRUE) {
  switch(
    which,
    has_dec = {
      incomp <- x[!grepl("[0-9]+\\.[0-9]+", x$longitude) | !grepl("[0-9]+\\.[0-9]+", x$latitude), ]
    },
    no_zeros = {
      incomp <- x[grepl("[0-9]+\\.[0]+$", x$longitude) | grepl("[0-9]+\\.[0]+$", x$latitude), ]
    },
    both = {
      incomp1 <- x[!grepl("[0-9]+\\.[0-9]+", x$longitude) | !grepl("[0-9]+\\.[0-9]+", x$latitude), ]
      incomp2 <- x[grepl("[0-9]+\\.[0]+$", x$longitude) | grepl("[0-9]+\\.[0]+$", x$latitude), ]
      incomp <- rbind(incomp1, incomp2)
      incomp <- incomp[!duplicated(incomp), ]
    }
  )
  
  if (NROW(incomp) == 0) incomp <- NA
  if (drop) {
    switch(
      which,
      has_dec = {
        x <- x[grepl("[0-9]+\\.[0-9]+", x$longitude), ]
        x <- x[grepl("[0-9]+\\.[0-9]+", x$latitude), ]
      },
      no_zeros = {
        x <- x[!grepl("[0-9]+\\.[0]+$", x$longitude), ]
        x <- x[!grepl("[0-9]+\\.[0]+$", x$latitude), ]
      },
      both = {
        x <- x[grepl("[0-9]+\\.[0-9]+", x$longitude), ]
        x <- x[grepl("[0-9]+\\.[0-9]+", x$latitude), ]
        x <- x[!grepl("[0-9]+\\.[0]+$", x$longitude), ]
        x <- x[!grepl("[0-9]+\\.[0]+$", x$latitude), ]
      }
    )
  }
  row.names(incomp) <- NULL
  row.names(x) <- NULL
  return(x)
}