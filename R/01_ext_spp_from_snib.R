# 21 de febrero de 2017
# Extracting data from Mexican's SNIB dataset
# Extrayendo datos del SNIB y describiendolos
# Aim: to identify the introduced species inside Areas Naturales 
# Protegidas de Mexico
# Objetivo: identificar las especies introducidas dentro de las Areas
# Naturales Protegidas de Mexico
#
# El Sistema Nacional de Información sobre la Biodiversidad Mexicana _
# SNIB_ reune los datos presencia de especies en el pais, recolectados
# en proyectos financiados o no por el gobierno y en datos de museos y
# herbarios. El profesor Jordan Golubov facilito la base de datos de
# especies consideradas como introducidas en el territorio mexicano.
# Con el objetivo de escoger las especies para la investigación, se
# realizó un __cruce geografico__ entre los poligonos de las _Areas
# Naturales Protegidas_ con los puntos de presencia de _Especies
# Exoticas_ con el uso del paquete `raster` y `sp`.
# Los datos originales del SNIB se denominan _Database1281.csv
# en formato ".csv"

library(raster)

anp <- shapefile("data/geo_shp/ANP_shp/181ANP_Geo_ITRF08_Enero_2017.shp")
registros <- read.csv("data/Database1281.csv")

# La base de datos original tiene una gran cantidad de datos que
# no son relevantes, mantener solo datos de taxonomia, coordenadas y estatus

nombres <- names(registros)
registros <- registros[, c(7:13, 34:35, 37)]

# El genero y la especie aparecen en diferentes celdas por lo que es
# necesario unir los caracteres guardados en estas celdas

sp <- paste(registros$Genero, registros$Especie)
registros$sp <- sp

# Inicio del solapamiento (overlay) del vector geografico de areas naturales
# protegidas con los registros de distribución de las especies
# introducidas

occ <- registros[, c("Longitud", "Latitud")]
spatial_occ <- SpatialPointsDataFrame(coords = occ, data = registros, proj4string = CRS("+proj=longlat +datum=WGS84")) # convertir los puntos a puntos espaciales
crs(anp) <- "+proj=longlat +datum=WGS84" # mismas proyecciones para los objetos
over_reg_anp <- over(spatial_occ, anp)
names(over_reg_anp)
over_reg_anp <- cbind(registros, over_reg_anp)

# Como se esperaba, muchos datos quedan por fuera de los poligonos
# de las ANP, por lo que se pueden eliminar las filas que contengan Na

over_reg_anp <- na.omit(over_reg_anp)


# Dado que las especies catalogadas como introducidas pueden estar
# tambien en UMAS, es necesario retirarlas en caso de que esten presentes
# en la base de datosverificar que no existan introducidos en umas

which(over_reg_anp$Estatus == "Introducida en UMAS") 

write.csv(over_reg_anp, "output/01_over_reg_anp.csv", row.names = FALSE)

over_reg_anp <- over_reg_anp[over_reg_anp$Reino=="Plantae", ]

# Descripción de los datos encontrados de spp introducidas en ANP
# reinos presentes y numero de especies
spp_reino <- data.frame(table(over_reg_anp$Reino, over_reg_anp$sp))
spp_reino[spp_reino == "0"] <- NA
spp_reino <- na.omit(spp_reino)
spp_reino <- data.frame(table(spp_reino$Var1))
colnames(spp_reino)[1] <- "reino"
colnames(spp_reino)[2] <- "num_sp"

# clases por reino
x_reino_clase <- data.frame(table(over_reg_anp$Reino, over_reg_anp$Clase))
x_reino_clase[x_reino_clase == "0"] <- NA
x_reino_clase <- na.omit(x_reino_clase)
x_reino_clase <- data.frame(table(x_reino_clase$Var1))
colnames(x_reino_clase)[1] <- "reino"
colnames(x_reino_clase)[2] <- "num_clase"

# clases presentes
x_clases <- data.frame(table(over_reg_anp$Clase, over_reg_anp$sp))
x_clases[x_clases == "0"] <- NA
x_clases <- na.omit(x_clases)
x_clases <- data.frame(table(x_clases$Var1))
colnames(x_clases)[1] <- "clase"
colnames(x_clases)[2] <- "num_sp"


# familias por clase
x_clase_fam <- data.frame(table(over_reg_anp$Clase, over_reg_anp$Familia))
x_clase_fam [x_clase_fam == "0"] <- NA
x_clase_fam <- na.omit(x_clase_fam)
x_clase_fam <- data.frame(table(x_clase_fam$Var1))
colnames(x_clase_fam)[1] <- "clase"
colnames(x_clase_fam)[2] <- "num_fam"

# numero de registros por sp
regx_sp <- data.frame(table(over_reg_anp$sp))
regx_sp [regx_sp == "0"] <- NA
regx_sp[regx_sp == ""] <- NA
regx_sp <- na.omit(regx_sp)

# numero de anps en los que esta presente cada sp
x_sp_anp <- data.frame(table(over_reg_anp$sp, over_reg_anp$NOMBRE))
x_sp_anp[x_sp_anp == "0"] <- NA
x_sp_anp[x_sp_anp == ""] <- NA
x_sp_anp <- na.omit(x_sp_anp)
x_sp_anp <- data.frame(table(x_sp_anp$Var1))
colnames(x_sp_anp)[1] <- "sp"
colnames(x_sp_anp)[2] <- "num_anp"
x_sp_anp[x_sp_anp == "0"] <- NA
x_sp_anp[x_sp_anp == ""] <- NA
x_sp_anp <- na.omit(x_sp_anp)

# numero de sp por anp
x_anp_sp <- data.frame(table(over_reg_anp$NOMBRE, over_reg_anp$sp))
x_anp_sp [x_anp_sp == "0"] <- NA
x_anp_sp [x_anp_sp == ""] <- NA
x_anp_sp <- na.omit(x_anp_sp)
x_anp_sp <- data.frame(table(x_anp_sp$Var1))
colnames(x_anp_sp)[1] <- "anp"
colnames(x_anp_sp)[2] <- "num_sp"
x_anp_sp[x_anp_sp == "0"] <- NA
x_anp_sp[x_anp_sp == ""] <- NA
x_anp_sp <- na.omit(x_anp_sp)
