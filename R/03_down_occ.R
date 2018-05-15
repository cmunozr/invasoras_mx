# 21 de marzo de 2017
# Aim: to download species ocurrences of the selected species from
# several repositories
# Objetivo: Descargar registros de presencia de las especies seleccionadas
# desde diferentes repositorios
# Carlos Munoz
#
# Despues de tomar la decisi?f³n sobre las especies a usar, comence
# a descargar los datos de presencia desde GBIF, estos datos idealmente
# solo de PRESERVED_SPECIMENS, para evitar el riesgo que surge en
# las observaciones en campo al intentar describir una especie,
# sobre todo, sabiendo que los pastos se pueden confundir muy
# facilmente. Este proceso se hace dentro de la funcion Get.Species.Data.R

library(spocc)
library(rgbif)
library(rlist)
library(data.table)
library(sp)
library(stringr)
library(plyr)
library(raster)



# Gbif----------------------------------------------------- 1 hora

source("R/03_f1_get.species.obs.R")
source("R/03_f2_get.species.data.R")

# Leer el csv con las especies

sp_bdinv_anp <- read.csv("output/02_sel_spp_inv.csv")

### convertir el data frame de especies a un vector http://stackoverflow.com/questions/2545228/converting-a-dataframe-to-a-vector-by-rows

sp_name <- as.vector(t(sp_bdinv_anp[,"sp"]))
sp_name <- sort(stringr::str_trim(sp_name))

# proceso tomado de https://sites.google.com/site/mikegahan1/rgbif

# registros de especimenes preservados
sp_obs_pre <- lapply(sp_name, function(x) Get.Species.Obs(x, type = c("PRESERVED_SPECIMEN")))
sp_conteo_pre <- rbindlist(sp_obs_pre, fill = T)

# registros de literatura
sp_obs_lit <- lapply(sp_name, function(x) Get.Species.Obs(x, type = c("LITERATURE")))
sp_conteo_lit <- rbindlist(sp_obs_lit, fill = T)

sp_conteo <- sp_conteo_lit
sp_conteo$Count <- sp_conteo_lit$Count + sp_conteo_pre$Count

write.csv(sp_conteo, "output/03_sp_conteo_gbif.csv", row.names = F)

# La descarga maxima de presencias es de 200000 por lo que se debe
# dividir la data frame sp conteo segun la columna count
# no encontre una funci?f³n directa y no se programarlas
# por tanto, los dividi con la funci?f³n cumsum http://stackoverflow.com/questions/18434720/remove-rows-in-a-data-frame-until-the-sum-of-a-column-get-specific-value
# pero antes fue necesario volver numerica la columna count

# Dividiendo por listas 

sp_conteo_pre <- split(sp_conteo_pre, f= sp_conteo$Species, drop = T)

sp_conteo_lit[sp_conteo_lit$Count == 0,] <- NA
sp_conteo_lit <- na.omit(sp_conteo_lit)
sp_conteo_lit <- split(sp_conteo_lit, f= sp_conteo_lit$Species, drop = T)

# Descargar los registros de presencia

sp_occ_listA <- lapply(sp_conteo_pre, function(x) Get.Species.Data(x, type = "PRESERVED_SPECIMEN"))
sp_occ_listB <- lapply(sp_conteo_lit, function(x) Get.Species.Data(x, type = "LITERATURE"))


# Unir las listas 

sp_occ_gbif_list <- c(sp_occ_listA, sp_occ_listB)
sp_occ_gbif_df <- rbindlist(sp_occ_gbif_list, fill = TRUE)

# Eliminar algunas columnas innecesarias

sp_occ_gbif_df <- sp_occ_gbif_df[, -c("locality")]

#  ?Fueron descargados los datos
# de GBIF con los mismos nombres que el SNIB?

sp_name_gbif <- sort(unique(sp_occ_gbif_df$species))
setdiff(sp_name_gbif, sp_name)

# Sinonimos verificado en http://www.theplantlist.org
# Kalanchoe delagoensis = Bryophyllum delagoense
# Kali turgida = Salsola kali

sp_occ_gbif_df[sp_occ_gbif_df == "Kalanchoe delagoensis"] <- "Bryophyllum delagoense"
sp_occ_gbif_df[sp_occ_gbif_df == "Kali turgida"] <- "Salsola kali"

sp_name_gbif <- sort(unique(sp_occ_gbif_df$species))
setequal(sp_name, sp_name_gbif)

write.csv(sp_occ_gbif_df, "output/03_sp_occ_gbif.csv", row.names = F)


# spocc --------------------------------------------------- 6 horas

# La cantidad de datos de organismos preservados es baja en algunas de
# las especies, esto teniendo en cuenta que cuando se limpian los datos
# ambiental y geograficamente se reduce agresivamente la cantidad de
# informacion

sp_occ_spocc <- occ(query = sp_name, from = c(
  "idigbio","ecoengine", "bison", "ala"
  ), limit = 500)
sp_occ_spocc_df <- occ2df(sp_occ_spocc)
sp_occ_spocc_df <- sp_occ_spocc_df[-which(is.na(sp_occ_spocc_df[,"longitude"])),]

# Es necesario agregar una columna de pais a cada registro de presencia
# para poder establecer si se encuentra en Mexico o no y si cae fuera
# de los limites de alg?n poligono de pais
# convertir los puntos a puntos espaciales   

sp_occ_coords <- sp_occ_spocc_df[, c("longitude", "latitude")]
spatial_occ <- SpatialPointsDataFrame(coords = sp_occ_coords,
data = sp_occ_spocc_df, proj4string = CRS("+proj=longlat +datum=WGS84"))

world_shp <- shapefile("data/geo_shp/world_shp/TM_WORLD_BORDERS-0.3.shp")
crs(world_shp) <- "+proj=longlat +datum=WGS84"

over_occ_wrld <- over(spatial_occ, world_shp)
sp_occ_spocc_df$country <- over_occ_wrld$NAME

write.csv(sp_occ_spocc_df, "output/03_sp_occ_spocc.csv", row.names = F)

rm(list=ls())

# snib ------------------------------------------------------

# Extraer todos los datos de las especies elegidas que se almacenan en el
# snib

# Cargar datos del snib

snib_data <- read.csv("data/Database1281.csv")[, c("Genero", "Especie", "Longitud", "Latitud", "Aniocolecta")]
snib_data$sp <- paste(snib_data$Genero, snib_data$Especie)
snib_data <- snib_data[, c("sp", "Longitud", "Latitud")]

sp_bdinv_anp <- read.csv("output/02_sel_spp_inv.csv")
sp <- as.vector(t(sp_bdinv_anp[1]))
sp <- data.frame(sp)

# Cruzar la base de datos del snib y de nombres de las especies

sp_occ_snib <- semi_join(x = snib_data, y = sp, by = "sp")

# Sumarle el nombre del pais al data.frame de registros de presencias
# para poder unir los datos de gbif, spocc y snib

sp_occ_snib$country <- rep("Mexico", nrow(sp_occ_snib))

write.csv(sp_occ_snib, "output/03_sp_occ_snib.csv", row.names = F)

rm(list=ls())

# Merge data --------------------------------------------------------


# Se unen los registros de presencia de cada una de las especies,
# ya que la base de datos sp_occ_spocc descarga de diferentes
# repositorios mostrados en la columna 'prov', para unir estas filas
# se debe usar rbind.fill ya que las otras bases de datos sp_occ_gbif
# y sp_occ_snib les hace falta aquella columna 'prov'

sp_occ_gbif <- read.csv("output/03_sp_occ_gbif.csv")
sp_occ_spocc <- read.csv("output/03_sp_occ_spocc.csv")[,c("name", "longitude", "latitude", "country")]
sp_occ_snib <- read.csv("output/03_sp_occ_snib.csv")

tit <- c("sp", "longitude", "latitude", "country")
names(sp_occ_gbif) <- tit
names(sp_occ_spocc) <- tit
names(sp_occ_snib) <- tit


sp_occ <- rbind.fill(sp_occ_spocc, sp_occ_gbif, sp_occ_snib)
rm(sp_occ_gbif, sp_occ_snib, sp_occ_spocc)
# Verificar el numero de especies

spp_num <- as.character(unique(sp_occ$sp))

# 59 nombres de especies, esto se debe a la sensibilidad de R por 
# caracteres en letra mayuscula y minuscula, por lo que se deben
# estandarizar los nombres: primera letra mayuscula

simpleCap <- function(x) {
  paste(toupper(substring(x, 1, 1)), tolower(substring(x, 2)),
        sep="", collapse=" ")
}

sp_occ$sp <- sapply(as.character(sp_occ$sp), simpleCap)

# Se generan mas especies, 35, porque hay sinonimos la diferencia radica
# entre los nombres usados en el SNIB, GBIF y algunas de las bases de
# datos que se consultaron a traves de SPOCC, el SNIB usa sinonimos
# verificado en http://www.theplantlist.org
# Kalanchoe delagoensis = Bryophyllum delagoense
# Anacyclus australis = Cotula australis
# Mnesithea laevis = Rottboellia cochinchinensis
# Glebionis coronaria = Chrysanthemum coronarium
# Eclipta prostrata = Anthemis cotula

sp_occ[sp_occ == "Kalanchoe delagoensis"] <- "Bryophyllum delagoense"
sp_occ[sp_occ == "Anacyclus australis"] <- "Cotula australis"
sp_occ[sp_occ == "Mnesithea laevis"] <- "Rottboellia cochinchinensis"
sp_occ[sp_occ == "Glebionis coronaria"] <- "Chrysanthemum coronarium"
sp_occ[sp_occ == "Eclipta prostrata"] <- "Anthemis cotula"
sp_occ[sp_occ == "Cenchrus purpureus"] <- "Pennisetum purpureum"
sp_occ[sp_occ == "Cenchrus clandestinus"] <- "Pennisetum purpureum"
sp_occ[sp_occ == "Bromus tectorum var. tectorum"] <- "Bromus tectorum"
sp_occ[sp_occ == "Bromus tectorum var. glabratus"] <- "Bromus tectorum"
sp_occ[sp_occ == "Eragrostis curvula var. curvula"] <- "Eragrostis curvula"
sp_occ[sp_occ == "Eragrostis curvula var. conferta"] <- "Eragrostis curvula"
sp_occ[sp_occ == "Melinis repens subsp. repens"] <- "Melinis repens"
sp_occ[sp_occ == "Poa pratensis subsp. pratensis"] <- "Poa pratensis" 
sp_occ[sp_occ == "Poa pratensis subsp. pratensis"] <- "Poa pratensis" 
sp_occ[sp_occ == "Poa pratensis subsp. agassizensis"] <- "Poa pratensis" 
sp_occ[sp_occ == "Poa pratensis subsp. angustifolia"] <- "Poa pratensis" 
sp_occ[sp_occ == "Salsola kali tenuifolia" ] <- "Salsola kali"
sp_occ[sp_occ == "Salsola kali var. tenuifolia" ] <- "Salsola kali"


# Volver a verificar

length(as.character(unique(sp_occ$sp)))

write.csv(sp_occ, "output/03_sp_occ.csv", row.names = F)
