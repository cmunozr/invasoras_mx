# 22 de febrero de 2017
# Aim: to select the introduced species that will be used
# Objetivo: seleccionar las especies introducidas que seran usadas
# Seleccion de especies por medio de dos filtros
# Primero usando la lista compilada de la CONABIO y SEMARNAT
# Segundo usa las listas de alta invasividad -100Mex-


library(data.table)
library(dplyr)

reg_anp_cruce <- read.csv("output/01_over_reg_anp.csv")

# Se genera una tabla que resume el numero de especies presentes en las 
# ANP

sp_anp_cruce <- data.frame(table(reg_anp_cruce$Reino,reg_anp_cruce$Clase, reg_anp_cruce$Familia,reg_anp_cruce$sp))
sp_anp_cruce[sp_anp_cruce == "0"] <- NA
sp_anp_cruce <- na.omit(sp_anp_cruce)
colnames(sp_anp_cruce)[1] <- "reino"
colnames(sp_anp_cruce)[2] <- "clase"
colnames(sp_anp_cruce)[3] <- "fam"
colnames(sp_anp_cruce)[4] <- "sp"
colnames(sp_anp_cruce)[5] <- "numreg"
# cantidad de especies 530

unique(sp_anp_cruce$reino)
sp_plantas <- sp_anp_cruce[sp_anp_cruce$reino == "Plantae",]

# Intersectar bases de datos-------------------------------------

# Leer csv de las bases de datos de invasoras (CONABIO,SEMARNAT),
# se le da un valor de NA a los ceros porque no contienen informaci?n

sp_bdinv <- read.csv("data/sp_bdinv_conabio_semar.csv")
sp_bdinv[sp_bdinv =="0"]<- NA
sp_bdinv[sp_bdinv ==""]<- NA

# Consolidar los datos de las listas ya que una especie puede encontrarse
# en m?s de una lista, se siguio el tutorial encontrado en
#http://stackoverflow.com/questions/17328445/consolidating-multiple-duplicated-rows-of-a-dataframe-in-r?rq=1

sp_bdinv <- data.table(sp_bdinv)
sp_bdinv <- sp_bdinv[, lapply(.SD, na.omit), by= "sp"]
sp_bdinv <- data.frame(sp_bdinv)

# Pegar las columnas de informaci?n de la listas en una variable

sp_bdinv <- within(sp_bdinv, list <- paste(A,B,sep="-"))
sp_bdinv <- sp_bdinv[-c(2,3)]

# ahora si, se intersectan las bases de datos

sp_bdinv_anp <- inner_join(sp_bdinv,sp_plantas, by="sp")
# Cantidad de especies 61

# Filtro de plantas acuaticas------------------------------
# No son relevantes porque el an?lisis es eminentemente terrestre

###### FILTRO DE PLANTAS ACUATICAS##########

#quitar Pistia stratiotes, Cyperus alternifolius, Cyperus difformis,
# Cyperus involucratus, Egeria densa, Arundo donax, Glyceria fluitans,
# Phragmites australis, Potamogeton crispus, Stuckenia pectinata,
#Typha latifolia, Rorippa nasturtium-aquaticum, Myriophyllum aquaticum,
# Polygonum amphibium, libro azul profesora koleff
# Hydrocotyle ranunculoides, http://inin.gob.mx/publicaciones/documentospdf/CN%2058%20Estimacion%20de%20biomasa.pdf
# Hydrocotyle verticillata, https://dialnet.unirioja.es/descarga/articulo/1395832.pdf
# Juncus interior acuatica seg?n http://www.snib.mx/iptconabio/resource?r=SNIB-BE023-BE023506F-ND

#####plantas acuaticas
sp_bdinv_anp[sp_bdinv_anp == "Arundo donax"] <- NA
sp_bdinv_anp[sp_bdinv_anp == "Agrostis stolonifera"] <- NA
sp_bdinv_anp[sp_bdinv_anp == "Casuarina cunninghamiana"] <- NA
sp_bdinv_anp[sp_bdinv_anp == "Eleusine indica"] <- NA
sp_bdinv_anp[sp_bdinv_anp == "Hedychium coronarium"] <- NA
sp_bdinv_anp <- na.omit(sp_bdinv_anp)
# 56 spp

# Intersectar base de datos ---------------

# Leer base de datos con spp de invasividad MERI
# Seleccionar las especies que tienen una evaluacion MUY ALTA
# y aquellas especies de las cuales no se encontro o no se realizo un
# MERI (principio de precauci?n)

sp_bdinv_meri <- read.csv("data/sp_bdinv_meri.csv")
high <- sp_bdinv_meri[sp_bdinv_meri$Categoria.riesgo == "MUY ALTO", ]
NE <-  sp_bdinv_meri[sp_bdinv_meri$Categoria.riesgo == "NE", ]
sp_bdinv_high <- rbind(high, NE)

# ahora si, se intersectar las bases de datos

sp_bdinv_anp <- semi_join(sp_bdinv_anp, sp_bdinv_high, by= "sp") 
# 25 spp

write.csv(sp_bdinv_anp, file = "output/02_sel_spp_inv.csv",row.names = F)
