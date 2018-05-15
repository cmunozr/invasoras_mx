# Aqui hay otra funci?fÃ‚Â³n construida para extraer los datos de las especies requeridas
# desde GBIF
# funci?fÃ‚Â³n tomada desde https://sites.google.com/site/mikegahan1/rgbif

Get.Species.Data <- function(sp_conteo, type) {
  # Extraer valores key de la tabla de referencia
  key.lookup <- sp_conteo
  # imprimir la clave
  key <- key.lookup$Key
  species.obs <- (key.lookup$Count)

  # Campos clave que se desean extraer
  key.fields <- c(
    "species", "decimalLongitude", "decimalLatitude",
    "locality", "country"
  )

  out.data <- occ_search(
    taxonKey = key, return = "data", start = 1,
    limit = species.obs, basisOfRecord = type, fields = key.fields, hasCoordinate = TRUE
  )
  setDT(out.data)
  return(out.data)
}
