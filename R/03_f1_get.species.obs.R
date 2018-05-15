# función para obtener el numero de observaciones por nombre de especie y clave key
# tomada de https://sites.google.com/site/mikegahan1/rgbif
Get.Species.Obs <- function(species.name, type) {
  # extraer la clave para cada nombre
  key <- name_backbone(name = species.name)$speciesKey

  if (is.null(key)) {
    out.table <- data.table(Species = species.name, Key = NA, Count = NA)
  } else {
    species.obs <- occ_search(taxonKey = key, basisOfRecord = type, return = "meta", hasCoordinate = TRUE)$count
    out.table <- data.table(Species = species.name, Key = key, Count = species.obs)
  }
  return(out.table)
}
