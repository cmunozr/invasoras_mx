search_clim_plushum <- function(sp = spp[4],
                                clim = "output/10_modsel_clim.csv",
                                eval = "output/10_eval_all/",
                                pattern.mix = "_climhum") {
  # leerlos
  elec_csv <- read.csv(clim)
  # buscar el modelo de la especie i
  i <- grep(sp, elec_csv$ModelFile_proy)
  if(length(i)==0){data.frame(matrix(nrow = 0, ncol = 0 ))}else{

  elec_spp <- elec_csv[i, ]
  path_ <- as.character(elec_spp$ModelFile_proy)
  # eliminar la ruta, dejar solo el nombre
  elec_spp <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(path_))

  # leer los csv de evaluacion de climhum
  climhum <- read.csv(paste0(eval, sp, pattern.mix, ".csv"))
  i <- grep(elec_spp, climhum$ModelFile_proy)
  climhum <- climhum[i, ]
  return(climhum)
  }
}
