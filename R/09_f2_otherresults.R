other.results <- function(mod) {
  res <- mod@results
  x10 <- res[grepl('X10.percentile.training.presence.logistic.threshold',
                   rownames(res)),]
  MTP <- res[grepl('Minimum.training.presence.logistic.threshold',
                   rownames(res)),]
  Ent <- res[grepl('Entropy',
                   rownames(res)),]
  df <- data.frame(rbind(x10 = x10, MTP = MTP, Ent = Ent))
  df$var <- row.names(df)
  names(df) <- c("data", "var")
  return(df)
}


