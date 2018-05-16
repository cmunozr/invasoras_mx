compile <- function(path.enmeval = "output/09_mod_clim/",
                    path.proc.bin = "output/10_eval_clim/",
                    pattern.enmeval = "_ENMeval.csv",
                    pattern.cal = "_eval.csv",
                    pattern.proy = "kmeval.csv",
                    type = "clim",
                    sp, write = T, path.write) {
  a <- dir(path = path.enmeval, pattern = sp, full.names = T)
  m <- rep(list(1), length(a))
  n <- rep(list(1), length(a))
  r <- rep(list(1), length(a))
  
  for (i in 1:length(a))
  {
    m[[i]] <- list.files(a[i], pattern = pattern.enmeval, full.names = T)
    n[[i]] <- read.csv(m[[i]])
    r[[i]] <- n[[i]][order(n[[i]][,"settings"]),]
  }

  o <- rbindlist(r)
  
  b <- dir(path = path.proc.bin, pattern = sp, full.names = T)
  c <- b[grep(pattern.proy, b)]

  if (is.null(pattern.cal)) {
    q <- rep(list(1), length(c))
    for (i in 1:length(c))
    {
      q[[i]] <- read.csv(c[i])
    }
    q <- rbindlist(q)
    colnames(q) <- paste(colnames(q), sep = "_", "proy")
    results <- cbind(o, q)
  } else {
    q <- rep(list(1), length(c))
    for (i in 1:length(c))
    {
      q[[i]] <- read.csv(c[i])
    }
    q <- rbindlist(q)
    colnames(q) <- paste(colnames(q), sep = "_", "proy")

    e <- b[grep(pattern.cal, b)]
    f <- rep(list(1), length(e))
    for (i in 1:length(c))
    {
      f[[i]] <- read.csv(e[i])
    }
    f <- rbindlist(f)
    colnames(f) <- paste(colnames(f), sep = "_", "cal")
    results <- cbind(o, q, f)
  }
  if (write == TRUE) {
    write.csv(results, paste0(path.write, sp, "_", type, ".csv"),
              row.names = F
    )
    return("ok")
  } else {
    return(results)
  }
  rm(list = ls())
  gc()
}
