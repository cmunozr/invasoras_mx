# Carlos Munoz 21 de abril de 2017
# Modificada la funcion Outliers de biogeo

# Outliers trans: el objetivo es identificar los puntos outliers de una
# variable ambiental que vayamos a usar para reconstruir el nicho de una especie.
# Usa dos metodos, cuartiles y r jacknife. Esta funcion luego es usada 
# en All_outilers

rjack <-
  function (d) 
  {
    xx <- d
    d <- unique(d)
    rng <- diff(range(d))
    mx <- mean(d)
    n <- as.numeric(length(d))
    n1 <- abs(n - 1)
    t1 <- (0.95 * sqrt(n)) + 0.2
    x <- sort(d)
    y <- rep(0, n1)
    i <- 201
    for (i in 1:n1) {
      x1 <- x[i + 1]
      if (x[i] < mx) {
        y[i] <- (x1 - x[i]) * (mx - x[i])
      }
      else {
        y[i] <- (x1 - x[i]) * (x1 - mx)
      }
    }
    my <- mean(y)
    z <- y/(sqrt(sum((y - my)^2)/n1))
    out <- rep(0, length(xx))
    if (any(z > t1)) {
      f <- which(z > t1)
      v <- x[f]
      if (v < median(x)) {
        xa <- (xx <= v) * 1
        out <- out + xa
      }
      if (v > median(x)) {
        xb <- (xx >= v) * 1
        out <- out + xb
      }
    }
    else {
      out <- out
    }
    f <- which(out == 1)
  }

outliers_trans <- function(dat, env)
{ 
  nr <- nrow(dat)
  ee <- rep(0, nr)
  ee2 <- rep(0, nr)
  b1 <- boxplot.stats(env, coef = 1.5)
  xc <- env
  xr <- range(b1$stats)
  fe <- which(xc > xr[2] | xc < xr[1])
  fe2 <- rjack(xc)
  ee[fe] <- 1
  ee2[fe2] <- 1
  out <- cbind(ee, ee2)
  return(out)
} 

# all_outliers: identifica y elimina los puntos outlier. Se consider?
# outlier a cualquier punto que fuese marcado para 3 o más variables
# los dos metodos (cuartiles o rjacknife).

all_outliers <- function(dat, env)
{
  cn <- names(env)
  nv <- length(cn)
  ee <- {
}
  for (i in 1:nv) {
  e <- outliers_trans(dat = dat, env = na.omit(env[,i]))
  ee <- cbind(ee, e)
}
  nv2 <- nv * 2
  rjack <- rowSums(ee[, seq(2, nv2, 2)])
  quart <- rowSums(ee[, seq(1, nv2, 2)])
  nr <- nrow(dat)
  out <- rep(0, nr)
  fac <- which(rjack >= 2 | quart >= 2 )
  dat2 <- cbind(dat,out)
  no_out <- dat2[-fac,]
  if (nrow(no_out)== 0)
  { no_out <- dat
    no_out[c('out', names(env))] <- NULL}
  else
    {
      no_out[c('out', names(env))] <- NULL 
    }
  return(no_out)
}

env_space <- function(dat,x,y,vars)
  
{
  xy <- dat[,c(x,y)]
  dat2 <- cbind(dat,(extract(vars, xy)))
  return(dat2)
}



