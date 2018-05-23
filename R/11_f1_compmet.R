# Metric comparation
# The performance metrics to compare depends on column names of the csv, is
# for that reason appears arguments name.a ...

metric_comp <- function(path.a = clim,
                        path.b = climhum,
                        path.c = clim_plushum,
                        compare,
                        path.fig = "figs/",
                        path.perf = "output/11_performance/",
                        name.a = "clim",
                        name.b = "climhum",
                        name.c = "clim_plushum") {
  a <- read.csv(path.a)
  b <- read.csv(path.b)
  c <- read.csv(path.c)

  # performance
  a_extract <- a[, compare]
  b_extract <- b[, compare]
  c_extract <- c[, compare]
  diff_1 <- b_extract - a_extract
  diff_2 <- c_extract - a_extract
  df <- cbind(b_extract, a_extract, diff_1, c_extract, a_extract, diff_2)
  colnames(df) <- c(
    paste0(compare, "_", name.b),
    paste0(compare, "_", name.a),
    "diff_1",
    paste0(compare, "_", name.c),
    paste0(compare, "_", name.a),
    "diff_2"
  )
  write.csv(df, paste0(path.perf, "comp_met_", compare, ".csv"))

  pdf(paste0(path.fig, "11_comp_met_", compare, ".pdf"))
  par(mfrow = c(3, 2), oma = c(0, 0, 2, 0))
  barplot(a_extract,
    ylab = paste(expression(Delta), compare),
    names.arg = seq(1, length(a_extract), by = 1)
  )
  plot.new()
  barplot(b_extract,
    ylab = paste(expression(Delta), compare),
    names.arg = seq(1, length(a_extract), by = 1)
  )
  plot(diff_1, type = "l", ylab = "", xlab = "")
  abline(h = 0)
  barplot(c_extract,
    ylab = paste(expression(Delta), compare),
    names.arg = seq(1, length(a_extract), by = 1), xlab = "sp"
  )
  plot(diff_2, type = "l", ylab = "", xlab = "sp")
  abline(h = 0)
  dev.off()
}
