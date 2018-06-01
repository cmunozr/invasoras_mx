# Thesis packages
# Operational
thesis_packages <- c("raster", "data.table", "dplyr", "spocc", "rgbif",
"rlist", "sp", "stringr", "plyr", "rgeos", "scrubr", "dismo", "vegan",
"rgdal", "sqldf", "maps", "testthat", "rlist", "ENMeval", "rowr",
"doSNOW", "foreach", "gridExtra", "plotrix", "ggplot2", "colorRamps",
"scatterplot3d", "SpatialPack", "FSA")

missing_pkgs <- thesis_packages[which(!thesis_packages %in% installed.packages())]
if(length(missing_pkgs)) install.packages(missing_pkgs)

if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox')

# Styler
thesis_stylishcode <- c("roxygen2", "styler")
missing_stylishcode <- thesis_stylishcode[which(!thesis_stylishcode %in% installed.packages())]
if(length(missing_stylishcode)) install.packages(missing_stylishcode)

