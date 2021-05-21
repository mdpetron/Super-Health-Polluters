# now lets look at using INMAP to assess exposures on specific populations 

library(devtools)
library(data.table)
devtools::install_github("cchoirat/rinmap")
library(rinmap)

data(units2005)

run_inmap()

getAnywhere('run_inmap')


install.packages("ncdf4")

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
setwd("C:/Users/Mike Petroni/Documents/GitHub/Super-Health-Polluters/Data/")
unzip("Inmap/isrm_v1.2.1.zip")
 nc_data <- nc_open() 