rm(list=ls())

setwd("~/Documents/R/test")

library(ncdf4)
library(lattice)
library(RColorBrewer)
library(maptools)
library(maps)
library(rasterVis)
library(sp)
library(rgdal)
library(raster)
library(rgeos)

# read netcdf data 
filename <- 'cru_ts4.01.2011.2016.tmp.dat.nc'
nc <- nc_open(filename)
dname <- "tmp"
coast_lines <- shapefile("~/Documents/R/test/ne_110m_coastline/ne_110m_coastline.shp")
# nc_close(nc)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")
tunits <- ncatt_get(nc,"time","units")
tmp_array <- ncvar_get(nc, dname)
nc_close(nc)

nlon <- dim(lon)
nlat <- dim(lat)
nt <- dim(time)

tustr <- strsplit(tunits$value, " since ")
tustr <- unlist(tustr)

date1 <- as.Date(time, origin = tustr[2], by=tustr[1])
tyear <- year(date1)
tmonth <- month(date1)
tday  <- day(date1)

# ZLIM = c(-40,40)
# 
# png(filename="eof_maps.png", width=10, height=5, units="in", res= 300)
# par(mar=c(0,0.25,0.25,0))
# image(tmp_array[ , , 1], col = rev(brewer.pal(10, "RdBu")),
#       xaxs="i", yaxs="i", xlab="", ylab="",
#       axes = FALSE)
# map("world2", add=TRUE, col=1, lwd=0.5)
# box()
# par(mar=c(1,3,3,3))
# 
# imageScale(1, ZLIM, col=rev(brewer.pal(10, "RdBu")), axis.pos=1)
# box()
# dev.off()

m <- 1
tmp_slice <- tmp_array[, , m]
tmp <- raster(tmp_slice)
mapTheme <- rasterTheme(region=brewer.pal(10,"RdBu"))
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
plt <- levelplot(tmp_slice ~ lon * lat, data=grid, 
                 at=cutpts,pretty=T, 
                 par.settings=mapTheme,
                 xlab="Longtitude",
                 ylab="Latitude",
                 #axis(side=1,at=c(-100,0,100),labels=c("W"," ", "E")),
                 col.regions=(rev(brewer.pal(10,"RdBu"))))
plt + layer(sp.lines(coast_lines, col="black", lwd=0.5))


# image(lon, lat, tmp_slice, col = rev(brewer.pal(10, "RdBu")))
# grid <- expand.grid(lon=lon, lat=lat)
# cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
# levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts,pretty=T, 
#           col.regions=(rev(brewer.pal(10,"RdBu"))))
# plot(coast_lines)