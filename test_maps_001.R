rm(list = ls(all = TRUE))
library( maps )
library( ncdf4 )
# library( rgdal )
library(RColorBrewer)
# library(ggmap)
library(ggplot2)
library(mapdata)

#####
# Make a basemap for data visualization 
#####

world <- map_data("world")

#####
# read nc file:
#####

ncfilename = 'cru_ts4.01.2011.2016.tmp.dat.nc'

ncid = nc_open(ncfilename)

lat  = ncvar_get(ncid, 'lat'); nlat = length(lat)
lon  = ncvar_get(ncid, 'lon'); nlon = length(lon)
tim  = ncvar_get(ncid, 'time')
tim_unit = ncatt_get(ncid, 'time', 'units')$value
tim_unit = unlist(strsplit(tim_unit, ' since '))

datetime = as.Date(tim, origin = tim_unit[2], by=tim_unit[1])

tmp  = ncvar_get(ncid, 'tmp')

nc_close(ncid)

tmp = tmp[ , , 4]

yy = t(kronecker(matrix(1,1,nlon),lat))
xx = kronecker(matrix(1,1,nlat),lon)

yy = matrix(yy, nrow=length(yy))
xx = matrix(xx, nrow=length(xx))
zz = matrix(tmp,nrow=length(tmp))

data0 = data.frame(long = xx, lat = yy, tmp = zz)

# Start plotting ... 

myPalette <- colorRampPalette(rev(brewer.pal(10, "RdYlBu")))

ggplot(data=data0,aes(x=long,y=lat, color=tmp)) + 
  geom_point()+
  scale_colour_gradientn(name = "deg C",colours = myPalette(10), limits=c(-35,35))+
  geom_polygon( data=world, aes(x=long, y=lat, group=group),size=0.3, colour="black",fill=NA)+
  # # Orthographic projection with default orientation (looking down at North pole)
  # coord_map("orthographic", orientation = c(90, 0, 0)) + 
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  labs(title = "CRU TEM v4.01",
       subtitle = "Colored by Air Temperature",
       x = "", y = "") +
  ggsave("test.png",width=10, height=8, dpi=150)