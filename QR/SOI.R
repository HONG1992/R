rm(list = ls())
library("soiltexture")
library("fields")

filename1 = "Physical Properties summary_PF.csv"
filename2 = "Physical Properties summary_AL.csv"
filename3 = "All_VWC_Results_1.csv"
data  = read.csv(filename3, header = 1, sep = ',', skip =0)
data[,7]=as.numeric(data[,7])
data[,8]=as.numeric(data[,8])
pdf(file="ufwc.pdf")
geo <- TT.geo.get()
color_table = rev(heat.colors(12))

iwd.res <- TT.iwd(
  geo = geo,
  tri.data = data,
  z.name = "vwc") 
TT.image(
  x = iwd.res,
  geo = geo,
  class.sys = "USDA.TT",
  grid.show = FALSE
)
TT.plot(grid.show = F,
        geo = geo,
        class.sys = "USDA.TT",
        tri.data = data,
        pch =3,
        add = T # <<-- important
)

z_range <- range( c(iwd.res$z), na.rm = T)
image.plot(legend.only=TRUE, zlim= z_range, col = color_table, 
           smallplot = c(.87,.90, 0.15, 0.80), 
           legend.args=list(text="UFWC"))

dev.off()