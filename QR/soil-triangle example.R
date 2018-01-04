rm(list = ls())
library("soiltexture")
# library("ggplot2")
# Create a dummy data frame of soil textures:
my.text <- data.frame(
  "CLAY" = c(05,60,15,05,25,05,25,45,65,75,13,47),
  "SILT" = c(05,08,15,25,55,85,65,45,15,15,17,43),
  "SAND" = c(90,32,70,70,20,10,10,10,20,10,70,10),
  "OC" = c(20,14,15,05,12,15,07,21,25,30,05,28)
) #
# Display the table:
my.text
pdf(file="myplot.pdf")
geo <- TT.geo.get()
iwd.res <- TT.iwd(
  geo = geo,
  tri.data = my.text,
  z.name = "") #
TT.image(
  x = iwd.res,
  geo = geo,
  class.sys = "USDA.TT",
  grid.show = FALSE
)
TT.plot(grid.show = F,
        geo = geo,
        class.sys = "USDA.TT",
        tri.data = my.text,
        pch = 19,
        add = T # <<-- important
)
dev.off()