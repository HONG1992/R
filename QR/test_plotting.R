rm(list = ls())

library("lubridate") #?Ȱ?װ
library("ggplot2")   #?Ȱ?װ
# library("Hmisc")     #?Ȱ?װ

f = function(x, a, b) {
  # a * abs(x) ^ b
  a * abs(x) ^ b
}

data_fr <- read.csv('fr.csv')
p_fr <- paste('F',character(length(data_fr$x)), sep = "")
data_fr <-
  data.frame(x = data_fr$x,
             y = data_fr$y)

fr_tmp<-data_fr$x
fr_vwc<-data_fr$y

vwc_a_b_FR <-
  nls(fr_vwc ~ f(fr_tmp, a, b), start = list(a = 1, b = 1))
data_fr_1 <-data.frame(X1 = data_fr$x,
             Y11 = data_fr$y,
             Y12 = predict(vwc_a_b_FR))  


data_th <- read.csv('th.csv')
p_th <- paste('T',character(length(data_th$x)), sep = "")
data_th <-
  data.frame(x = data_th$x,
             y = data_th$y)

th_tmp<-data_th$x
th_vwc<-data_th$y

vwc_a_b_TH <-
  nls(th_vwc ~ f(th_tmp, a, b), start = list(a = 1, b = 1))
data_th_1 <-
  data.frame(X2 = data_th$x,
             Y21 = data_th$y,
             Y22 = predict(vwc_a_b_TH))  

p_x = c(data_fr_1$X1,data_th_1$X2)
p_y = c(data_fr_1$Y11,data_th_1$Y21)
p_y1 = c(data_fr_1$Y12,data_th_1$Y22)
p_c = c(p_fr, p_th)
# 
data <- data.frame(x = p_x, y= p_y, period = p_c)
data1 <- data.frame(x = p_x,y= p_y1, period = p_c)

p<-ggplot(data = data) +
  geom_point(data=data,aes(x = x, y = y,shape = period, col = period),size = 2) + 
  scale_shape_manual(values = c(3,4))

last_plot()+geom_line(data=data1,aes(x = x, y = y,col = period),size =1.5) +
            scale_color_manual(values = c("#006400", "#8B0000"))
 
p
  

