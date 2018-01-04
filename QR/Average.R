rm(list = ls())

library("lubridate") #先安装
library("ggplot2")   #先安装

sub_path = "Newfolder/"

# filename0 = "DTXJZhongCR1000_LL_SFG_Min30_2015_09_25_12_20_01.dat"
# prefix_site = "SFGT"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'

# filename0 = "DTXJZhongCR1000_LL_SFG_Min120_2015_09_25_12_20_01.dat"
# prefix_site = "SFGT"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
# suffix_TMP = '_Avg'
#
filename0 = "DTXJShangCR1000_Min30_2015_09_25_12_28_57.dat"
prefix_site = "PT10"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg' 
#
# filename0 = "DTXJShangCR1000_Min120_2015_09_25_12_28_57.dat"
# prefix_site = "PT10"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'


filename = paste(sub_path,filename0, sep = "") # 加上路径
headers  = read.csv(
  filename, header = 1, sep = ','
  , skip =1, nrows = 1
)

# 只读取数据部分（第五行以后）
data     = read.csv(filename, skip = 4, sep = ',',header = 0)

date_TS  = as.Date(data[,1], "%Y-%m-%d") # 日期

allname_list = names(headers)

fdate<-factor(date_TS)

date_TS  <-levels(fdate)

n_var<-length(data)

n_obs<-length(date_TS)

data<-
  
for(i in 5:17) {
data[,i]<- tapply(data[,i],fdate,mean)
}
