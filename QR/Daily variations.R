rm(list = ls())

library("lubridate") #?Ȱ?װ
library("ggplot2")   #?Ȱ?װ
library("MASS")   #?Ȱ?װ
library("Hmisc")     #?Ȱ?װ

sub_path = "Newfolder/"

filename = "PT4_MIN_30_2015_09_26_17_03_52.dat"
prefix_site = "PT4"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg'

filename = paste(sub_path,filename, sep = "")

# 只读取头文件（第二行）
headers  = read.csv(
  filename, header = 1, sep = ','
  , skip = 1, nrows = 1
)

# 只读取数据部分（第五行以后）
data     = read.csv(filename, skip = 4, sep = ',',header = 0)

allname_list = names(headers)

varname_vwc = names(headers)
varname_vwc <- varname_vwc[grepl(prefix_VWC,varname_vwc)]
varname_vwc <- varname_vwc[grepl(suffix_VWC,varname_vwc)]

varname_vwc <- substr(varname_vwc, nchar(prefix_VWC) + 1,
                      nchar(varname_vwc) - nchar(suffix_VWC))

# 找到平均温度的所有列

varname_tmp = names(headers)
varname_tmp <- varname_tmp[grepl(prefix_TMP,varname_tmp)]
varname_tmp <- varname_tmp[grepl(suffix_TMP,varname_tmp)]

varname_tmp <- substr(varname_tmp, nchar(prefix_TMP) + 1,
                      nchar(varname_tmp) - nchar(suffix_TMP))

depth_vwc = gsub('[^0-9]','',varname_vwc) #提取深度数值
depth_tmp = gsub('[^0-9]','',varname_tmp) #提取深度数值

same_depth = intersect(depth_vwc, depth_tmp) #获取相同观测深度

same_depth = same_depth[1]

tmp_name = paste(prefix_TMP, same_depth,'cm',suffix_TMP,sep = "")

tmp_idx_in_data = grep(tmp_name,allname_list) #找到对应的列序号

TMP = data[,tmp_idx_in_data]

vwc_name = paste(prefix_VWC, same_depth,'cm',suffix_VWC,sep = "")

vwc_idx_in_data = grep(vwc_name,allname_list) #找到对应的列序号

VWC = data[,vwc_idx_in_data]

tmp_idx <- 

