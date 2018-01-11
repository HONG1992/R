rm(list = ls(all=TRUE))

library(gvlma)
library(lavaan)
library(psych)
library(sem)
library(lubridate)

new_data_path = "New_data(short)/"
flag_added = "new_"

filename = "1#_Daily_201405.dat"
prefix_site = "PT1"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg'

filename = paste(new_data_path, flag_added, filename, sep = "")

df <- read.table(file = 'All_VWC_Results.csv', header = T, sep = ',')
headers <- read.csv(filename, header = 1, sep = ',', skip = 0, nrows = 1)
data <- read.csv(filename, skip = 1, sep = ',',header = 0)
date_TS <- as.Date(data[,2], "%Y-%m-%d") 

allname_list<- names(headers)

varname_vwc  <-  names(headers)
varname_vwc <- varname_vwc[grepl(prefix_VWC,varname_vwc)]
varname_vwc <- varname_vwc[grepl(suffix_VWC,varname_vwc)]
varname_vwc <- substr(varname_vwc, nchar(prefix_VWC) + 1,
                      nchar(varname_vwc) - nchar(suffix_VWC))



varname_tmp = names(headers)
varname_tmp <- varname_tmp[grepl(prefix_TMP,varname_tmp)]
varname_tmp <- varname_tmp[grepl(suffix_TMP,varname_tmp)]
varname_tmp <- substr(varname_tmp, nchar(prefix_TMP) + 1,
                      nchar(varname_tmp) - nchar(suffix_TMP))

depth_vwc = gsub('[^0-9]','',varname_vwc) 
depth_tmp = gsub('[^0-9]','',varname_tmp) 

same_depth = intersect(depth_vwc, depth_tmp) 

n_depth = length(same_depth)

all_years  = unique(year(date_TS)) 
n_years    = length(all_years) 
year_list  = year(date_TS)
month_list = month(date_TS)

# HS.model <- 'a =~ SILT+SAND+ph+ec+orp+tvwc
#              b =~ ph+ec+orp+tvwc'
# 
# fit <- cfa(HS.model,data=df)
# parTable(fit)
# summary(fit)

# lm1<-lm(a~CLAY+SILT+SAND+ph+ec+orp+tvwc+tmp+vwc+gc+bd+SSA,data=df)
lm1<-lm(a~SILT+SAND+ph+ec+orp+tvwc,data=df)
gvmodel1 <- gvlma(lm1)
summary(gvmodel1)


# lm2<-lm(b~CLAY+SILT+SAND+ph+ec+orp+tvwc+tmp+vwc+gc+bd+SSA,data=df)
lm2<-lm(b~ph+ec+orp+tvwc,data=df)
gvmodel2 <- gvlma(lm2)
summary(gvmodel2)



