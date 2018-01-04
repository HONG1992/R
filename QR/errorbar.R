rm(list = ls())

library("lubridate") #先安装
library("ggplot2")   #先安装
library("MASS")   #先安装
library("grid")
# library("Hmisc")     #先安装

#============== Deal with original data and combine all data in frozen & thawed period ============

sub_path = "Raw_data/"
new_data_path = "New_data(short)/"
figure_path = "Figures_new_intervals/"
flag_added = "new_"

list_filename    = c("1#_Daily_201405.dat",
                     "PT3_Daily.dat",
                     "PT4_Daily_2015_09_26_17_03_52.dat",
                     "PT5_Daily_2015_09_25_13_39_15.dat")

list_prefix_site = c("PT1","PT3", "PT4", "PT5")
list_prefix_VWC  = c('VWC_','VWC_','VWC_','VWC_')
list_suffix_VWC  = c('_Avg','_Avg','_Avg','_Avg')
list_prefix_TMP  = c('ST_','ST_','ST_','ST_')
list_suffix_TMP  = c('_Avg','_Avg','_Avg','_Avg')

TMP_FR_ALL = NULL
TMP_TH_ALL = NULL

VWC_FR_ALL = NULL
VWC_TH_ALL = NULL

SITE_FR_ALL = NULL
SITE_TH_ALL = NULL

DEPTH_FR_ALL = NULL
DEPTH_TH_ALL = NULL

for (i_file in 1:length(list_filename)) {
  
  filename = list_filename[i_file]
  prefix_site = list_prefix_site[i_file]
  prefix_VWC = list_prefix_VWC[i_file]
  suffix_VWC = list_suffix_VWC[i_file]
  prefix_TMP = list_prefix_TMP[i_file]
  suffix_TMP = list_suffix_TMP[i_file]
  
  filename = paste(new_data_path, flag_added, filename, sep = "")
  
  # 只读取头文件（第二行）
  headers  = read.csv(
    filename, header = 1, sep = ','
    , skip = 0, nrows = 1
  )
  
  # 只读取数据部分（第五行以后）
  data     = read.csv(filename, skip = 1, sep = ',',header = 0)
  
  date_TS  = as.Date(data[,2], "%Y-%m-%d") # 日期
  
  allname_list = names(headers)
  
  # 找到平均水分的所有列
  
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
  
  n_depth = length(same_depth) #获取相同观测深度的数量
  
  all_years  = unique(year(date_TS)) # 获取所有年份
  n_years    = length(all_years) # 获取年份数量
  year_list  = year(date_TS)
  month_list = month(date_TS)
  
  # 获取每个深度上的起始月份 +++ 2015/10/27
  #   1）计算月平均值
  #   2）找到最大值
  #   3）往后推12个月
  
  onset_final_month = matrix(data = NA, nrow = n_depth, ncol = 2)
  
  temp_date <- seq.Date(from = date_TS[1],
                        to = date_TS[length(date_TS)],
                        by = "month") #生成月份序列
  
  for (i in 1:n_depth) {
    #对深度上进行循环计算
    
    tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
    
    tmp_idx_in_data = grep(tmp_name,allname_list) #找到对应的列序号
    
    TMP = data[,tmp_idx_in_data]
    
    yyyymm <-
      paste(format(as.POSIXlt(date_TS), format = "%Y-%m"), "01", sep = "-")  # 构造时间序列，所有的day==01
    
    monthly_mean <- tapply(TMP, yyyymm, mean, simplify = T)   # 计算月平均值
    
    idx_max = which.max(monthly_mean)    # 找到最大值所在月份
    
    onset_final_month[i,1] = month(temp_date[idx_max])
    onset_final_month[i,2] = month(temp_date[idx_max]) - 1
  }
  
  for (i in 1:n_depth) {
    
    #  对逐个深度进行计算
    
    onset = onset_final_month[i,1]   # 得到这个深度上的温度最高的月份，即开始月份
    final = onset_final_month[i,2]   # 得到结束月份
    
    # (n_years - 1)
    
    for (yr in 1:(n_years - 1)) {
      
      #  对年份进行循环计算
      
      idx_year_part_1 = which(year_list == all_years[yr] &
                                month_list >= onset)            # 找到前一年的半截数据的位置
      idx_year_part_2 = which(year_list == all_years[yr + 1] &
                                month_list <= final)            # 找到后一年的半截数据的位置
      idx_year        = c(idx_year_part_1, idx_year_part_2)     # 把上面两个拼接起来，得到一整年的数据位置
      
      if (length(idx_year) >= 280) {
        
        # 如果一年的数据量满足一定条件，则继续，否则就不进行下面的计算
        
        data_selected_year = data[idx_year,] # 提取某一年的所有数据
        
        # 获取相应深度上的VWC和TMP
        
        vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
        tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
        
        vwc_idx_in_data = grep(vwc_name,allname_list) #找到对应的列序号
        tmp_idx_in_data = grep(tmp_name,allname_list) #找到对应的列序号
        
        VWC = data_selected_year[,vwc_idx_in_data]    # 提取含水量数据
        TMP = data_selected_year[,tmp_idx_in_data]    # 提取温度数据
        
        idx = which(TMP < -0.025)  # 找到小于某一温度值的所有位置
        
        VWC = VWC[idx]            # 筛选 含水量
        TMP = TMP[idx]            # 筛选 温度
        
        n_points = length(TMP)
        
        min_TMP = which.min(TMP) # 找到最低温度所在位置
        
        TMP_FR  = TMP[1:min_TMP] # 冻结期温度
        VWC_FR  = VWC[1:min_TMP] # 冻结期水分
        
        TMP_TH  = TMP[(min_TMP + 1):n_points] # 融化期温度
        VWC_TH  = VWC[(min_TMP + 1):n_points] # 融化期水分
        
        b_FR = sort(TMP_FR, index.return = TRUE) # 对冻结期按温度进行排序
        TMP_FR = TMP_FR[b_FR$ix]
        VWC_FR = VWC_FR[b_FR$ix]
        
        b_TH = sort(TMP_TH, index.return = TRUE) # 对融化期按温度进行排序
        TMP_TH = TMP_TH[b_TH$ix]
        VWC_TH = VWC_TH[b_TH$ix]
        
        # SITE_FR = array(dim = length(TMP_FR))
        SITE_FR = rep(prefix_site, length(TMP_FR))
        SITE_TH = rep(prefix_site, length(TMP_TH))
        
        DEPTH_FR = rep(same_depth[i], length(TMP_FR))
        DEPTH_TH = rep(same_depth[i], length(TMP_TH))
        
        DEPTH_FR_ALL = c(DEPTH_FR_ALL, DEPTH_FR)
        DEPTH_TH_ALL = c(DEPTH_TH_ALL, DEPTH_TH)
        
        SITE_FR_ALL = c(SITE_FR_ALL, SITE_FR)
        SITE_TH_ALL = c(SITE_TH_ALL, SITE_TH)
        
        TMP_FR_ALL = c(TMP_FR_ALL, TMP_FR)
        VWC_FR_ALL = c(VWC_FR_ALL, VWC_FR)
        
        TMP_TH_ALL = c(TMP_TH_ALL, TMP_TH)
        VWC_TH_ALL = c(VWC_TH_ALL, VWC_TH) 
        
      }
    }
  }
}

# 将所有上面计算得到的变量拼接起来 ===
# 1st col = site name
# 2nd col = depth
# 3rd col = soil temperature during frozen period
# 4th col = vol. water content during frozen period

data_fr <-
  data.frame(
    Borehole = SITE_FR_ALL, Depth = DEPTH_FR_ALL, TMP_FR = TMP_FR_ALL, VWC_FR = VWC_FR_ALL)  # 构造冻结期的数据框

# 将所有上面计算得到的变量拼接起来 ===
# 1st col = site name
# 2nd col = depth
# 3rd col = soil temperature during thawed period
# 4th col = vol. water content during thawed period

data_th <-
  data.frame(
    Borehole = SITE_TH_ALL, Depth = DEPTH_TH_ALL, TMP_TH = TMP_TH_ALL, VWC_TH = VWC_TH_ALL)  # 构造融化期的数据框

#============== Deal with soil texture ============

# Read soil texture table :

Soil_Texture = read.csv('Soil_Texture.csv', skip = 0, sep = ',',header = TRUE)
Texture_List = Soil_Texture$Soil.Texture.Classification
all_types  = unique(Soil_Texture$Soil.Texture.Classification) # 获取所有Soil types

for (i in 1:length(all_types)) { # for each type ===
  data_fr_tmp_soil_texture = NULL
  data_fr_vwc_soil_texture = NULL 
  data_th_tmp_soil_texture = NULL
  data_th_vwc_soil_texture = NULL
  
  type = all_types[i] # one of soil texture type
  idx = grep(type, Soil_Texture$Soil.Texture.Classification) # obtain the corresponding borehole & depth
  
  for (ii in 1:length(idx)){ # obtain all data for the same soil type ===
    
    idx_in_data_fr = grep(paste(Soil_Texture$Borehole[idx[ii]], Soil_Texture$Depth[idx[ii]], sep = '_'), 
                          paste(data_fr$Borehole, data_fr$Depth, sep = '_'));
    data_fr_tmp_soil_texture = c(data_fr_tmp_soil_texture, data_fr$TMP_FR[idx_in_data_fr])
    data_fr_vwc_soil_texture = c(data_fr_vwc_soil_texture, data_fr$VWC_FR[idx_in_data_fr])
    
    idx_in_data_th = grep(paste(Soil_Texture$Borehole[idx[ii]], Soil_Texture$Depth[idx[ii]], sep = '_'), 
                          paste(data_th$Borehole, data_th$Depth, sep = '_'));
    data_th_tmp_soil_texture = c(data_th_tmp_soil_texture, data_th$TMP_TH[idx_in_data_th])
    data_th_vwc_soil_texture = c(data_th_vwc_soil_texture, data_th$VWC_TH[idx_in_data_th])
    
  }
  
  # the following is traditional process in previous programs === 
  
  # 设置拟合函数的数学形式
  
  f = function(x, a, b) {
    # a * abs(x) ^ b
    a * abs(x) ^ b
  }
  
  # 拟合结果 Modified by K, 20160403
  
  vwc_a_b_FR <-
    nls(data_fr_vwc_soil_texture ~ f(data_fr_tmp_soil_texture, a, b), start = list(a = 1, b = 1))
  coefs_FR = coef(vwc_a_b_FR) # 冻结期的估计结果
  # confint_FR = confint(vwc_a_b_FR,level = 0.99)
  RSS.p.FR <- sum(residuals(vwc_a_b_FR) ^ 2)
  TSS.FR <- sum((data_fr_vwc_soil_texture - mean(data_fr_vwc_soil_texture)) ^ 2)
  R.FR <- 1 - (RSS.p.FR / TSS.FR)
  
  vwc_a_b_TH <-
    nls(data_th_vwc_soil_texture ~ f(data_th_tmp_soil_texture, a, b), start = list(a = 1, b = 1))
  coefs_TH = coef(vwc_a_b_TH) # 融化期的估计结果
  # confint_TH = confint(vwc_a_b_TH,level = 0.99)
  RSS.p.TH <- sum(residuals(vwc_a_b_TH) ^ 2)
  TSS.TH <- sum((data_th_vwc_soil_texture - mean(data_th_vwc_soil_texture)) ^ 2)
  R.TH <- 1 - (RSS.p.TH / TSS.TH)
  
  #  画图 ... ...
  
  #设置输出图片的文件名 Modified by K, 20160404
  
  pdf_name = paste(
    figure_path,
    'GH_',
    type,
    '.png',
    sep = ""
  )
  
  # SETING XLIMIT and YLIMIT
  xlim0 = c(-12, 0)
  ylim0 = c(0, 0.45)
  
  main_title = paste(type)# 构造主标题 Modified by K, 20160404
  
  data_fr_1 <-
    data.frame(X1 = data_fr_tmp_soil_texture,
               Y11 = data_fr_vwc_soil_texture,
               Y12 = predict(vwc_a_b_FR))  # 构造冻结期的数据框 Modified by K, 20160404
  
  

  
  data_th_1 <-
    data.frame(X2 = data_th_tmp_soil_texture,
               Y21 = data_th_vwc_soil_texture,
               Y22 = predict(vwc_a_b_TH))  # 构造融化期的数据框 Modified by K, 20160404
  
 

  
  
#   P_X = c(data_fr_1$X1,data_th_1$X2)
#   P_Y = c(data_fr_1$Y11,data_th_1$Y21)
#   P_Y1 = c(data_fr_1$Y12,data_th_1$Y22)
#   P_C = c(P_FR, P_TH)
  
  C <- cut(data_fr_1$X1,breaks=60)
  D <- cut(data_th_1$X2,breaks=60)
  
  P_X_FR <- tapply(data_fr_1$X1, list(C), mean)
  P_Y_FR <- tapply(data_fr_1$Y11, list(C), mean)
  Y_FR_SD <- tapply(data_fr_1$Y11, list(C), sd) 
  
  P_X_TH <- tapply(data_th_1$X2, list(D), mean)
  P_Y_TH <- tapply(data_th_1$Y21, list(D), mean) 
  Y_TH_SD <- tapply(data_th_1$Y21, list(D), sd) 
  
  P_X_FR <- as.data.frame(P_X_FR)
  P_Y_FR <- as.data.frame(P_Y_FR)
  
  P_X_TH <- as.data.frame(P_X_TH)
  P_Y_TH <- as.data.frame(P_Y_TH)
  
  x_FR <- as.vector(P_X_FR$P_X_FR)
  y_FR <- as.vector(P_Y_FR$P_Y_FR)
  y.sd.FR <- as.vector(Y_FR_SD)
  
  
  x_TH <- as.vector(P_X_TH$P_X_TH)
  y_TH <- as.vector(P_Y_TH$P_Y_TH)
  y.sd.TH <- as.vector(Y_TH_SD)

  data_new_FR <- data.frame(x_FR,y_FR,sd=y.sd.FR)
  data_new_TH <- data.frame(x_TH,y_TH,sd=y.sd.TH)
  P_FR <- paste('Freezing',character(length(data_new_FR$x_FR)), sep = "")
  P_TH <- paste('Thawing',character(length(data_new_TH$x_TH)), sep = "")
  
  t1=c(data_new_FR$x,data_new_TH$x_TH)
  t2=c(data_new_FR$y_FR,data_new_TH$y_TH)
  t3=c(P_FR,P_TH)
  t4=c(data_new_FR$sd,data_new_TH$sd)
  data_new <- data.frame(t1,t2,t3,t4)
  

  
  
  p <-  ggplot(data=data_new,aes(x=t1,y=t2,ymin=t2-t4,ymax=t2+t4,color=t3)) +  #errorbar
    ylab(expression(paste("Volumetric Water Content (", "m" ^ 3, "/m" ^ 3, ")", sep = ""))) +  # y标题
    xlab(expression(paste("Soil Temperature (" ^ "o", "C)", sep = ""))) +  # x标题
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # y取值范围及标注的间隔
    scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)  +     # x取值范围及标注的间隔
    geom_point(size =2,alpha = 0.5) +
    geom_errorbar(width = 0.2,alpha = 0.5) +
    scale_colour_manual(values = c("#000080", "#FF0000")) # 设置绘图颜色， 注意与上面的对应起来

  
  # 设置坐标轴的粗细、字体等
  last_plot() + theme(
    axis.text.x = element_text(
      #坐标轴X的字体大小、颜色、旋转角度
      size = 18,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    axis.text.y = element_text(
      #坐标轴Y的字体大小、颜色、旋转角度
      size = 18,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    axis.title.x = element_text(
      #X标题的字体大小、颜色、旋转角度
      size = 20,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    
    axis.title.y = element_text(
      #Y标题的字体大小、颜色、旋转角度
      size = 20,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 90
    )
  )
  
  last_plot() +
    theme(legend.title = element_blank()) +  # 去掉图例的标题
    theme(legend.text = element_text(size = 20)) + # 设置图例的文字大小
    theme(legend.justification = 'right',legend.position = c(0.9, 0.8))+
    theme(legend.key.size=unit(1.5,'cm')) 
   
  
  
  
  ## 给出相对位置
  x0 = 0.0
  y0 = 0.9
  ## 计算绝对位置
  x00 = (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
  y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
  y01 = (ylim0[2] - ylim0[1]) * (y0 - 0.085) + ylim0[1]
  
  x1 = 0.25
  ## 计算绝对位置
  x10 = (xlim0[2] - xlim0[1]) * x1 + xlim0[1]
  
  
  last_plot() + ggtitle(main_title)  +    # 加个标题
  theme(plot.title = element_text(size=22))
  
  ggsave(
    pdf_name,
    width = 9,
    height = 5,
    limitsize = FALSE,
    dpi = 600  # 输出图片，尺寸9×5
  )
}
