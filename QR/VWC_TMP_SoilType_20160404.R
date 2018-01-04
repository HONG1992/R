rm(list = ls())

library("lubridate") #先安装
library("ggplot2")   #先安装
library("spuRs")     #先安装
source("../function_20160123.R")

sub_path = "Raw_data/"
new_data_path = "New_data/"
figure_path = "Figures/"
flag_added = "new_"

# ++++ 2016-01-20 :

list_filename    = c(
  "EBo_TB_Daily.dat","1#_Daily_201405.dat","PT3_Daily.dat",
  "PT4_Daily_2015_09_26_17_03_52.dat",
  "PT5_Daily_2015_04_06_14_12_49.dat",
  "EBoTA_Daily.dat"
)

list_prefix_site = c("EBo_TB", "PT1", "PT3", "PT4", "PT5","EBoTA")
list_prefix_VWC  = c("Soil_VWC_1_",'VWC_','VWC_','VWC_','VWC_','soil_vwc_')
list_suffix_VWC  = c('_Avg','_Avg','_Avg','_Avg','_Avg','_Avg')
list_prefix_TMP  = c('Soil_Ta_1_','ST_','ST_','ST_','ST_','soil_T_')
list_suffix_TMP  = c('_Avg','_Avg','_Avg','_Avg','_Avg','_Avg')

# filename = "EBo_TB_Daily.dat"
# prefix_site = "EBo_TB"
# prefix_VWC = 'Soil_VWC_1_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'Soil_Ta_1_'
# suffix_TMP = '_Avg'

# filename = "1#_Daily_201405.dat"
# prefix_site = "PT1"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
#
# filename = "PT3_Daily.dat"
# prefix_site = "PT3"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
#
# filename = "PT4_Daily_2015_09_26_17_03_52.dat"
# prefix_site = "PT4"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
#
# filename = "PT5_Daily_2015_04_06_14_12_49.dat"
# prefix_site = "PT5"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'

# filename = paste(new_data_path,"new_",filename, sep = "")

filename = paste(sub_path, filename, sep = "")

# SETING XLIMIT and YLIMIT
xlim0 = c(-12,0)
ylim0 = c(0, 0.45)

# 只读取头文件（第二行）
headers  = read.csv(
  filename, header = 1, sep = ','
  , skip = 1, nrows = 1
)

# 只读取数据部分（第五行以后）
data     = read.csv(filename, skip = 4, sep = ',',header = 0)

date_TS  = as.Date(data[,1], "%Y-%m-%d") # 日期

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

# ====end===20151027====
# n_depth
for (i in 1:1) {
  #  对逐个深度进行计算
  
  onset = onset_final_month[i,1]   # 得到这个深度上的温度最高的月份，即开始月份
  final = onset_final_month[i,2]   # 得到结束月份
  
  # (n_years - 1)
  
  for (yr in 2:2) {
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
      
      # 设置拟合函数的数学形式
      
      #       f = function(x, theta) {
      #         theta[1] * abs(x) ^ theta[2] + theta[3] * abs(x) ^ (theta[4]-1)
      #       }
      
      f = function(x, theta) {
        theta[1] + (theta[2] - theta[1]) /
          ((1 + (theta[3] * abs(x)) ^ theta[4]) ^ (1 - 1 / theta[4]))
      }
      
      # 建立最小二乘误差函数
      
      ls_equation = function(theta, x0, y0) {
        sum((y0 - f(x0, theta)) ^ 2)
      }
      
      # 拟合结果
      
      theta0 <- c(1,1,10,1) # 初始值
      
      vwc_a_b_FR <-
        optim(
          theta0, ls_equation, x0 = TMP_FR, y0 = VWC_FR,
          control = list(maxit = 30000)
        )  # 最小化ls_equation
      
      coefs_FR = vwc_a_b_FR$par # 冻结期的估计结果
      
      theta0 <- c(1,1,10,1) # 初始值
      
      vwc_a_b_TH <-
        optim(
          theta0, ls_equation, x0 = TMP_TH, y0 = VWC_TH,
          control = list(maxit = 30000)
        )   # 最小化ls_equation
      
      coefs_TH = vwc_a_b_TH$par # 融化期的估计结果
      
      #  画图 ... ...
      
      #设置输出图片的文件名
      
      pdf_name = paste(
        figure_path, 'GH_',prefix_site,"_",all_years[yr],'_',
        same_depth[i],'cm',
        '.png',
        sep = ""
      )
      
      main_title = paste(prefix_site,"_",all_years[yr],"~",all_years[yr +
                                                                       1],':',same_depth[i],'cm') # 构造主标题
      
      data_fr <-
        data.frame(
          X1 = TMP_FR, Y11 = VWC_FR, Y12 = f(TMP_FR, vwc_a_b_FR$par)
        )  # 构造冻结期的数据框
      data_th <-
        data.frame(
          X2 = TMP_TH, Y21 = VWC_TH, Y22 = f(TMP_TH, vwc_a_b_TH$par)
        )  # 构造融化期的数据框
      
      p_fr <- ggplot(data = data_fr) +
        ylab(expression(
          paste("Volumetric Water Content (","m" ^ 3,"/m" ^ 3,")",sep = "")  # y标题
        )) +
        xlab(expression(paste("Soil Temperature (" ^ "o","C)",sep = ""))) +  # x标题
        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # y取值范围及标注的间隔
        scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)       # x取值范围及标注的间隔
      
      # 设置坐标轴的粗细、字体等
      
      ggplot2_set_axis(p_fr)
      
      last_plot() + geom_point(
        aes(x = X1, y = Y11, col = "Freezing")
        ,  size = 3, shape = 1, alpha = 0.5
      )  # 冻结期的散点图
      
      last_plot() + geom_line(aes(x = X1, y = Y12), col = "#006400",  size = 1)  # 冻结期的拟合曲线
      
      last_plot() + geom_point(
        data = data_th, aes(x = X2, y = Y21, col = "Thawing")
        ,  size = 3, shape = 1, alpha = 0.5
      ) # 融化期的散点图
      
      last_plot() + geom_line(data = data_th, aes(x = X2, y = Y22),  # 融化期的拟合曲线
                              col = "#8B0000",  size = 1)
      
      last_plot() + scale_colour_manual(values = c("#006400","#8B0000"))  # 设置绘图颜色， 注意与上面的对应起来
      
      last_plot() + theme(legend.title = element_blank()) +  # 去掉图例的标题
        theme(legend.text = element_text(size = 15))         # 设置图例的文字大小
      
      ## 给出相对位置
      x0 = 0.0
      y0 = 0.9
      ## 计算绝对位置
      x00 = (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
      y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
      y01 = (ylim0[2] - ylim0[1]) * (y0 - 0.16) + ylim0[1]
      
      last_plot() + annotate(
        "text", x = x00, y = y00, parse = T,
        label = paste(
          "F: UW==", sprintf("%.3f", coefs_FR[1]),
          "+","frac(",sprintf("%.3f", coefs_FR[2] - coefs_FR[1]),
          ",(1+(", sprintf("%.3f", coefs_FR[3]),"%*%",
          "abs(T)",
          ")^",sprintf("%.3f",coefs_FR[4]),")^",sprintf("%.3f",1 - 1 / coefs_FR[4]),
          ")",
          sep = ""
        ), color = "#006400", size = 5,
        family = "Helvetica", hjust = 0
      )
      
      last_plot() + annotate(
        "text", x = x00, y = y01, parse = T,
        label = paste(
          "T: UW==", sprintf("%.3f", coefs_TH[1]),
          "+","frac(",sprintf("%.3f", coefs_TH[2] - coefs_TH[1]),
          ",(1+(", sprintf("%.3f", coefs_TH[3]),"%*%",
          "abs(T)",
          ")^",sprintf("%.3f",coefs_TH[4]),")^",sprintf("%.3f",1 - 1 / coefs_TH[4]),
          ")",
          sep = ""
        ), color = "#8B0000", size = 5,
        family = "Helvetica", hjust = 0
      )
      
      #       last_plot() + geom_text(
      #         x = x00, y = y01,  # 标注估计的冻结期函数
      #         label = paste(
      #           "UWC==", sprintf("%.3f",coefs_FR[1]),
      #           "%.% T ^",
      #           sprintf("%.3f",coefs_FR[2]),
      #           sprintf("%+.3f",coefs_FR[3]),
      #           "%.% T ^",
      #           sprintf("%.3f",(coefs_FR[4] -
      #                             1)),
      #           sep = ""
      #         ),
      #         parse = T, color = "#006400", size = 5,
      #         family = "Helvetica", hjust = 0
      #       )
      
      last_plot() + ggtitle(main_title)      # 加个标题
      
      ggsave(
        pdf_name, width = 9, height = 5,limitsize = FALSE, dpi = 600  # 输出图片，尺寸9×5
      )
      
    }
  }
}