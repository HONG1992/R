rm(list = ls())

library("lubridate") #先安装
library("ggplot2")   #先安装
# library("Hmisc")     #先安装

# ++++ 2016-01-20 :
source("function_20160123.R")

sub_path = "Raw_data/"
# new_data_path = "New_data/"
figure_path = "Figures/"
# flag_added = "new_"

# ++++ 2016-01-20 :

list_filename    = c(
  "1#_Daily_201405.dat","PT3_Daily.dat",
  "PT4_Daily_2015_09_26_17_03_52.dat",
  "PT5_Daily_2015_09_25_13_39_15.dat"
)
# "DTXJShangCR1000_Min30_2015_09_25_12_28_57.dat",
# "DTXJZhongCR1000_LL_SFG_Min30_2015_09_25_12_20_01.dat"
list_prefix_site = c( "PT1", "PT3", "PT4", "PT5")
list_prefix_VWC  = c('VWC_','VWC_','VWC_','VWC_')
list_suffix_VWC  = c('_Avg','_Avg','_Avg','_Avg')
list_prefix_TMP  = c('ST_','ST_','ST_','ST_')
list_suffix_TMP  = c('_Avg','_Avg','_Avg','_Avg')

output_data_col_01 = c()
output_data_col_02 = c()
output_data_col_03 = c()
output_data_col_04 = c()
output_data_col_05 = c()
output_data_col_06 = c()
output_data_col_07 = c()
output_data_col_08 = c()
output_data_col_09 = c()

# end +++++

for (i_filename in 1:5) {
  filename = list_filename[i_filename]
  prefix_site = list_prefix_site[i_filename]
  prefix_VWC = list_prefix_VWC[i_filename]
  suffix_VWC = list_suffix_VWC[i_filename]
  prefix_TMP = list_prefix_TMP[i_filename]
  suffix_TMP = list_suffix_TMP[i_filename]
  
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
    
    monthly_mean <-
      tapply(TMP, yyyymm, mean, simplify = T)   # 计算月平均值
    
    idx_max = which.max(monthly_mean)    # 找到最大值所在月份
    
    onset_final_month[i,1] = month(temp_date[idx_max])
    onset_final_month[i,2] = month(temp_date[idx_max]) - 1
  }
  
  # ====end===20151027====
  
  # for (i in 1:n_depth) { #  对逐个深度进行计算
  
  for (i in 1:n_depth) { #  对逐个深度进行计算
    
    onset = onset_final_month[i,1]   # 得到这个深度上的温度最高的月份，即开始月份
    final = onset_final_month[i,2]   # 得到结束月份
    
    for (yr in 1:(n_years - 1)) { #  对年份进行循环计算
      # for (yr in 1:(n_years - 1)) {  #  对年份进行循环计算
      
      idx_year_part_1 = which(year_list == all_years[yr] &
                                month_list >= onset)            # 找到前一年的半截数据的位置
      idx_year_part_2 = which(year_list == all_years[yr + 1] &
                                month_list <= final)            # 找到后一年的半截数据的位置
      idx_year        = c(idx_year_part_1, idx_year_part_2)     # 把上面两个拼接起来，得到一整年的数据位置
      
      if (length(idx_year) >= 200) {
        # 如果一年的数据量满足一定条件，则继续，否则就不进行下面的计算
        
        data_selected_year = data[idx_year,] # 提取某一年的所有数据
        
        # 获取相应深度上的VWC和TMP
        
        vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
        tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
        
        vwc_idx_in_data = grep(vwc_name,allname_list) #找到对应的列序号
        tmp_idx_in_data = grep(tmp_name,allname_list) #找到对应的列序号
        
        VWC = data_selected_year[,vwc_idx_in_data]    # 提取含水量数据
        TMP = data_selected_year[,tmp_idx_in_data]    # 提取温度数据
        
        idx = which(TMP < -0.)  # 找到小于某一温度值的所有位置
        
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
        
        # ++++ 2016-01-20 :
        
        print(search_break_point(TMP_FR, VWC_FR))
        print(search_break_point(TMP_TH, VWC_TH))
        
        bp_fr = TMP_FR[search_break_point(TMP_FR, VWC_FR)]  # 获取突变点
        bp_th = TMP_TH[search_break_point(TMP_TH, VWC_TH)]  # 获取突变点

        vwc_bp_fr = NaN # 冻结过程，平缓阶段的平均值
        vwc_bp_th = NaN # 融化过程，平缓阶段的平均值
        trd_dp_fr = NaN # 冻结过程，陡峭阶段的斜率
        trd_dp_th = NaN # 融化过程，陡峭阶段的斜率
        
        if (length(bp_fr) != 0) {
          
          idx_bp_fr = which(TMP_FR <= bp_fr)    # 冻结期：突变点温度以下的所有数据的位置
          vwc_bp_fr = mean(VWC_FR[idx_bp_fr], na.rm = T) # 冻结期：突变点温度以下的所有含水量的平均值
          
          idx_bp_fr_2 = which(TMP_FR > bp_fr)    # 冻结期：突变点温度以上的所有数据的位置
          trd_bp_fr_model = lm(VWC_FR[idx_bp_fr_2] ~ TMP_FR[idx_bp_fr_2]) # 估计线性方程
          
          trd_dp_fr = summary.lm(trd_bp_fr_model)$coefficients[2,1] # 提出斜率
          
        } else {
          bp_fr = NaN
        }
        
        if (length(bp_th) != 0) {
          idx_bp_th = which(TMP_TH <= bp_th)    # 融化期：突变点温度以下的所有数据的位置
          vwc_bp_th = mean(VWC_TH[idx_bp_th], na.rm = T) # 融化期：突变点温度以下的所有含水量的平均值
          
          idx_bp_th_2 = which(TMP_TH > bp_th)    # 融化期：突变点温度以上的所有数据的位置
          trd_bp_th_model = lm(VWC_TH[idx_bp_th_2] ~ TMP_TH[idx_bp_th_2]) # 估计线性方程
          
          trd_dp_th = summary.lm(trd_bp_th_model)$coefficients[2,1] # 提出斜率
          
        } else {
          bp_th = NaN
        }
        
        output_data_col_01 = c(output_data_col_01, prefix_site)   # Borehold ID
        output_data_col_02 = c(output_data_col_02, same_depth[i]) # Depth
        output_data_col_03 = c(output_data_col_03, all_years[yr]) # Year
        
        output_data_col_04 = c(output_data_col_04, bp_fr)         # Break Point during Freezing
        output_data_col_05 = c(output_data_col_05, vwc_bp_fr)     # mean VWC 1
        output_data_col_06 = c(output_data_col_06, trd_dp_fr)     # Trend VWC vs TEMP 1
        
        output_data_col_07 = c(output_data_col_07, bp_th)         # Break Point during Thawing
        output_data_col_08 = c(output_data_col_08, vwc_bp_th)     # mean VWC 2
        output_data_col_09 = c(output_data_col_09, trd_dp_th)     # Trend VWC vs TEMP 2
        
      }
    }
  }
}

# 拼接数据框

output_bp = data.frame(
  bh_id = output_data_col_01,
  depth_id = output_data_col_02,
  year_id  = output_data_col_03,
  tmp_bp_fr = output_data_col_04,
  vwc_bp_fr = output_data_col_05,
  trd_bp_fr = output_data_col_06,
  tmp_bp_th = output_data_col_07,
  vwc_bp_th = output_data_col_08,
  trd_bp_th = output_data_col_09
)

write.csv(
  output_bp, file = paste("All_VWC_Results.csv", sep = ''),
  row.names = F, quote = F
) # 写出所有结果