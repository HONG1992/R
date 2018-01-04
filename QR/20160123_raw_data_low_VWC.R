rm(list = ls())

library("lubridate") #�Ȱ�װ
library("ggplot2")   #�Ȱ�װ
# library("Hmisc")     #�Ȱ�װ

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
  
  # ֻ��ȡͷ�ļ����ڶ��У�
  headers  = read.csv(
    filename, header = 1, sep = ','
    , skip = 1, nrows = 1
  )
  
  # ֻ��ȡ���ݲ��֣��������Ժ�
  data     = read.csv(filename, skip = 4, sep = ',',header = 0)
  
  date_TS  = as.Date(data[,1], "%Y-%m-%d") # ����
  
  allname_list = names(headers)
  
  # �ҵ�ƽ��ˮ�ֵ�������
  
  varname_vwc = names(headers)
  varname_vwc <- varname_vwc[grepl(prefix_VWC,varname_vwc)]
  varname_vwc <- varname_vwc[grepl(suffix_VWC,varname_vwc)]
  
  varname_vwc <- substr(varname_vwc, nchar(prefix_VWC) + 1,
                        nchar(varname_vwc) - nchar(suffix_VWC))
  
  # �ҵ�ƽ���¶ȵ�������
  
  varname_tmp = names(headers)
  varname_tmp <- varname_tmp[grepl(prefix_TMP,varname_tmp)]
  varname_tmp <- varname_tmp[grepl(suffix_TMP,varname_tmp)]
  
  varname_tmp <- substr(varname_tmp, nchar(prefix_TMP) + 1,
                        nchar(varname_tmp) - nchar(suffix_TMP))
  
  depth_vwc = gsub('[^0-9]','',varname_vwc) #��ȡ�����ֵ
  depth_tmp = gsub('[^0-9]','',varname_tmp) #��ȡ�����ֵ
  
  same_depth = intersect(depth_vwc, depth_tmp) #��ȡ��ͬ�۲����
  
  n_depth = length(same_depth) #��ȡ��ͬ�۲���ȵ�����
  
  all_years  = unique(year(date_TS)) # ��ȡ�������
  n_years    = length(all_years) # ��ȡ�������
  year_list  = year(date_TS)
  month_list = month(date_TS)
  
  # ��ȡÿ������ϵ���ʼ�·� +++ 2015/10/27
  #   1��������ƽ��ֵ
  #   2���ҵ����ֵ
  #   3��������12����
  
  onset_final_month = matrix(data = NA, nrow = n_depth, ncol = 2)
  
  temp_date <- seq.Date(from = date_TS[1],
                        to = date_TS[length(date_TS)],
                        by = "month") #�����·�����
  
  for (i in 1:n_depth) {
    #������Ͻ���ѭ������
    
    tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
    
    tmp_idx_in_data = grep(tmp_name,allname_list) #�ҵ���Ӧ�������
    
    TMP = data[,tmp_idx_in_data]
    
    yyyymm <-
      paste(format(as.POSIXlt(date_TS), format = "%Y-%m"), "01", sep = "-")  # ����ʱ�����У����е�day==01
    
    monthly_mean <-
      tapply(TMP, yyyymm, mean, simplify = T)   # ������ƽ��ֵ
    
    idx_max = which.max(monthly_mean)    # �ҵ����ֵ�����·�
    
    onset_final_month[i,1] = month(temp_date[idx_max])
    onset_final_month[i,2] = month(temp_date[idx_max]) - 1
  }
  
  # ====end===20151027====
  
  # for (i in 1:n_depth) { #  �������Ƚ��м���
  
  for (i in 1:n_depth) { #  �������Ƚ��м���
    
    onset = onset_final_month[i,1]   # �õ��������ϵ��¶���ߵ��·ݣ�����ʼ�·�
    final = onset_final_month[i,2]   # �õ������·�
    
    for (yr in 1:(n_years - 1)) { #  ����ݽ���ѭ������
      # for (yr in 1:(n_years - 1)) {  #  ����ݽ���ѭ������
      
      idx_year_part_1 = which(year_list == all_years[yr] &
                                month_list >= onset)            # �ҵ�ǰһ��İ�����ݵ�λ��
      idx_year_part_2 = which(year_list == all_years[yr + 1] &
                                month_list <= final)            # �ҵ���һ��İ�����ݵ�λ��
      idx_year        = c(idx_year_part_1, idx_year_part_2)     # ����������ƴ���������õ�һ���������λ��
      
      if (length(idx_year) >= 200) {
        # ���һ�������������һ�������������������Ͳ���������ļ���
        
        data_selected_year = data[idx_year,] # ��ȡĳһ�����������
        
        # ��ȡ��Ӧ����ϵ�VWC��TMP
        
        vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
        tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
        
        vwc_idx_in_data = grep(vwc_name,allname_list) #�ҵ���Ӧ�������
        tmp_idx_in_data = grep(tmp_name,allname_list) #�ҵ���Ӧ�������
        
        VWC = data_selected_year[,vwc_idx_in_data]    # ��ȡ��ˮ������
        TMP = data_selected_year[,tmp_idx_in_data]    # ��ȡ�¶�����
        
        idx = which(TMP < -0.)  # �ҵ�С��ĳһ�¶�ֵ������λ��
        
        VWC = VWC[idx]            # ɸѡ ��ˮ��
        TMP = TMP[idx]            # ɸѡ �¶�
        
        n_points = length(TMP)
        
        min_TMP = which.min(TMP) # �ҵ�����¶�����λ��
        
        TMP_FR  = TMP[1:min_TMP] # �������¶�
        VWC_FR  = VWC[1:min_TMP] # ������ˮ��
        
        TMP_TH  = TMP[(min_TMP + 1):n_points] # �ڻ����¶�
        VWC_TH  = VWC[(min_TMP + 1):n_points] # �ڻ���ˮ��
        
        b_FR = sort(TMP_FR, index.return = TRUE) # �Զ����ڰ��¶Ƚ�������
        TMP_FR = TMP_FR[b_FR$ix]
        VWC_FR = VWC_FR[b_FR$ix]
        
        b_TH = sort(TMP_TH, index.return = TRUE) # ���ڻ��ڰ��¶Ƚ�������
        TMP_TH = TMP_TH[b_TH$ix]
        VWC_TH = VWC_TH[b_TH$ix]
        
        # ++++ 2016-01-20 :
        
        print(search_break_point(TMP_FR, VWC_FR))
        print(search_break_point(TMP_TH, VWC_TH))
        
        bp_fr = TMP_FR[search_break_point(TMP_FR, VWC_FR)]  # ��ȡͻ���
        bp_th = TMP_TH[search_break_point(TMP_TH, VWC_TH)]  # ��ȡͻ���

        vwc_bp_fr = NaN # ������̣�ƽ���׶ε�ƽ��ֵ
        vwc_bp_th = NaN # �ڻ����̣�ƽ���׶ε�ƽ��ֵ
        trd_dp_fr = NaN # ������̣����ͽ׶ε�б��
        trd_dp_th = NaN # �ڻ����̣����ͽ׶ε�б��
        
        if (length(bp_fr) != 0) {
          
          idx_bp_fr = which(TMP_FR <= bp_fr)    # �����ڣ�ͻ����¶����µ��������ݵ�λ��
          vwc_bp_fr = mean(VWC_FR[idx_bp_fr], na.rm = T) # �����ڣ�ͻ����¶����µ����к�ˮ����ƽ��ֵ
          
          idx_bp_fr_2 = which(TMP_FR > bp_fr)    # �����ڣ�ͻ����¶����ϵ��������ݵ�λ��
          trd_bp_fr_model = lm(VWC_FR[idx_bp_fr_2] ~ TMP_FR[idx_bp_fr_2]) # �������Է���
          
          trd_dp_fr = summary.lm(trd_bp_fr_model)$coefficients[2,1] # ���б��
          
        } else {
          bp_fr = NaN
        }
        
        if (length(bp_th) != 0) {
          idx_bp_th = which(TMP_TH <= bp_th)    # �ڻ��ڣ�ͻ����¶����µ��������ݵ�λ��
          vwc_bp_th = mean(VWC_TH[idx_bp_th], na.rm = T) # �ڻ��ڣ�ͻ����¶����µ����к�ˮ����ƽ��ֵ
          
          idx_bp_th_2 = which(TMP_TH > bp_th)    # �ڻ��ڣ�ͻ����¶����ϵ��������ݵ�λ��
          trd_bp_th_model = lm(VWC_TH[idx_bp_th_2] ~ TMP_TH[idx_bp_th_2]) # �������Է���
          
          trd_dp_th = summary.lm(trd_bp_th_model)$coefficients[2,1] # ���б��
          
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

# ƴ�����ݿ�

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
) # д�����н��