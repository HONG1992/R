rm(list = ls())

library("lubridate") #�Ȱ�װ
library("ggplot2")   #�Ȱ�װ
library("spuRs")     #�Ȱ�װ
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
  
  monthly_mean <- tapply(TMP, yyyymm, mean, simplify = T)   # ������ƽ��ֵ
  
  idx_max = which.max(monthly_mean)    # �ҵ����ֵ�����·�
  
  onset_final_month[i,1] = month(temp_date[idx_max])
  onset_final_month[i,2] = month(temp_date[idx_max]) - 1
}

# ====end===20151027====
# n_depth
for (i in 1:1) {
  #  �������Ƚ��м���
  
  onset = onset_final_month[i,1]   # �õ��������ϵ��¶���ߵ��·ݣ�����ʼ�·�
  final = onset_final_month[i,2]   # �õ������·�
  
  # (n_years - 1)
  
  for (yr in 2:2) {
    #  ����ݽ���ѭ������
    
    idx_year_part_1 = which(year_list == all_years[yr] &
                              month_list >= onset)            # �ҵ�ǰһ��İ�����ݵ�λ��
    idx_year_part_2 = which(year_list == all_years[yr + 1] &
                              month_list <= final)            # �ҵ���һ��İ�����ݵ�λ��
    idx_year        = c(idx_year_part_1, idx_year_part_2)     # ����������ƴ���������õ�һ���������λ��
    
    if (length(idx_year) >= 280) {
      # ���һ�������������һ�������������������Ͳ���������ļ���
      
      data_selected_year = data[idx_year,] # ��ȡĳһ�����������
      
      # ��ȡ��Ӧ����ϵ�VWC��TMP
      
      vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
      tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
      
      vwc_idx_in_data = grep(vwc_name,allname_list) #�ҵ���Ӧ�������
      tmp_idx_in_data = grep(tmp_name,allname_list) #�ҵ���Ӧ�������
      
      VWC = data_selected_year[,vwc_idx_in_data]    # ��ȡ��ˮ������
      TMP = data_selected_year[,tmp_idx_in_data]    # ��ȡ�¶�����
      
      idx = which(TMP < -0.025)  # �ҵ�С��ĳһ�¶�ֵ������λ��
      
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
      
      # ������Ϻ�������ѧ��ʽ
      
      #       f = function(x, theta) {
      #         theta[1] * abs(x) ^ theta[2] + theta[3] * abs(x) ^ (theta[4]-1)
      #       }
      
      f = function(x, theta) {
        theta[1] + (theta[2] - theta[1]) /
          ((1 + (theta[3] * abs(x)) ^ theta[4]) ^ (1 - 1 / theta[4]))
      }
      
      # ������С��������
      
      ls_equation = function(theta, x0, y0) {
        sum((y0 - f(x0, theta)) ^ 2)
      }
      
      # ��Ͻ��
      
      theta0 <- c(1,1,10,1) # ��ʼֵ
      
      vwc_a_b_FR <-
        optim(
          theta0, ls_equation, x0 = TMP_FR, y0 = VWC_FR,
          control = list(maxit = 30000)
        )  # ��С��ls_equation
      
      coefs_FR = vwc_a_b_FR$par # �����ڵĹ��ƽ��
      
      theta0 <- c(1,1,10,1) # ��ʼֵ
      
      vwc_a_b_TH <-
        optim(
          theta0, ls_equation, x0 = TMP_TH, y0 = VWC_TH,
          control = list(maxit = 30000)
        )   # ��С��ls_equation
      
      coefs_TH = vwc_a_b_TH$par # �ڻ��ڵĹ��ƽ��
      
      #  ��ͼ ... ...
      
      #�������ͼƬ���ļ���
      
      pdf_name = paste(
        figure_path, 'GH_',prefix_site,"_",all_years[yr],'_',
        same_depth[i],'cm',
        '.png',
        sep = ""
      )
      
      main_title = paste(prefix_site,"_",all_years[yr],"~",all_years[yr +
                                                                       1],':',same_depth[i],'cm') # ����������
      
      data_fr <-
        data.frame(
          X1 = TMP_FR, Y11 = VWC_FR, Y12 = f(TMP_FR, vwc_a_b_FR$par)
        )  # ���춳���ڵ����ݿ�
      data_th <-
        data.frame(
          X2 = TMP_TH, Y21 = VWC_TH, Y22 = f(TMP_TH, vwc_a_b_TH$par)
        )  # �����ڻ��ڵ����ݿ�
      
      p_fr <- ggplot(data = data_fr) +
        ylab(expression(
          paste("Volumetric Water Content (","m" ^ 3,"/m" ^ 3,")",sep = "")  # y����
        )) +
        xlab(expression(paste("Soil Temperature (" ^ "o","C)",sep = ""))) +  # x����
        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # yȡֵ��Χ����ע�ļ��
        scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)       # xȡֵ��Χ����ע�ļ��
      
      # ����������Ĵ�ϸ�������
      
      ggplot2_set_axis(p_fr)
      
      last_plot() + geom_point(
        aes(x = X1, y = Y11, col = "Freezing")
        ,  size = 3, shape = 1, alpha = 0.5
      )  # �����ڵ�ɢ��ͼ
      
      last_plot() + geom_line(aes(x = X1, y = Y12), col = "#006400",  size = 1)  # �����ڵ��������
      
      last_plot() + geom_point(
        data = data_th, aes(x = X2, y = Y21, col = "Thawing")
        ,  size = 3, shape = 1, alpha = 0.5
      ) # �ڻ��ڵ�ɢ��ͼ
      
      last_plot() + geom_line(data = data_th, aes(x = X2, y = Y22),  # �ڻ��ڵ��������
                              col = "#8B0000",  size = 1)
      
      last_plot() + scale_colour_manual(values = c("#006400","#8B0000"))  # ���û�ͼ��ɫ�� ע��������Ķ�Ӧ����
      
      last_plot() + theme(legend.title = element_blank()) +  # ȥ��ͼ���ı���
        theme(legend.text = element_text(size = 15))         # ����ͼ�������ִ�С
      
      ## �������λ��
      x0 = 0.0
      y0 = 0.9
      ## �������λ��
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
      #         x = x00, y = y01,  # ��ע���ƵĶ����ں���
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
      
      last_plot() + ggtitle(main_title)      # �Ӹ�����
      
      ggsave(
        pdf_name, width = 9, height = 5,limitsize = FALSE, dpi = 600  # ���ͼƬ���ߴ�9��5
      )
      
    }
  }
}