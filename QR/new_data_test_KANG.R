rm(list = ls())

library("lubridate") #�Ȱ�װ
library("ggplot2")   #�Ȱ�װ

# library("Hmisc")     #�Ȱ�װ

sub_path = "Raw_data/"
new_data_path = "New_data(short)/"
figure_path = "Figures_new/"
flag_added = "new_"

# filename = "EBo_TB_Daily.dat"
# prefix_site = "EBo_TB"
# prefix_VWC = 'Soil_VWC_1_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'Soil_Ta_1_'
# suffix_TMP = '_Avg'

filename = "1#_Daily_201405.dat"
prefix_site = "PT1"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg'
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
# filename = "PT5_Daily_2015_09_25_13_39_15.dat"
# prefix_site = "PT5"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'

filename = paste(new_data_path, "new_", filename, sep = "")

# SETING XLIMIT and YLIMIT
xlim0 = c(-12, 0)
ylim0 = c(0, 0.45)

# ֻ��ȡͷ�ļ����ڶ��У�
headers  = read.csv(
  filename,
  header = 1,
  sep = ','
  ,
  skip = 0,
  nrows = 1
)

# ֻ��ȡ���ݲ��֣��������Ժ�
data     = read.csv(filename,
                    skip = 1,
                    sep = ',',
                    header = 0)

date_TS  = as.Date(data[, 2], "%Y-%m-%d") # ����

allname_list = names(headers)

# �ҵ�ƽ��ˮ�ֵ�������

varname_vwc = names(headers)
varname_vwc <- varname_vwc[grepl(prefix_VWC, varname_vwc)]
varname_vwc <- varname_vwc[grepl(suffix_VWC, varname_vwc)]

varname_vwc <- substr(varname_vwc,
                      nchar(prefix_VWC) + 1,
                      nchar(varname_vwc) - nchar(suffix_VWC))

# �ҵ�ƽ���¶ȵ�������

varname_tmp = names(headers)
varname_tmp <- varname_tmp[grepl(prefix_TMP, varname_tmp)]
varname_tmp <- varname_tmp[grepl(suffix_TMP, varname_tmp)]

varname_tmp <- substr(varname_tmp,
                      nchar(prefix_TMP) + 1,
                      nchar(varname_tmp) - nchar(suffix_TMP))

depth_vwc = gsub('[^0-9]', '', varname_vwc) #��ȡ�����ֵ
depth_tmp = gsub('[^0-9]', '', varname_tmp) #��ȡ�����ֵ

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
  
  tmp_name = paste(prefix_TMP, same_depth[i], 'cm', suffix_TMP, sep = "")
  
  tmp_idx_in_data = grep(tmp_name, allname_list) #�ҵ���Ӧ�������
  
  TMP = data[, tmp_idx_in_data]
  
  yyyymm <-
    paste(format(as.POSIXlt(date_TS), format = "%Y-%m"), "01", sep = "-")  # ����ʱ�����У����е�day==01
  
  monthly_mean <- tapply(TMP, yyyymm, mean, simplify = T)   # ������ƽ��ֵ
  
  idx_max = which.max(monthly_mean)    # �ҵ����ֵ�����·�
  
  onset_final_month[i, 1] = month(temp_date[idx_max])
  onset_final_month[i, 2] = month(temp_date[idx_max]) - 1
}

# ====end===20151027====

for (i in 1:n_depth) {
  #  �������Ƚ��м���
  
  onset = onset_final_month[i, 1]   # �õ��������ϵ��¶���ߵ��·ݣ�����ʼ�·�
  final = onset_final_month[i, 2]   # �õ������·�
  
  # +++ 20160403 by K
  TMP_FR_ALL = NULL
  TMP_TH_ALL = NULL
  
  VWC_FR_ALL = NULL
  VWC_TH_ALL = NULL
  
  for (yr in 1:(n_years - 1)) {
    #  ����ݽ���ѭ������
    
    idx_year_part_1 = which(year_list == all_years[yr] &
                              month_list >= onset)            # �ҵ�ǰһ��İ�����ݵ�λ��
    idx_year_part_2 = which(year_list == all_years[yr + 1] &
                              month_list <= final)            # �ҵ���һ��İ�����ݵ�λ��
    idx_year        = c(idx_year_part_1, idx_year_part_2)     # ����������ƴ���������õ�һ���������λ��
    
    if (length(idx_year) >= 280) {
      # ���һ�������������һ�������������������Ͳ���������ļ���
      
      data_selected_year = data[idx_year, ] # ��ȡĳһ�����������
      
      # ��ȡ��Ӧ����ϵ�VWC��TMP
      
      vwc_name = paste(prefix_VWC, same_depth[i], 'cm', suffix_VWC, sep = "")
      tmp_name = paste(prefix_TMP, same_depth[i], 'cm', suffix_TMP, sep = "")
      
      vwc_idx_in_data = grep(vwc_name, allname_list) #�ҵ���Ӧ�������
      tmp_idx_in_data = grep(tmp_name, allname_list) #�ҵ���Ӧ�������
      
      VWC = data_selected_year[, vwc_idx_in_data]    # ��ȡ��ˮ������
      TMP = data_selected_year[, tmp_idx_in_data]    # ��ȡ�¶�����
      
      idx = which(TMP < -0.05)  # �ҵ�С��ĳһ�¶�ֵ������λ��
      
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
      
      # +++ 20150403 by K
      
      TMP_TH_ALL = c(TMP_TH_ALL, TMP_TH)
      TMP_FR_ALL = c(TMP_FR_ALL, TMP_FR)
      
      VWC_TH_ALL = c(VWC_TH_ALL, VWC_TH)
      VWC_FR_ALL = c(VWC_FR_ALL, VWC_FR)
      
    }
    
    # ������Ϻ�������ѧ��ʽ
    
    f = function(x, a, b) {
      # a * abs(x) ^ b
      a * abs(x) ^ b
    }
    
    # ��Ͻ�� Modified by K, 20160403
    
    vwc_a_b_FR <-
      nls(VWC_FR_ALL ~ f(TMP_FR_ALL, a, b), start = list(a = 1, b = 1))
    coefs_FR = coef(vwc_a_b_FR) # �����ڵĹ��ƽ��
    RSS.p.FR <- sum(residuals(vwc_a_b_FR) ^ 2)
    TSS.FR <- sum((VWC_FR_ALL - mean(VWC_FR_ALL)) ^ 2)
    R.FR <- 1 - (RSS.p.FR / TSS.FR)
    # �����ڵĹ��ƽ��
    
    vwc_a_b_TH <-
      nls(VWC_TH_ALL ~ f(TMP_TH_ALL, a, b), start = list(a = 1, b = 1))
    coefs_TH = coef(vwc_a_b_TH) # �ڻ��ڵĹ��ƽ��
    RSS.p.TH <- sum(residuals(vwc_a_b_TH) ^ 2)
    TSS.TH <- sum((VWC_TH_ALL - mean(VWC_TH_ALL)) ^ 2)
    R.TH <- 1 - (RSS.p.TH / TSS.TH)
    
    #  ��ͼ ... ...
    
    #�������ͼƬ���ļ��� Modified by K, 20160403
    
    pdf_name = paste(
      figure_path,
      'GH_',
      prefix_site,
      "_",
      same_depth[i],
      'cm',
      '.png',
      sep = ""
    )
    
    main_title = paste(prefix_site,
                       "_",
                       same_depth[i],
                       'cm') # ���������� Modified by K, 20160403
    
    data_fr <-
      data.frame(X1 = TMP_FR_ALL,
                 Y11 = VWC_FR_ALL,
                 Y12 = predict(vwc_a_b_FR))  # ���춳���ڵ����ݿ� Modified by K, 20160403
    data_th <-
      data.frame(X2 = TMP_TH_ALL,
                 Y21 = VWC_TH_ALL,
                 Y22 = predict(vwc_a_b_TH))  # �����ڻ��ڵ����ݿ� Modified by K, 20160403
    
    p_fr <- ggplot(data = data_fr) +
      ylab(expression(
        paste("Volumetric Water Content (", "m" ^ 3, "/m" ^ 3, ")", sep = "")  # y����
      )) +
      xlab(expression(paste("Soil Temperature (" ^ "o", "C)", sep = ""))) +  # x����
      scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # yȡֵ��Χ����ע�ļ��
      scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)       # xȡֵ��Χ����ע�ļ��
    
    # ����������Ĵ�ϸ�������
    p_fr  + theme(
      axis.text.x = element_text(
        #������X�������С����ɫ����ת�Ƕ�
        size = 18,
        color = "black",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5,
        angle = 0
      ),
      axis.text.y = element_text(
        #������Y�������С����ɫ����ת�Ƕ�
        size = 18,
        color = "black",
        face = "bold",
        vjust = 0.5,
        hjust = 0.5,
        angle = 0
      ),
      axis.title.x = element_text(
        #X����������С����ɫ����ת�Ƕ�
        size = 20,
        color = "black",
        face  = "bold",
        vjust = 0.5,
        hjust = 0.5,
        angle = 0
      ),
      
      axis.title.y = element_text(
        #Y����������С����ɫ����ת�Ƕ�
        size = 20,
        color = "black",
        face  = "bold",
        vjust = 0.5,
        hjust = 0.5,
        angle = 90
      )
    )
    
    last_plot() + geom_point(
      aes(x = X1, y = Y11, col = "Freezing")
      ,
      size = 2,
      shape = 16,
      alpha = 0.35
    )  # �����ڵ�ɢ��ͼ
    
    last_plot() + geom_line(aes(x = X1, y = Y12), col = "#006400",  size = 0.8)  # �����ڵ��������
    
    last_plot() + geom_point(
      data = data_th,
      aes(x = X2, y = Y21, col = "Thawing")
      ,
      size = 2,
      shape = 16,
      alpha = 0.35
    ) # �ڻ��ڵ�ɢ��ͼ
    
    last_plot() + geom_line(data = data_th,
                            aes(x = X2, y = Y22),
                            # �ڻ��ڵ��������
                            col = "#8B0000",
                            size = 0.8)
    
    last_plot() + scale_colour_manual(values = c("#006400", "#8B0000"))  # ���û�ͼ��ɫ�� ע��������Ķ�Ӧ����
    
    last_plot() + theme(legend.title = element_blank()) +  # ȥ��ͼ���ı���
      theme(legend.text = element_text(size = 15)) + # ����ͼ�������ִ�С
      theme(legend.justification = 'right',
            legend.position = c(0.9, 0.8))
    #         theme(legend.justification = c(0,1)) +
    #       theme(legend.position = right )
    ## �������λ��
    x0 = 0.0
    y0 = 0.9
    ## �������λ��
    x00 = (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
    y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
    y01 = (ylim0[2] - ylim0[1]) * (y0 - 0.085) + ylim0[1]
    
    x1 = 0.25
    ## �������λ��
    x10 = (xlim0[2] - xlim0[1]) * x1 + xlim0[1]
    
    last_plot() + geom_text(
      x = x00,
      y = y00,
      # ��ע���Ƶ��ڻ��ں���
      label = paste(
        "Wu==",
        sprintf("%.3f", coefs_TH["a"]),
        "%.% theta ^",
        sprintf("%.3f", coefs_TH["b"]),
        sep = ""
      ),
      parse = T,
      color = "#8B0000",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    
    last_plot() + geom_text(
      x = x10,
      y = y00,
      # ��עR
      label = paste("R^2==", sprintf("% .2f", R.TH),
                    sep = ""),
      parse = T,
      color = "#8B0000",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    last_plot() + geom_text(
      x = x00,
      y = y01,
      # ��ע���ƵĶ����ں���
      label = paste(
        "Wu==",
        sprintf("%.3f", coefs_FR["a"]),
        "%.% theta ^",
        sprintf("%.3f", coefs_FR["b"]),
        sep = ""
      ),
      parse = T,
      color = "#006400",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    
    last_plot() + geom_text(
      x = x10,
      y = y01,
      # ��עR
      label = paste("R^2==", sprintf("% .2f", R.FR),
                    sep = ""),
      parse = T,
      color = "#006400",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    
    last_plot() + geom_text(
      x = x00,
      y = y00,
      # ��ע���Ƶ��ڻ��ں���
      label = paste(
        "Wu==",
        sprintf("%.3f", coefs_TH["a"]),
        "%.% theta ^",
        sprintf("%.3f", coefs_TH["b"]),
        sep = ""
      ),
      parse = T,
      color = "#8B0000",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    
    last_plot() + geom_text(
      x = x00,
      y = y01,
      # ��ע���ƵĶ����ں���
      label = paste(
        "Wu==",
        sprintf("%.3f", coefs_FR["a"]),
        "%.% theta ^",
        sprintf("%.3f", coefs_FR["b"]),
        sep = ""
      ),
      parse = T,
      color = "#006400",
      size = 5,
      family = "Helvetica",
      hjust = 0
    )
    #
    last_plot() + ggtitle(main_title)      # �Ӹ�����
    
    ggsave(
      pdf_name,
      width = 9,
      height = 5,
      limitsize = FALSE,
      dpi = 600  # ���ͼƬ���ߴ�9��5
    )
    
  }
}