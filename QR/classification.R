rm(list = ls())

library("lubridate") #?Ȱ?װ
library("ggplot2")   #?Ȱ?װ
library("MASS")   #?Ȱ?װ
library("grid")
# library("Hmisc")     #?Ȱ?װ

#============== Deal with original data and combine all data in frozen & thawed period ============

sub_path = "Raw_data/"
new_data_path = "New_data(short)/"
figure_path = "Figures_new/"
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
  
  # ֻ??ȡͷ?ļ????ڶ??У?
  headers  = read.csv(
    filename, header = 1, sep = ','
    , skip = 0, nrows = 1
  )
  
  # ֻ??ȡ???ݲ??֣????????Ժ???
  data     = read.csv(filename, skip = 1, sep = ',',header = 0)
  
  date_TS  = as.Date(data[,2], "%Y-%m-%d") # ????
  
  allname_list = names(headers)
  
  # ?ҵ?ƽ??ˮ?ֵ???????
  
  varname_vwc = names(headers)
  varname_vwc <- varname_vwc[grepl(prefix_VWC,varname_vwc)]
  varname_vwc <- varname_vwc[grepl(suffix_VWC,varname_vwc)]
  
  varname_vwc <- substr(varname_vwc, nchar(prefix_VWC) + 1,
                        nchar(varname_vwc) - nchar(suffix_VWC))
  
  # ?ҵ?ƽ???¶ȵ???????
  
  varname_tmp = names(headers)
  varname_tmp <- varname_tmp[grepl(prefix_TMP,varname_tmp)]
  varname_tmp <- varname_tmp[grepl(suffix_TMP,varname_tmp)]
  
  varname_tmp <- substr(varname_tmp, nchar(prefix_TMP) + 1,
                        nchar(varname_tmp) - nchar(suffix_TMP))
  
  depth_vwc = gsub('[^0-9]','',varname_vwc) #??ȡ??????ֵ
  depth_tmp = gsub('[^0-9]','',varname_tmp) #??ȡ??????ֵ
  
  same_depth = intersect(depth_vwc, depth_tmp) #??ȡ??ͬ?۲?????
  
  n_depth = length(same_depth) #??ȡ??ͬ?۲????ȵ???��
  
  all_years  = unique(year(date_TS)) # ??ȡ????????
  n_years    = length(all_years) # ??ȡ??????��
  year_list  = year(date_TS)
  month_list = month(date_TS)
  
  # ??ȡÿ???????ϵ???ʼ?·? +++ 2015/10/27
  #   1????????ƽ??ֵ
  #   2???ҵ?????ֵ
  #   3????????12????
  
  onset_final_month = matrix(data = NA, nrow = n_depth, ncol = 2)
  
  temp_date <- seq.Date(from = date_TS[1],
                        to = date_TS[length(date_TS)],
                        by = "month") #?????·?????
  
  for (i in 1:n_depth) {
    #???????Ͻ???ѭ??????
    
    tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
    
    tmp_idx_in_data = grep(tmp_name,allname_list) #?ҵ???Ӧ????????
    
    TMP = data[,tmp_idx_in_data]
    
    yyyymm <-
      paste(format(as.POSIXlt(date_TS), format = "%Y-%m"), "01", sep = "-")  # ????ʱ?????У????е?day==01
    
    monthly_mean <- tapply(TMP, yyyymm, mean, simplify = T)   # ??????ƽ??ֵ
    
    idx_max = which.max(monthly_mean)    # ?ҵ?????ֵ?????·?
    
    onset_final_month[i,1] = month(temp_date[idx_max])
    onset_final_month[i,2] = month(temp_date[idx_max]) - 1
  }
  
  for (i in 1:n_depth) {
    
    #  ?????????Ƚ??м???
    
    onset = onset_final_month[i,1]   # ?õ??????????ϵ??¶????ߵ??·ݣ?????ʼ?·?
    final = onset_final_month[i,2]   # ?õ??????·?
    
    # (n_years - 1)
    
    for (yr in 1:(n_years - 1)) {
      
      #  ?????ݽ???ѭ??????
      
      idx_year_part_1 = which(year_list == all_years[yr] &
                                month_list >= onset)            # ?ҵ?ǰһ???İ??????ݵ?λ??
      idx_year_part_2 = which(year_list == all_years[yr + 1] &
                                month_list <= final)            # ?ҵ???һ???İ??????ݵ?λ??
      idx_year        = c(idx_year_part_1, idx_year_part_2)     # ??????��??ƴ????��???õ?һ??????????λ??
      
      if (length(idx_year) >= 280) {
        
        # ????һ????????��????һ?????????????????????Ͳ??????????ļ???
        
        data_selected_year = data[idx_year,] # ??ȡĳһ????????????
        
        # ??ȡ??Ӧ?????ϵ?VWC??TMP
        
        vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
        tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
        
        vwc_idx_in_data = grep(vwc_name,allname_list) #?ҵ???Ӧ????????
        tmp_idx_in_data = grep(tmp_name,allname_list) #?ҵ???Ӧ????????
        
        VWC = data_selected_year[,vwc_idx_in_data]    # ??ȡ??ˮ��????
        TMP = data_selected_year[,tmp_idx_in_data]    # ??ȡ?¶?????
        
        idx = which(TMP < -0.025)  # ?ҵ?С??ĳһ?¶?ֵ??????λ??
        
        VWC = VWC[idx]            # ɸѡ ??ˮ��
        TMP = TMP[idx]            # ɸѡ ?¶?
        
        n_points = length(TMP)
        
        min_TMP = which.min(TMP) # ?ҵ??????¶?????λ??
        
        TMP_FR  = TMP[1:min_TMP] # ???????¶?
        VWC_FR  = VWC[1:min_TMP] # ??????ˮ??
        
        TMP_TH  = TMP[(min_TMP + 1):n_points] # ?ڻ????¶?
        VWC_TH  = VWC[(min_TMP + 1):n_points] # ?ڻ???ˮ??
        
        b_FR = sort(TMP_FR, index.return = TRUE) # ?Զ????ڰ??¶Ƚ???????
        TMP_FR = TMP_FR[b_FR$ix]
        VWC_FR = VWC_FR[b_FR$ix]
        
        b_TH = sort(TMP_TH, index.return = TRUE) # ???ڻ??ڰ??¶Ƚ???????
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

# ???????????????õ??ı?��ƴ????�� ===
# 1st col = site name
# 2nd col = depth
# 3rd col = soil temperature during frozen period
# 4th col = vol. water content during frozen period

data_fr <-
  data.frame(
    Borehole = SITE_FR_ALL, Depth = DEPTH_FR_ALL, TMP_FR = TMP_FR_ALL, VWC_FR = VWC_FR_ALL)  # ???춳???ڵ????ݿ?

# ???????????????õ??ı?��ƴ????�� ===
# 1st col = site name
# 2nd col = depth
# 3rd col = soil temperature during thawed period
# 4th col = vol. water content during thawed period

data_th <-
  data.frame(
    Borehole = SITE_TH_ALL, Depth = DEPTH_TH_ALL, TMP_TH = TMP_TH_ALL, VWC_TH = VWC_TH_ALL)  # ?????ڻ??ڵ????ݿ?

#============== Deal with soil texture ============

# Read soil texture table :

Soil_Texture = read.csv('Soil_Texture.csv', skip = 0, sep = ',',header = TRUE)
Texture_List = Soil_Texture$Soil.Texture.Classification
all_types  = unique(Soil_Texture$Soil.Texture.Classification) # ??ȡ????Soil types

for (i in 1:length(all_types)) { # for each type ===
  # for (i in 3:3){   #plot_test
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
  
  # ???????Ϻ???????ѧ??ʽ
  
  f = function(x, a, b) {
    # a * abs(x) ^ b
    a * abs(x) ^ b
  }
  
  # ???Ͻ??? Modified by K, 20160403
  
  vwc_a_b_FR <-
    nls(data_fr_vwc_soil_texture ~ f(data_fr_tmp_soil_texture, a, b), start = list(a = 1, b = 1))
  coefs_FR = coef(vwc_a_b_FR) # ?????ڵĹ��ƽ???
  # confint_FR = confint(vwc_a_b_FR,level = 0.99)
  RSS.p.FR <- sum(residuals(vwc_a_b_FR) ^ 2)
  TSS.FR <- sum((data_fr_vwc_soil_texture - mean(data_fr_vwc_soil_texture)) ^ 2)
  R.FR <- 1 - (RSS.p.FR / TSS.FR)
  
  vwc_a_b_TH <-
    nls(data_th_vwc_soil_texture ~ f(data_th_tmp_soil_texture, a, b), start = list(a = 1, b = 1))
  coefs_TH = coef(vwc_a_b_TH) # ?ڻ??ڵĹ��ƽ???
  # confint_TH = confint(vwc_a_b_TH,level = 0.99)
  RSS.p.TH <- sum(residuals(vwc_a_b_TH) ^ 2)
  TSS.TH <- sum((data_th_vwc_soil_texture - mean(data_th_vwc_soil_texture)) ^ 2)
  R.TH <- 1 - (RSS.p.TH / TSS.TH)

  #  ??ͼ ... ...
  
  #????????ͼƬ???ļ??? Modified by K, 20160404
  
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
  
  main_title = paste(type)# ?????????? Modified by K, 20160404
  
  data_fr_1 <-
    data.frame(X1 = data_fr_tmp_soil_texture,
               Y11 = data_fr_vwc_soil_texture,
               Y12 = predict(vwc_a_b_FR))  # ???춳???ڵ????ݿ? Modified by K, 20160404
  
  
  
  
  data_th_1 <-
    data.frame(X2 = data_th_tmp_soil_texture,
               Y21 = data_th_vwc_soil_texture,
               Y22 = predict(vwc_a_b_TH))  # ?????ڻ??ڵ????ݿ? Modified by K, 20160404
  
  
  
  
  
  P_X = c(data_fr_1$X1,data_th_1$X2)
  P_Y = c(data_fr_1$Y11,data_th_1$Y21)
  P_Y1 = c(data_fr_1$Y12,data_th_1$Y22)  
  P_FR <- paste('Freezing',character(length(data_fr_1[,1])), sep = "")
  P_TH <- paste('Thawing',character(length(data_th_1[,1])), sep = "")
  P_C = c(P_FR, P_TH)

  data1 <- data.frame(x = P_X, y = P_Y, period = P_C )
  data2 <- data.frame(x = P_X, y = P_Y1, period = P_C )
  
  p <- ggplot(data=data1,aes(x = x, y = y,color=period,shape=period)) +
    ylab(expression(paste("Volumetric Water Content (", "m" ^ 3, "/m" ^ 3, ")", sep = ""))) +  # y????
    xlab(expression(paste("Soil Temperature (" ^ "o", "C)", sep = ""))) +  # x????
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # yȡֵ??Χ????ע?ļ???
    scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)  +     # xȡֵ??Χ????ע?ļ???
    # geom_point(size =2.5,alpha = 0.25) +
    scale_colour_manual(values = c("#000080", "#FF0000"))   # ???û?ͼ??ɫ?? ע?????????Ķ?Ӧ??��
    # scale_shape_manual(values = c(3, 4))
#   
  last_plot() + geom_line(data=data2,aes(x = x, y = y ,color= period),size=1,show_guide  = FALSE) 
  
  # ???????????Ĵ?ϸ????????
  last_plot() + theme(
    axis.text.x = element_text(
      #??????X????????С????ɫ????ת?Ƕ?
      size = 18,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    axis.text.y = element_text(
      #??????Y????????С????ɫ????ת?Ƕ?
      size = 18,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    axis.title.x = element_text(
      #X????????????С????ɫ????ת?Ƕ?
      size = 20,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    
    axis.title.y = element_text(
      #Y????????????С????ɫ????ת?Ƕ?
      size = 20,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 90
    )
  )
  
  last_plot() +
    theme(legend.title = element_blank()) +  # ȥ??ͼ???ı???
    theme(legend.text = element_text(size = 20)) + # ????ͼ???????ִ?С
    theme(legend.justification = 'right',legend.position = c(0.9, 0.8))+
    theme(legend.key.size=unit(1.5,'cm')) 
  
  
  
  
  ## ????????λ??
  x0 = 0.0
  y0 = 0.9
  ## ????????λ??
  x00 = (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
  y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
  y01 = (ylim0[2] - ylim0[1]) * (y0 - 0.085) + ylim0[1]
  
  x1 = 0.35
  ## ????????λ??
  x10 = (xlim0[2] - xlim0[1]) * x1 + xlim0[1]
  
  last_plot() + geom_text(
    x = x00,
    y = y00,
    # ??ע?��ƵĶ????ں???
    label = paste(
      "Wu==",
      sprintf("%.3f", coefs_FR["a"]),
      "%.% theta ^",
      sprintf("%.3f", coefs_FR["b"]),
      sep = ""
    ),
    parse = T,
    color = "#000080",
    size = 6.5,
    family = "Helvetica",
    hjust = 0
  )
  
  last_plot() + geom_text(
    x = x10,
    y = y00,
    # ??עR
    label = paste("R^2==", sprintf("% .2f", R.FR),
                  sep = ""),
    parse = T,
    color = "#000080",
    size = 6.5,
    family = "Helvetica",
    hjust = 0
  )
  
  last_plot() + geom_text(
    x = x00,
    y = y01,
    # ??ע?��Ƶ??ڻ??ں???
    label = paste(
      "Wu==",
      sprintf("%.3f", coefs_TH["a"]),
      "%.% theta ^",
      sprintf("%.3f", coefs_TH["b"]),
      sep = ""
    ),
    parse = T,
    color = "#FF0000",
    size = 6.5,
    family = "Helvetica",
    hjust = 0
  )
  
  last_plot() + geom_text(
    x = x10,
    y = y01,
    # ??עR
    label = paste("R^2==", sprintf("% .2f", R.TH),
                  sep = ""),
    parse = T,
    color = "#FF0000",
    size = 6.5,
    family = "Helvetica",
    hjust = 0
  )
  
  last_plot() + ggtitle(main_title)  +    # ?Ӹ?????
    theme(plot.title = element_text(size=22))
  
  ggsave(
    pdf_name,
    width = 9,
    height = 5,
    limitsize = FALSE,
    dpi = 600  # ????ͼƬ???ߴ?9??5
  )
}