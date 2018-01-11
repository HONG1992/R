rm(list = ls())

library("lubridate") #?Ȱ?װ
library("ggplot2")   #?Ȱ?װ
library("MASS")   #?Ȱ?װ
library("grid")
# library("Hmisc")     #?Ȱ?װ

#============== Deal with original data and combine all data in frozen & thawed period ============

sub_path = "Raw_data/"
new_data_path = "New_data(short)/"
figure_path = "Figures_new_test/"
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


# 1st col = site name
# 2nd col = depth
# 3rd col = soil temperature during thawed period
# 4th col = vol. water content during thawed period

data_th <-
  data.frame(
    Borehole = SITE_TH_ALL, Depth = DEPTH_TH_ALL, TMP_TH = TMP_TH_ALL, VWC_TH = VWC_TH_ALL)  # ?????ڻ??ڵ????ݿ?

#============== Deal with soil texture ============

# Read soil texture table :

Soil_Properties = read.csv('Soil_Properties.csv', skip = 0, sep = ',',header = TRUE)
texture_List = Soil_Properties$Soil.Texture.Classification
ph_List = Soil_Properties$ph
ec_List = Soil_Properties$ec
orp_List = Soil_Properties$orp
gc_List = Soil_Properties$gc
bd_List = Soil_Properties$bd
tvwc_List = Soil_Properties$tvwc

all_texture_types  = unique(Soil_Properties$Soil.Texture.Classification) 
all_ph_types  = unique(Soil_Properties$ph) 
all_ec_types  = unique(Soil_Properties$ec) 
all_orp_types  = unique(Soil_Properties$orp) 
all_gc_types  = unique(Soil_Properties$gc) 
all_bd_types  = unique(Soil_Properties$bd) 
all_tvwc_types  = unique(Soil_Properties$tvwc) 

me<-length(all_tvwc_types)
sd<-length(all_tvwc_types)

for (i in 1:length(all_tvwc_types)) { # for each type ===
  data_fr_tmp_type = NULL
  data_fr_vwc_type = NULL 
  data_th_tmp_type = NULL
  data_th_vwc_type = NULL
  
  
  type = all_tvwc_types[i] # one of soil texture type
  idx = grep(type, Soil_Properties$tvwc) # obtain the corresponding borehole & depth

  for (ii in 1:length(idx)){ # obtain all data for the same soil type ===
    
    idx_in_data_fr = grep(paste(Soil_Properties$Borehole[idx[ii]], Soil_Properties$Depth[idx[ii]], sep = '_'),
                          paste(data_fr$Borehole, data_fr$Depth, sep = '_'));
    data_fr_tmp_type = c(data_fr_tmp_type, data_fr$TMP_FR[idx_in_data_fr])
    data_fr_vwc_type = c(data_fr_vwc_type, data_fr$VWC_FR[idx_in_data_fr])
    

    idx_in_data_th = grep(paste(Soil_Properties$Borehole[idx[ii]], Soil_Properties$Depth[idx[ii]], sep = '_'),
                          paste(data_th$Borehole, data_th$Depth, sep = '_'));
    data_th_tmp_type = c(data_th_tmp_type, data_th$TMP_TH[idx_in_data_th])
    data_th_vwc_type = c(data_th_vwc_type, data_th$VWC_TH[idx_in_data_th])
  }

  me[i]<-mean(c(data_th_vwc_type,data_fr_vwc_type))
  sd[i]<-sd(c(data_th_vwc_type,data_fr_vwc_type))
}

  # the following is traditional process in previous programs === 
 

  t1=all_tvwc_types
  t2=me
  t3=sd

  data_new <- data.frame(t1,t2,t3)

  


  P <-  ggplot(data=data_new,aes(x=t1,y=t2,ymin=t2-t3,ymax=t2+t3)) +  #errorbar
               ylab(expression(paste("Volumetric Water Content (", "m" ^ 3, "/m" ^ 3, ")", sep = ""))) +  # y????
               xlab(expression(paste("Soil TOTAL WATER ", sep = ""))) + 
               geom_point(size =2,alpha = 0.5) +
               geom_errorbar(width =0.02,alpha = 0.5) 
                          # scale_colour_manual(values = c("#000080", "#FF0000")) # ???û?ͼ??ɫ?? ע?????????Ķ?Ӧ??��

    last_plot() +
    theme(legend.title = element_blank()) +  # ȥ??ͼ???ı???
    theme(legend.text = element_text(size = 20)) + # ????ͼ???????ִ?С
    theme(legend.justification = 'right',legend.position = c(0.9, 0.8))+
    theme(legend.key.size=unit(1.5,'cm')) 
  
  

  
  pdf_name = paste(figure_path,'GH_','TVWC','.png',sep = "" )
  
  ggsave(
    pdf_name,
    width = 9,
    height = 5,
    limitsize = FALSE,
    dpi = 600  # ????ͼƬ???ߴ?9??5
  )

