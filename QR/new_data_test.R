rm(list = ls())

library("lubridate") 
library("ggplot2")   
library("grid")

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
# 
# filename = "1#_Daily_201405.dat"
# prefix_site = "PT1"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
# 
filename = "PT3_Daily.dat"
prefix_site = "PT3"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg'
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

filename = paste(new_data_path,"new_",filename, sep = "")

# SETING XLIMIT and YLIMIT
xlim0 = c(-12,0)
ylim0 = c(0, 0.45)


headers  = read.csv(
  filename, header = 1, sep = ','
  , skip = 0, nrows = 1
)


data     = read.csv(filename, skip = 1, sep = ',',header = 0)

date_TS  = as.Date(data[,2], "%Y-%m-%d") 
allname_list = names(headers)



varname_vwc = names(headers)
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
all_years  = unique(year(date_TS)) #n_years    = length(all_years) #ear_list  = year(date_TS)
month_list = month(date_TS)

# oset_final_month = matrix(data = NA, nrow = n_depth, ncol = 2)

temp_date <- seq.Date(from = date_TS[1],
                      to = date_TS[length(date_TS)],
                      by = "month") #


for (i in 1:n_depth) { #?
  
  tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
  
  tmp_idx_in_data = grep(tmp_name,allname_list) 
  
  TMP = data[,tmp_idx_in_data]
  
  yyyymm <-
    paste(format(as.POSIXlt(date_TS), format = "%Y-%m"), "01", sep = "-")  
#  monthly_mean <- tapply(TMP, yyyymm, mean, simplify = T)   # 
  idx_max = which.max(monthly_mean)     
  onset_final_month[i,1] = month(temp_date[idx_max])
  onset_final_month[i,2] = month(temp_date[idx_max]) - 1
}

# ====end===20151027====
pdf("myplot.pdf",height = 8, width =6)
grid.newpage
pushViewport(viewport(layout=grid.layout(4,2)))
matrixindex = cbind(rep(1:n_depth,each =(n_years - 1)), rep(1:(n_years - 1),times=n_depth))
 

for (i in 1:n_depth) { # 
  onset = onset_final_month[i,1]   #  final = onset_final_month[i,2]   #  
  for (yr in 1:(n_years - 1)) {  #   
    idx_year_part_1 = which(year_list == all_years[yr] &
                              month_list >= onset)               idx_year_part_2 = which(year_list == all_years[yr + 1] &
                              month_list <= final)            #   idx_year        = c(idx_year_part_1, idx_year_part_2)     # 
    if (length(idx_year) >= 280) {         # ? 
      data_selected_year = data[idx_year,] # ??ȡ  
      #  
      vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
      tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
      
      vwc_idx_in_data = grep(vwc_name,allname_list) #?ҵ??  tmp_idx_in_data = grep(tmp_name,allname_list) #?ҵ??  
      VWC = data_selected_year[,vwc_idx_in_data]    # ?? TMP = data_selected_year[,tmp_idx_in_data]    # ??ȡ 
      idx = which(TMP < -0.05)  # ?ҵ?С 
      VWC = VWC[idx]            # ɸѡ ?TMP = TMP[idx]            # ɸѡ 
      n_points = length(TMP)
      
      min_TMP = which.min(TMP) # ?ҵ???
      TMP_FR  = TMP[1:min_TMP] # ?????VWC_FR  = VWC[1:min_TMP] # ????
      TMP_TH  = TMP[(min_TMP + 1):n_points] # ?ڻ???VWC_TH  = VWC[(min_TMP + 1):n_points] # ?ڻ???
      b_FR = sort(TMP_FR, index.return = TRUE) # ?Զ???TMP_FR = TMP_FR[b_FR$ix]
      VWC_FR = VWC_FR[b_FR$ix]
      
      b_TH = sort(TMP_TH, index.return = TRUE) # ???ڻ?TMP_TH = TMP_TH[b_TH$ix]
      VWC_TH = VWC_TH[b_TH$ix]
      
      # ????
      f = function(x, a, b) {
        # a * abs(x) ^ b
        a * abs(x) ^ b
      }
      
      # ?
      vwc_a_b_FR <-
        nls(VWC_FR ~ f(TMP_FR, a, b), start = list(a = 1, b = 1))
      coefs_FR = coef(vwc_a_b_FR) # ????SS.p.FR <- sum(residuals(vwc_a_b_FR)^2)
      TSS.FR <- sum((VWC_FR- mean(VWC_FR))^2) 
      R.FR<-1-(RSS.p.FR/TSS.FR)
      # ????     vwc_a_b_TH <-
        nls(VWC_TH ~ f(TMP_TH, a, b), start = list(a = 1, b = 1))
      coefs_TH = coef(vwc_a_b_TH) # ?ڻ??ڵĹ.p.TH <- sum(residuals(vwc_a_b_TH)^2)
      TSS.TH <- sum((VWC_TH- mean(VWC_TH))^2)  
      R.TH<-1-(RSS.p.TH/TSS.TH)
      
      #   ??????ͼ_name = paste(figure_path, 'GH_',prefix_site,"_",all_years[yr],'_',
                       same_depth[i],'cm',
                       '.png',
                       sep = "")
      
      main_title = paste(prefix_site,"_",all_years[yr],"~",all_years[yr+1],':',same_depth[i],'cm') # ????????    data_fr <-
        data.frame(X1 = TMP_FR, Y11 = VWC_FR, Y12 = predict(vwc_a_b_FR))  # ???춳???ڵa_th <-
        data.frame(X2 = TMP_TH, Y21 = VWC_TH, Y22 = predict(vwc_a_b_TH))  # ?????ڻ?    p_fr <- ggplot(data = data_fr) +
        ylab(expression(
          paste("Volumetric Water Content (","m" ^ 3,"/m" ^ 3,")",sep = "")  # y????
  ) +
        xlab(expression(paste("Soil Temperature (" ^ "o","C)",sep = ""))) +  # x????
  cale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # yȡֵ??Χ??cale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)       # xȡֵ??Χ?    # ??????r  + theme(
        axis.text.x = element_text(  #??????X?? size = 10,
          color = "black",
          face = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        axis.text.y = element_text(  #??????Y?? size = 10,
          color = "black",
          face = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        axis.title.x = element_text( #X???????? size = 10,
          color = "black",
          face  = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        
        axis.title.y = element_text( #Y???????? size = 10,
          color = "black",
        , face  = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 90
        )
      )
      
      last_plot() + geom_point(aes(x = X1, y = Y11, col = "Freezing")
                               ,  size = 2, shape = 16, alpha = 0.35 )  # ?????ڵ?ɢ    last_plot() + geom_line(aes(x = X1, y = Y12), col = "#006400",  size = 0.8)  # ?????ڵ?    last_plot ) + geom_point(data = data_th, aes(x = X2, y = Y21, col = "Thawing")
                               ,  size = 2, shape = 16, alpha = 0.35 ) # ?ڻ??ڵ?ɢ?    last_plot() + geom_line(data = data_th, aes(x = X2, y = Y22),  # ?ڻ??ڵ??                     col = "#8B0000",  size = 0.8)
      
      last_plot() + scale_colour_manual(values = c("#006400","#8B0000"))  # ???û?ͼ??   last_plot() + theme(legend.title = element_blank()) +  # ȥ??ͼ???ı?eme(legend.text = element_text(size = 10)) + # ????ͼ????eme(legend.justification = 'right', legend.position=c(0.9,0.8))
      #         theme(legend.justification = c(0,1)) +
      #       theme(legend.position = right )
      ## ?? 0.0
      y0 = 0.9
      ## ??= (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
      y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
      y01 = (ylim0[2] - ylim0[1]) * (y0 -0.085) + ylim0[1]
      
      x1 = 0.25
      ## ??= (xlim0[2] - xlim0[1]) * x1 + xlim0[1]
      
      last_plot() +geom_text(x=x00, y=y00,   # ??ע?��Ƶ??ڻ                  label= paste("Wu==", sprintf("%.3f",coefs_TH["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_TH["b"]),
                                          sep = ""), 
                             parse=T, color="#8B0000", size=3, 
                             family="Helvetica",  hjust = 0)
      
      last_plot() +geom_text(x=x10, y=y00,   # ??עR
                       label= paste("R^2==",sprintf("% .2f",R.TH),
                                          sep = ""), 
                             parse=T, color="#8B0000", size=3, 
                             family="Helvetica",  hjust = 0)
      last_plot() +geom_text(x=x00, y=y01,  # ??ע?��ƵĶ??                 label= paste("Wu==", sprintf("%.3f",coefs_FR["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_FR["b"]),
                                          sep = ""), 
                             parse=T, color="#006400", size=3, 
                             family="Helvetica", hjust = 0)
      
      last_plot() +geom_text(x=x10, y=y01,   # ??עR
                       label= paste("R^2==",sprintf("% .2f",R.FR),
                                          sep = ""), 
                             parse=T, color="#006400", size=3, 
                             family="Helvetica", hjust = 0)
      
      last_plot() +geom_text(x=x00, y=y00,   # ??ע?��Ƶ??ڻ??                label= paste("Wu==", sprintf("%.3f",coefs_TH["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_TH["b"]),
                                          sep = ""), 
                             parse=T, color="#8B0000", size=3, 
                             family="Helvetica",  hjust = 0)
      
      last_plot() +geom_text(x=x00, y=y01,  # ??ע?��ƵĶ????ں               label= paste("Wu==", sprintf("%.3f",coefs_FR["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_FR["b"]),
                                          sep = ""), 
                             parse=T, color="#006400", size=3, 
                             family="Helvetica", hjust = 0)
      #       
     last <- last_plot() + ggtitle(main_title)      # ?Ӹ?????
    gsave(
        pdf_name, width = 6, height = 3,limitsize = FALSE, dpi = 600  # ????ͼƬ???ߴ?9?   }
    # print(last,vp=viewport(layout.pos.row=matrixindex[i,1]))
  }
  # print(last,vp=viewport(layout.pos.col=matrixindex[yr,2]))
}
dev.off

