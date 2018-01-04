par(new=T)


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
#   filename = "PT3_Daily.dat"
#   prefix_site = "PT3"
#   prefix_VWC = 'VWC_'
#   suffix_VWC = '_Avg'
#   prefix_TMP = 'ST_'
#   suffix_TMP = '_Avg'
#
filename = "PT4_Daily_2015_09_26_17_03_52.dat"
prefix_site = "PT4"
prefix_VWC = 'VWC_'
suffix_VWC = '_Avg'
prefix_TMP = 'ST_'
suffix_TMP = '_Avg'
#
# filename = "PT5_Daily_2015_09_25_13_39_15.dat"
# prefix_site = "PT5"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'

# filename = "DTXJZhongCR1000_LL_SFG_Min30_2015_09_25_12_20_01.dat"
# prefix_site = "SFGT"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg'
#
# filename = "DTXJShangCR1000_Min30_2015_09_25_12_28_57.dat"
# prefix_site = "PT10"
# prefix_VWC = 'VWC_'
# suffix_VWC = '_Avg'
# prefix_TMP = 'ST_'
# suffix_TMP = '_Avg' 

filename = paste(new_data_path,"new_",filename, sep = "")

# SETING XLIMIT and YLIMIT
xlim0 = c(-12,0)
ylim0 = c(0, 0.45)

# 只读取头文件（第二行）
headers  = read.csv(
  filename, header = 1, sep = ','
  , skip = 0, nrows = 1
)

# 只读取数据部分（第五行以后）
data     = read.csv(filename, skip = 4, sep = ',',header = 0)

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

for (i in 1:n_depth) { #对深度上进行循环计算
  
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

#  对逐个深度进行计算


onset = floor(mean(onset_final_month[,1]))  # 得到所有深度上的温度最高的月份，即开始月份
final = floor(mean(onset_final_month[,2]))  # 得到结束月份

for (yr in 1:(n_years - 1))  {  #  对年份进行循环计算
  
  idx_year_part_1 = which(year_list == all_years[yr] &
                            month_list >= onset)            # 找到前一年的半截数据的位置
  idx_year_part_2 = which(year_list == all_years[yr + 1] &
                            month_list <= final)            # 找到后一年的半截数据的位置
  idx_year        = c(idx_year_part_1, idx_year_part_2)     # 把上面两个拼接起来，得到一整年的数据位置
  
  if (length(idx_year) >= 280) {         # 如果一年的数据量满足一定条件，则继续，否则就不进行下面的计算
    
    data_selected_year = data[idx_year,] # 提取某一年的所有数据
    VWC<-vector(mode="numeric",length=0) # VWC和TMP都是向量
    TMP<-vector(mode="numeric",length=0)
    x<-numeric(length(idx_year))
    y<-numeric(length(idx_year))
    w<-numeric(length(idx_year))
    z<-numeric(length(idx_year))
    u<-numeric(length(idx_year))
    VWC1<-data.frame(a=x,b=y,c=w,d=z,e=u)
    TMP1<-data.frame(a=x,b=y,c=w,d=z,e=u)
    emin_TMP <-vector(mode="numeric",length=5)
    
    # 获取相应深度上的VWC和TMP
    for(i in 1:4){    
      
      vwc_name = paste(prefix_VWC, same_depth[i],'cm',suffix_VWC,sep = "")
      tmp_name = paste(prefix_TMP, same_depth[i],'cm',suffix_TMP,sep = "")
      
      vwc_idx_in_data = grep(vwc_name,allname_list) #找到对应的列序号
      tmp_idx_in_data = grep(tmp_name,allname_list) #找到对应的列序号
      
      VWC1[,i]= data_selected_year[,vwc_idx_in_data]    # 提取含水量数据
      TMP1[,i]= data_selected_year[,tmp_idx_in_data]    # 提取温度数据
      
      emin_TMP[i] = which.min(TMP1[,i])     #不同层的温度最低点的位置
      
      TMP1[,i]<-TMP1[,i]*(TMP1[,i]<(-0.05))   #选取温度和水分都满足条件的数据，即温度小于-0.05℃的温度和水分
      VWC1[,i]<-VWC1[,i]*(TMP1[,i]<(-0.05))
      
      TMP1[TMP1==0]<-NA
      VWC1[VWC1==0]<-NA
      
      # PT1
      #         VWC_FR <-c(VWC1[1:emin_TMP[1],1],VWC1[1:emin_TMP[2],2],VWC1[1:emin_TMP[3],3])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[1],1],TMP1[1:emin_TMP[2],2],TMP1[1:emin_TMP[3],3])
      #         TMP_FR <-na.omit(TMP_FR)
      #         
      #         VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1],VWC1[(emin_TMP[2]+1):365,2],VWC1[(emin_TMP[3]+1):365,3])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1],TMP1[(emin_TMP[2]+1):365,2],TMP1[(emin_TMP[3]+1):365,3])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #PT3
      #This is two layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[1],1],VWC1[1:emin_TMP[3],3])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[1],1],TMP1[1:emin_TMP[3],3])
      #         TMP_FR <-na.omit(TMP_FR)
      #         
      #         VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1],VWC1[(emin_TMP[3]+1):365,3])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1],TMP1[(emin_TMP[3]+1):365,3])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #This is one layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[2],2])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[2],2])
      #         TMP_FR <-na.omit(TMP_FR)
      #                 
      #         VWC_TH <-c(VWC1[(emin_TMP[2]+1):365,2])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[2]+1):365,2])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #PT4
      #This is two layer
      #                 VWC_FR <-c(VWC1[1:emin_TMP[1],1],VWC1[1:emin_TMP[3],3])
      #                 VWC_FR <-na.omit(VWC_FR)
      #                 TMP_FR <-c(TMP1[1:emin_TMP[1],1],TMP1[1:emin_TMP[3],3])
      #                 TMP_FR <-na.omit(TMP_FR)
      #                                 
      #                 VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1],VWC1[(emin_TMP[3]+1):365,3])
      #                 VWC_TH <-na.omit(VWC_TH)
      #                 TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1],TMP1[(emin_TMP[3]+1):365,3])
      #                 TMP_TH <-na.omit(TMP_TH)
      
      #This is one layer
      VWC_FR <-c(VWC1[1:emin_TMP[2],2])
      VWC_FR <-na.omit(VWC_FR)
      TMP_FR <-c(TMP1[1:emin_TMP[2],2])
      TMP_FR <-na.omit(TMP_FR)
      
      VWC_TH <-c(VWC1[(emin_TMP[2]+1):365,2])
      VWC_TH <-na.omit(VWC_TH)
      TMP_TH <-c(TMP1[(emin_TMP[2]+1):365,2])
      TMP_TH <-na.omit(TMP_TH)
      
      #This is one layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[4],4])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[4],4])
      #         TMP_FR <-na.omit(TMP_FR)
      #                         
      #         VWC_TH <-c(VWC1[(emin_TMP[4]+1):365,4])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[4]+1):365,4])
      #         TMP_TH <-na.omit(TMP_TH)
      
      
      #PT5
      #This is two layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[3],3],VWC1[1:emin_TMP[5],5])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[3],3],TMP1[1:emin_TMP[5],5])
      #         TMP_FR <-na.omit(TMP_FR)
      #                                 
      #         VWC_TH <-c(VWC1[(emin_TMP[3]+1):365,3],VWC1[(emin_TMP[5]+1):365,5])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[3]+1):365,3],TMP1[(emin_TMP[5]+1):365,5])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #This is two layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[1],1],VWC1[1:emin_TMP[2],2])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[1],1],TMP1[1:emin_TMP[2],2])
      #         TMP_FR <-na.omit(TMP_FR)
      #                                 
      #         VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1],VWC1[(emin_TMP[2]+1):365,2])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1],TMP1[(emin_TMP[2]+1):365,2])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #This is one layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[4],4])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[4],4])
      #         TMP_FR <-na.omit(TMP_FR)
      #                                 
      #         VWC_TH <-c(VWC1[(emin_TMP[4]+1):365,4])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[4]+1):365,4])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #SFGT
      #This is two layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[2],1],VWC1[1:emin_TMP[4],4])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[2],1],TMP1[1:emin_TMP[4],4])
      #         TMP_FR <-na.omit(TMP_FR)
      #                                 
      #         VWC_TH <-c(VWC1[(emin_TMP[2]+1):365,2],VWC1[(emin_TMP[4]+1):365,4])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[2]+1):365,2],TMP1[(emin_TMP[4]+1):365,4])
      #         TMP_TH <-na.omit(TMP_TH)
      
      #This is one layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[3],3])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[3],3])
      #         TMP_FR <-na.omit(TMP_FR)
      #         
      #         VWC_TH <-c(VWC1[(emin_TMP[3]+1):365,3])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[3]+1):365,3])
      #         TMP_TH <-na.omit(TMP_TH)
      #         
      #This is one layer
      #         VWC_FR <-c(VWC1[1:emin_TMP[1],1])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[1],1])
      #         TMP_FR <-na.omit(TMP_FR)
      #         
      #         VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1])
      #         TMP_TH <-na.omit(TMP_TH)      
      
      #         VWC_FR <-c(VWC1[1:emin_TMP[1],1],VWC1[1:emin_TMP[2],2],VWC1[1:emin_TMP[3],3],VWC1[1:emin_TMP[4],4])
      #         VWC_FR <-na.omit(VWC_FR)
      #         TMP_FR <-c(TMP1[1:emin_TMP[1],1],TMP1[1:emin_TMP[2],2],TMP1[1:emin_TMP[3],3],TMP1[1:emin_TMP[4],4])
      #         TMP_FR <-na.omit(TMP_FR)
      #         
      #         VWC_TH <-c(VWC1[(emin_TMP[1]+1):365,1],VWC1[(emin_TMP[2]+1):365,2],VWC1[(emin_TMP[3]+1):365,3],VWC1[(emin_TMP[4]+1):365,4])
      #         VWC_TH <-na.omit(VWC_TH)
      #         TMP_TH <-c(TMP1[(emin_TMP[1]+1):365,1],TMP1[(emin_TMP[2]+1):365,2],TMP1[(emin_TMP[3]+1):365,3],TMP1[(emin_TMP[4]+1):365,4])
      #         TMP_TH <-na.omit(TMP_TH)
      # idx=which(is.na(TMP1[,i])==F)
      
      
      b_FR = sort(TMP_FR, index.return = TRUE) # 对冻结期按温度进行排序
      TMP_FR = TMP_FR[b_FR$ix]
      VWC_FR = VWC_FR[b_FR$ix]
      
      b_TH = sort(TMP_TH, index.return = TRUE) # 对融化期按温度进行排序
      TMP_TH = TMP_TH[b_TH$ix]
      VWC_TH = VWC_TH[b_TH$ix]
      
      # 设置拟合函数的数学形式
      
      f = function(x, a, b) {
        # a * abs(x) ^ b
        a * abs(x) ^ b
      }
      
      # 拟合结果
      
      vwc_a_b_FR <-
        nls(VWC_FR ~ f(TMP_FR, a, b), start = list(a = 1, b = 1))
      
      coefs_FR = coef(vwc_a_b_FR) # 冻结期的估计结果
      
      vwc_a_b_TH <-
        nls(VWC_TH ~ f(TMP_TH, a, b), start = list(a = 1, b = 1))
      
      coefs_TH = coef(vwc_a_b_TH) # 融化期的估计结果
      #  画图 ... ...
      
      #设置输出图片的文件名
      
      
      data_fr <-
        data.frame(X1 = TMP_FR, Y11 = VWC_FR, Y12 = predict(vwc_a_b_FR))  # 构造冻结期的数据框
      data_th <-
        data.frame(X2 = TMP_TH, Y21 = VWC_TH, Y22 = predict(vwc_a_b_TH))  # 构造融化期的数据框
      
      p_fr <- ggplot(data = data_fr) +
        ylab(expression(
          paste("Volumetric Water Content (","m" ^ 3,"/m" ^ 3,")",sep = "")  # y标题
        )) +
        xlab(expression(paste("Soil Temperature (" ^ "o","C)",sep = ""))) +  # x标题
        scale_y_continuous(breaks = seq(0, 1, 0.1), limits = ylim0) +     # y取值范围及标注的间隔
        scale_x_continuous(breaks = seq(-50, 0, 1), limits = xlim0)       # x取值范围及标注的间隔
      
      # 设置坐标轴的粗细、字体等 
      p_fr  + theme(
        axis.text.x = element_text(  #坐标轴X的字体大小、颜色、旋转角度
          size = 15,
          color = "black",
          face = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        axis.text.y = element_text(  #坐标轴Y的字体大小、颜色、旋转角度
          size = 15,
          color = "black",
          face = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        axis.title.x = element_text( #X标题的字体大小、颜色、旋转角度
          size = 16,
          color = "black",
          face  = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 0
        ),
        
        axis.title.y = element_text( #Y标题的字体大小、颜色、旋转角度
          size = 16,
          color = "black",
          face  = "bold",
          vjust = 0.5,
          hjust = 0.5,
          angle = 90
        )
      )
      
      last_plot() + geom_point(aes(x = X1, y = Y11, col = "Freezing")
                               ,  size = 2, shape = 16, alpha = 0.5 )  # 冻结期的散点图
      
      last_plot() + geom_line(aes(x = X1, y = Y12), col = "#006400",  size = 1)  # 冻结期的拟合曲线
      
      last_plot() + geom_point(data = data_th, aes(x = X2, y = Y21, col = "Thawing")
                               ,  size = 2, shape = 16, alpha = 0.5 ) # 融化期的散点图
      
      last_plot() + geom_line(data = data_th, aes(x = X2, y = Y22),  # 融化期的拟合曲线
                              col = "#8B0000",  size = 1)
      
      last_plot() + scale_colour_manual(values = c("#006400","#8B0000"))  # 设置绘图颜色， 注意与上面的对应起来
      
      last_plot() + theme(legend.title = element_blank()) +  # 去掉图例的标题
        theme(legend.text = element_text(size = 15)) +        # 设置图例的文字大小
        theme(legend.justification = 'right', legend.position=c(0.9,0.8))
      ## 给出相对位置
      x0 = 0.0
      y0 = 0.9
      ## 计算绝对位置
      x00 = (xlim0[2] - xlim0[1]) * x0 + xlim0[1]
      y00 = (ylim0[2] - ylim0[1]) * y0 + ylim0[1]
      y01 = (ylim0[2] - ylim0[1]) * (y0 -0.085) + ylim0[1]
      
      last_plot() +geom_text(x=x00, y=y00,   # 标注估计的融化期函数
                             label= paste("Wu==", sprintf("%.3f",coefs_TH["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_TH["b"]),
                                          sep = ""), 
                             parse=T, color="#8B0000", size=5, 
                             family="Helvetica", hjust = 0)
      
      last_plot() +geom_text(x=x00, y=y01,  # 标注估计的冻结期函数
                             label= paste("Wu==", sprintf("%.3f",coefs_FR["a"]), 
                                          "%.% theta ^",
                                          sprintf("%.3f",coefs_FR["b"]),
                                          sep = ""), 
                             parse=T, color="#006400", size=5, 
                             family="Helvetica", hjust = 0)
      
      last_plot() + ggtitle(main_title)      # 加个标题
      
      
      ggsave(
        pdf_name, width = 9, height = 5,limitsize = FALSE, dpi = 600  # 输出图片，尺寸9×5
      )
    } 
  }
}
