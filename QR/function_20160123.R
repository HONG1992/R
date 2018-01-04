Mann_Kendall <- function(timeserial) {
  Mann_Kendall_sub <-
    function(timeserial) {
      r <- c()
      s <- c()
      U <- c()
      
      for (i in 2:length(timeserial))
      {
        r[i] <- 0
        
        for (j in 1:i)
        {
          if (timeserial[i] > timeserial[j]) {
            r[i] = r[i] + 1
          }
        }
        
        
        s[i] <- 0
        for (ii in 2:i) {
          s[i] <- s[i] + r[ii]
        }
        
        
        U[i] <- 0
        U[i] <-
          (s[i] - (i * (i - 1) / 4)) / sqrt(i * (i - 1) * (2 * i + 5) / 72)
        
      }
      
      r[1] <- 0
      s[1] <- 0
      U[1] <- 0
      
      LST <- list(r = r, s = s, U = U)
      
      return (LST)
    }
  
  timeserial_rev <- rev(timeserial)
  
  y1 <- Mann_Kendall_sub(timeserial)
  y2 <- Mann_Kendall_sub(timeserial_rev)
  
  y2$U <- -(rev(y2$U))
  
  LST <- list(UF = y1,UB = y2)
  return(LST)
}

# SET AXIS for Handle h

ggplot2_set_axis <- function(h) {
  h + theme(
    axis.text.x = element_text(
      size = 15,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 15
    ),
    axis.text.y = element_text(
      size = 15,
      color = "black",
      face = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    axis.title.x = element_text(
      size = 16,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 0
    ),
    
    axis.title.y = element_text(
      size = 16,
      color = "black",
      face  = "bold",
      vjust = 0.5,
      hjust = 0.5,
      angle = 90
    )
  )
}

break_point <- function(TMP, VWC) {
  b = sort(TMP, index.return = TRUE)
  
  TMP = TMP[b$ix]
  VWC = VWC[b$ix]
  
  d = Mann_Kendall(timeserial = VWC)
  
  yUF <- as.data.frame(d$UF[3])$U
  
  yUB <- as.data.frame(d$UB[3])$U
  
  break_point_index = which.min(abs(yUB - yUF))
  
  break_point = NaN
  
  if (min(abs(yUB - yUF)) <= 0.5) {
    break_point = TMP[break_point_index]
  }
  
  return(break_point)
  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <-
  function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols, nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(
          layout.pos.row = matchidx$row,
          layout.pos.col = matchidx$col
        ))
      }
    }
  }

search_break_point <- function(x_raw, y_raw) {
  
  x = (x_raw - min(x_raw)) / (max(x_raw) - min(x_raw)) # 归一�?
  
  y = (y_raw - min(y_raw)) / (max(y_raw) - min(y_raw)) # 归一�?
  
  length0 = length(y)
  
  angle_r2 = rep(NaN, length0) # 先建立一个向量，存储结果
  
  min_length =  8
  
  for (i in min_length:(length0 - min_length)) {
    # 从第八个点开始逐个计算，一直到倒数第八个点
    
    x_01 = x[1:i]  # 取出前半截数�?
    x_02 = x[(i + 1):length0] # 取出前半截数�?
    y_01 = y[1:i] # 取出后半截数�?
    y_02 = y[(i + 1):length0] # 取出后半截数�?
    
    model_01 = lm(y_01 ~ x_01)   #前半截线性回�?
    model_02 = lm(y_02 ~ x_02)   #后半截线性回�?
    
    slope_01 = summary.lm(model_01)$coefficients[2,1]  # 水平直线的斜�?
    slope_02 = summary.lm(model_02)$coefficients[2,1]  # 垂直直线的斜�?
    
    p_value_01 = summary.lm(model_01)$coefficients[2,4] # 水平直线的P�?
    p_value_02 = summary.lm(model_02)$coefficients[2,4] # 垂直直线的P�?
    
    r2_01 = summary.lm(model_01)$r.squared # 水平直线的R2
    r2_02 = summary.lm(model_02)$r.squared # 垂直直线的R2
    
    if (is.na(p_value_02) != 1 & is.na(p_value_01) != 1) {
      if (p_value_02 < 0.05 & p_value_01 < 0.05) {
        s = abs((slope_02 - slope_01) / (1 + slope_02 * slope_01)) # 计算tan(theta)
        
        if (atan(s) * 180 / pi > 50) {
          angle_r2[i] = atan(s) * 180 / pi + min(c(r2_01, r2_02)) * 100.
          # 夹角 + R2，这个值最大的时候，表示两条直线的夹角最大，并且拟合的也很好�?
          
        }
        
      }
    }
  }
  
  change_point_index = which.max(angle_r2)
  
  return(change_point_index)
  
}