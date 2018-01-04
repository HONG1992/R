rm(list = ls())

library(ggplot2)
source("function_20160123.R")

data0 <- read.csv("All_VWC_Results.csv", header = T)

brk_pnt = data0$tmp_bp_fr

data = data.frame(y = brk_pnt)

p0 <- ggplot(data = data, aes(y)) +
  geom_histogram(binwidth = 0.5) + 
  xlab("Break Temperature") + ylab("Num.")

ggplot2_set_axis(p0)

ggsave("Hist_Break.png")