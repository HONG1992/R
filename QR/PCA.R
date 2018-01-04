rm (list = ls())
library(psych) # 载入psych包
library(stats)
df<- read.table(file = 'All_VWC_Results.csv', header = T, sep = ',')
fa.parallel(df[,-1], fa = "pc", n.iter = 100,
            show.legend = F, main = "Scree plot with parallel analysis")
pc<-principal(df[,-1], nfactors = 3, score = T, rotate = "varimax")
pc
round(unclass(pc$weights),3)
# arrests.pr<- prcomp(df, scale = TRUE)
# summary(arrests.pr, loadings=TRUE)
# prcomp(df, scale = TRUE)
# cumsum(arrests.pr$sdev^2)/9
# arrests.pr$x
# screeplot(arrests.pr,main="df",type = "lines")
# biplot(arrests.pr)
# data.frame(sort(arrests.pr$x[,1]))
# df.cor<-cor(df)
# df.cor
# df.eigen<-eigen(df.cor)
# df.eigen
# df.pr<- princomp(df, cor=T)
# summary(df.pr)
# df.pr$center
# df.pr$scale
# df.fa<-factanal(df,factors = 2)
# print(df.fa, cutoff=0.001)
# df.fa<-factanal(df,factors = 2, scores = "regression")
# df.fa$scores
# 
