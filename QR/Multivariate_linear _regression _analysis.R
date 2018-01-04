rm(list = ls(all=TRUE))  
library(car)  
library(corrplot, quietly=TRUE)  
library(sqldf)  

## 数据读取  
folder <- "~/Documents/R/Quantify_results/QR/data/" 
# 销量分3类读取  
name <- "vt.csv"  
namepre <- "vp.csv"  
path <- paste(folder,name,sep = "");  
pathpre <- paste(folder,namepre,sep = "")  
# 训练集  
dataset <- read.csv(path, na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")  
# 预测集  
datasetpre <- read.csv(pathpre, na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")  
dataselect <- sqldf("select * from datasetpre")  
datapre <- dataselect[2:ncol(dataselect)]  

## 训练集准备  
names <- attributes(dataset)$names  
names  
datatrain <- dataset[names[1:length(names)-1]]  
datatarget <- dataset$t  
cor <- cor(dataset,use="pairwise", method="pearson")  
cor   
corrplot(cor)  

pca <- princomp(datatrain,cor=T)  
summary(pca,loadings=T)  
screeplot(pca,type='lines')  
loading <- as.data.frame(pca$loadings[])  
comps <- as.data.frame(as.matrix(datatrain)%*%as.matrix(loading))  

## 主成分拟合多元线性回归方程  
lmP <- lm(formula = dataset$t~comps$Comp.1+comps$Comp.2)  
summary(lmP)  

compsx <- comps[c("Comp.1","Comp.2")]  
x <- cor(compsx,use="pairwise", method="pearson")  
kappa(x)  

step(lmP)  
## 残差分析  
par(mfrow=c(2,2))  
plot(lmP,which=1)  
plot(lmP,which=2)  
plot(lmP,which=3)  
plot(lmP,which=4)  


# comppre <- as.data.frame(as.matrix(datapre)%*%as.matrix(loading))[c(1,2,3)]  
# coe <- as.matrix(lmP$coefficients[2:length(lmP$coefficients)])  
# tpre <- as.matrix(comppre)%*%coe+lmP$coefficients[1]  
# 
# output <- data.frame(dataselect[1],as.data.frame(tpre))  
# pathwrite <- paste(folder,"tpre.csv",sep = "")  
# write.table(output, file =pathwrite,sep =",",col.names =T,row.names = F)  