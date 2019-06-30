setwd("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP7")
#read data, build the x(with dimention 57) and y
mais <- read.csv("./data/mais_train.csv")

library(kernlab)
library(MASS)
mais.bestmodel<-ksvm(yield_anomaly~.,data=mais,scaled=TRUE,type="eps-svr",kernel="laplacedot",C=10,epsilon=0.1)
mean((mais.bestmodel@fitted - mais[,'yield_anomaly'])^2)
