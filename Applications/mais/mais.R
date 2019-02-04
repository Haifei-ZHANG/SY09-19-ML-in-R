setwd("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP7")
#read data, build the x(with dimention 57) and y
mais <- read.csv("./data/mais_train.csv")
mais <- mais[,-1]
mais.n <- nrow(mais)
mais.y <- mais[,2]
mais.x <- mais[,-2]
mais.p <- ncol(mais.x)
mais.napp <- floor(2*mais.n/3)
mais.ntest <- mais.n-mais.napp
mais.train <- sample(1:mais.n,mais.napp)

#build the train set and test set 
mais.trainset <- mais[mais.train,]
mais.testset <- mais[-mais.train,]
mais.trainset.x <- mais.x[mais.train,]
mais.trainset.y <- mais.y[mais.train]
mais.testset.x <- mais.x[-mais.train,]
mais.testset.y <- mais.y[-mais.train]

#feature extraction
#PCA PCR
library(pls)
mais.pcr.model <- pcr(yield_anomaly~.,data = mais.trainset,scale=TRUE,validation="CV")
summary(mais.pcr.model)
validationplot(mais.pcr.model,val.type = "MSEP", legendpos = "topright")
#here we can see, with all of the future is best

#subset selection
par(mfrow=c(1,1))
library('leaps')
mais.forwardss <- regsubsets(yield_anomaly~.,data = mais.trainset, method='forward', nvmax=57)
plot(mais.forwardss,scale = "adjr2",main = "Method forward stepwise adjusted R2")
plot(mais.forwardss,scale = "bic",main = "Methodforward stepwise BIC")
mais.forwardss.summary <- summary(mais.forwardss)
##########################################################################################################################

#method0 : linear regression
mais.lr.model <- lm(yield_anomaly~.,data = mais.trainset)
#use this model on the test set
mais.lr.yhat <- predict(mais.lr.model,newdata = mais.testset)
mais.lr.mse <- sum((mais.lr.yhat - mais.testset.y)^2)/mais.ntest
plot(mais.lr.yhat, mais.testset.y, asp = 1)
abline(0, 1)
##########################################################################################################################

#method1 : elastic net (Ridge and Lasso and the choice of the best alpha)
library(glmnet)
mais.ridge.cv.out <- cv.glmnet(model.matrix(yield_anomaly~.,mais),mais.y,alpha=0)
plot(mais.ridge.cv.out)
mais.ridge.model <- glmnet(model.matrix(yield_anomaly~.,mais.trainset),mais.trainset.y,lambda =  mais.ridge.cv.out$lambda.min,alpha = 0)
mais.ridge.yhat <- predict(mais.ridge.model,s=mais.ridge.cv.out$lambda.min,newx = model.matrix(yield_anomaly~.,mais.testset))
mais.ridge.mse <- mean((mais.ridge.yhat- mais.testset.y)^2)
##########################################################################################################################

mais.lasso.cv.out <- cv.glmnet(model.matrix(yield_anomaly~.,mais),mais.y,alpha=1)
plot(mais.lasso.cv.out)
mais.lasso.model <- glmnet(model.matrix(yield_anomaly~.,mais.trainset),mais.trainset.y,lambda =  mais.lasso.cv.out$lambda.min,alpha = 1)
mais.lasso.yhat <- predict(mais.lasso.model,s=mais.lasso.cv.out$lambda.min,newx = model.matrix(yield_anomaly~.,mais.testset))
mais.lasso.mse <- mean((mais.lasso.yhat- mais.testset.y)^2)
##########################################################################################################################

#confirm alpha by crosse validation
K <- 10
folds = sample(1:K,mais.n,replace = TRUE)
mais.elastic.mse <- rep(0,11)
for(alpha in seq(0,1,by=0.1)){
  mais.elastic.cv.out <- cv.glmnet(model.matrix(yield_anomaly~.,mais),mais.y,alpha=alpha)
  for(k in 1:K){
    
    #plot(mais.lasso.cv.out)
    mais.elastic.model <- glmnet(model.matrix(yield_anomaly~.,mais[folds == k,]),mais.y[folds == k],lambda =  mais.elastic.cv.out$lambda.min,alpha = alpha)
    mais.elastic.yhat <- predict(mais.elastic.model,s=mais.elastic.cv.out$lambda.min,newx = model.matrix(yield_anomaly~.,mais[folds == k,]))
    mais.elastic.mse[10*alpha+1] <- mais.elastic.mse[10*alpha+1]+sum((mais.elastic.yhat- mais.y[folds == k])^2)
    }
  mais.elastic.mse[10*alpha+1] <- mais.elastic.mse[10*alpha+1]/mais.n
}
plot(y=mais.elastic.mse,x=seq(0,1,by=0.1),pch=20, type="b", lty=1,main="mse en différents alphas",xlab="alpha",ylab="mse")#here we choose the best alpha, and build the model with this alpha
alpha <- (which.min(mais.elastic.mse)-1)/10
mais.elastic.cv.out <- cv.glmnet(model.matrix(yield_anomaly~.,mais),mais.y,alpha=alpha)
mais.elastic.bestmodel <- glmnet(model.matrix(yield_anomaly~.,mais.trainset),mais.trainset.y,lambda =  mais.elastic.cv.out$lambda.min,alpha =alpha)
mais.elastic.bestyhat <- predict(mais.elastic.bestmodel,s=mais.elastic.cv.out$lambda.min,newx = model.matrix(yield_anomaly~.,mais.testset))
mais.elastic.bestmse <- mean((mais.elastic.bestyhat- mais.testset.y)^2)
##########################################################################################################################

#method2 : poly regression
#find the best degree by crosse validation
#mais.x.names=paste(colnames(mais), collapse=" + ")
library('MASS')
mais.poly.mse <- rep(0,10)
for(d in 1:10){
  for(k in (1:K)){
   # x=as.matrix(mais[folds == k,-2])
    mais.poly.model=lm(yield_anomaly~poly(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,degree=d),data=mais[folds!=k,])
    #mais.poly.model=lm(mais.y[folds!=k]~poly(as.matrix(mais.x[folds!=k,]),degree=d))
    mais.poly.yhat<-predict(mais.poly.model,newdata=mais[folds==k,])
    mais.poly.mse[d]<-mais.poly.mse[d]+ sum((mais.y[folds == k]-mais.poly.yhat)^2)
  }
  mais.poly.mse[d] <- mais.poly.mse[d]/mais.n
}
mais.poly.bestmodel <- lm(yield_anomaly~poly(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,degree=which.min(mais.poly.mse)),data=mais.trainset)
mais.poly.bestyhat <- predict(mais.poly.bestmodel,newdata=mais.testset)
mais.poly.bestmse <- mean((mais.poly.bestyhat- mais.testset.y)^2)
##########################################################################################################################

#method3 : spline regression
library('splines')
#Natural Cubic Splines
mais.ns.mse <- rep(0,10)
for(d in 1:10){
  for(k in (1:K)){
    # x=as.matrix(mais[folds == k,-2])
    mais.ns.model=lm(yield_anomaly~ns(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,df=d),data=mais[folds!=k,])
    #mais.poly.model=lm(mais.y[folds!=k]~poly(as.matrix(mais.x[folds!=k,]),degree=d))
    mais.ns.yhat<-predict(mais.ns.model,newdata=mais[folds==k,])
    mais.ns.mse[d]<-mais.ns.mse[d]+ sum((mais.y[folds == k]-mais.ns.yhat)^2)
  }
  mais.ns.mse[d] <- mais.ns.mse[d]/mais.n
}
mais.ns.bestmodel <- lm(yield_anomaly~ns(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,df=which.min(mais.ns.mse)),data=mais.trainset)
mais.ns.bestyhat <- predict(mais.ns.bestmodel,newdata=mais.testset)
mais.ns.bestmse <- mean((mais.ns.bestyhat- mais.testset.y)^2)
##########################################################################################################################

#B-Spline Basis
mais.bs.mse <- rep(0,10)
for(d in 1:10){
  for(k in (1:K)){
    # x=as.matrix(mais[folds == k,-2])
    mais.bs.model=lm(yield_anomaly~bs(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,df=d),data=mais[folds!=k,])
    #mais.poly.model=lm(mais.y[folds!=k]~poly(as.matrix(mais.x[folds!=k,]),degree=d))
    mais.bs.yhat<-predict(mais.bs.model,newdata=mais[folds==k,])
    mais.bs.mse[d]<-mais.bs.mse[d]+ sum((mais.y[folds == k]-mais.bs.yhat)^2)
  }
  mais.bs.mse[d] <- mais.bs.mse[d]/mais.n
}
mais.bs.bestmodel <- lm(yield_anomaly~bs(year_harvest + NUMD + IRR + ETP_1 + ETP_2 + ETP_3 + ETP_4 + ETP_5 + ETP_6 + ETP_7 + ETP_8 + ETP_9 + PR_1 + PR_2 + PR_3 + PR_4 + PR_5 + PR_6 + PR_7 + PR_8 + PR_9 + RV_1 + RV_2 + RV_3 + RV_4 + RV_5 + RV_6 + RV_7 + RV_8 + RV_9 + SeqPR_1 + SeqPR_2 + SeqPR_3 + SeqPR_4 + SeqPR_5 + SeqPR_6 + SeqPR_7 + SeqPR_8 + SeqPR_9 + Tn_1 + Tn_2 + Tn_3 + Tn_4 + Tn_5 + Tn_6 + Tn_7 + Tn_8 + Tn_9 + Tx_1 + Tx_2 + Tx_3 + Tx_4 + Tx_5 + Tx_6 + Tx_7 + Tx_8 + Tx_9,df=which.min(mais.bs.mse)),data=mais.trainset)
mais.bs.bestyhat <- predict(mais.bs.bestmodel,newdata=mais.testset)
mais.bs.bestmse <- mean((mais.bs.bestyhat- mais.testset.y)^2)
##########################################################################################################################

#regression tree
library(tree)
mais.tree.model<-tree(yield_anomaly~.,mais.trainset)
#plot(mais.tree.model)
#text(mais.tree.model,pretty=0)
mais.tree.yhat <- predict(mais.tree.model,newdata = mais.testset)
mais.tree.mse <- mean((mais.tree.yhat - mais.testset.y)^2)
#use the pruning tree
mais.prune.mse <- rep(0,20)
mais.prune.mse[1]=1
for(beta in 2:20){
  for(k in (1:K)){
    mais.prunetree.model<-tree(yield_anomaly~.,data=mais[folds!=k,])
    mais.prunetree.model<-prune.tree(mais.prunetree.model,best=beta)
    #mais.poly.model=lm(mais.y[folds!=k]~poly(as.matrix(mais.x[folds!=k,]),degree=d))
    mais.prunetree.yhat<-predict(mais.prunetree.model,newdata=mais[folds==k,])
    mais.prune.mse[beta]<-mais.prune.mse[beta]+ sum((mais.y[folds == k]-mais.prunetree.yhat)^2)
  }
  mais.prune.mse[beta] <- mais.prune.mse[beta]/mais.n
}
#plot(y=mais.prune.mse,x=seq(1,20,by=1),pch=20, lty=1, ylim = c(0.7,1))
beta<-which.min(mais.prune.mse)
mais.prunetree.bestmodel<-tree(yield_anomaly~.,data=mais.trainset)
mais.prunetree.bestmodel=prune.tree(mais.prunetree.bestmodel,best=beta)
mais.prunetree.bestyhat <- predict(mais.prunetree.bestmodel,newdata = mais.testset)
mais.prunetree.bestmse <- mean((mais.prunetree.bestyhat - mais.testset.y)^2)
##########################################################################################################################

#SVR
library(kernlab)
library(MASS)
mais.svr.mse <- rep(0,6)
C <- c(0.01,0.1,1,10,100,1000)
for(i in 1:length(C)){
  for(k in (1:K)){
    mais.svr.model<-ksvm(yield_anomaly~.,data=mais[folds!=k,],scaled=TRUE,type="eps-svr",kernel="laplacedot",C=C[i],epsilon=0.1)
    mais.svr.yhat<-predict(mais.svr.model,newdata=mais[folds==k,])
    mais.svr.mse[i]<-mais.svr.mse[i]+ sum((mais.y[folds == k]-mais.svr.yhat)^2)
  }
  mais.svr.mse[i] <- mais.svr.mse[i]/mais.n
}
plot(y=mais.svr.mse,x=c(0.01,0.1,1,10,100,1000),pch=20, lty=1, ylim = c(0.4,1))
C<-C[which.min(mais.svr.mse)]
mais.svr.bestmodel<-ksvm(yield_anomaly~.,data=mais.trainset,scaled=TRUE,type="eps-svr",kernel="laplacedot",C=C,epsilon=0.1)
mais.svr.bestyhat <- predict(mais.svr.bestmodel,newdata = mais.testset)
mais.svr.bestmse <- mean((mais.svr.bestyhat - mais.testset.y)^2)
cat("La mse de SVR : ",mais.svr.bestmse,"\n")

#Mixture of regression
library(mixtools)
mais.regmixEM.model<-regmixEM(mais.trainset.y,as.matrix(mais.trainset.x),epsilon = 1e-04)
beta<-as.matrix(mais.regmixEM.model$beta[,2])
mais.regmixEM.testX <- as.matrix(mais.testset.x)
mais.regmixEM.yhat<-mais.regmixEM.testX %*% beta[-1,1]+beta[1,1]
mais.regmixEM.mse<-mean((mais.regmixEM.yhat - mais.testset.y)^2)


#Conclusion
MSEs <- c(mais.lr.mse,mais.ridge.mse,mais.lasso.mse,mais.elastic.bestmse,mais.poly.bestmse,mais.ns.bestmse,mais.bs.bestmse,mais.tree.mse,mais.prunetree.bestmse,mais.regmixEM.mse,mais.svr.bestmse)
methodes <- c("linear regression","Ridgr regression","Lasso regression","Elestic net","Polynomial regression","Natual spline","B-spline","Regression tree","Regression pruned tree","Mixture of regression","SVR")
barplot(MSEs,col=c("steelblue","steelblue","steelblue","mediumturquoise","mediumturquoise","mediumturquoise","mediumturquoise", "mediumturquoise","mediumturquoise","sandybrown","hotpink","hotpink"),ylim=c(0,1.1),width=1,space=1,ylab="MSE",las=1)
text(x=seq(2,24,by=2),y=-0.05, srt = 45, adj = 1, labels = methodes,xpd = TRUE)
abline(h=seq(0,1,by=0.1),col="#00000088",lwd=2)
abline(h=0)
