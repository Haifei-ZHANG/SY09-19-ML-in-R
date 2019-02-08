astro <- read.csv("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP7/astro/astronomy_train.csv")
astro = astro [,c(-1,-10)]
astro.n <- nrow(astro)
astro.p <- ncol(astro)
napp<- round(2*astro.n/3)
ntst<- astro.n-napp
ntrain <- sample(1:astro.n,  floor(2*astro.n/3))
astro.train <- astro[ntrain,]
astro.test <- astro[-ntrain,]
library(caret) 
K=10 
folds<-createFolds(1:astro.n,K)

#KNN
library(class)
library(MASS)
library(caret)
error_knn<-0
knn_class<-knn(astro.train[,-12],astro.test[,-12],astro.train[,12],k=100)
knn_table<-table(astro.test[,12],knn_class)
error_knn<-1-sum(diag(knn_table))/ntst
cat("Error knn =", error_knn)

#LDA + subset selection
library(klaR)
error_lda<-0
class.fit_b<-stepclass(class~.,data=astro,method='lda',direction="backward")
for(i in (1:K)){
  data.train<-astro[-folds[[i]],]
  data.test<-astro[folds[[i]],]
  lda.mod<-lda(class.fit_b$formula,data=data.train)
  lda.pred<-predict(lda.mod,newdata=data.test)
  lda.table<-table(data.test$class,lda.pred$class)
  error_lda<-error_lda+(1-sum(diag(lda.table))/(astro.n/K))
}
error_lda<-error_lda/K
(class.fit_b)
cat("Error lda =", error_lda)

#QDA + Subset selection
error_qda<-0
class.fit_b<-stepclass(class~.,data=astro,method='qda',direction="backward")
for(i in (1:K)){
  data.train<-astro[-folds[[i]],]
  data.test<-astro[folds[[i]],]
  qda.mod<-qda(class.fit_b$formula,data=data.train)
  qda.pred<-predict(qda.mod,newdata=data.test)
  qda.table<-table(data.test$class,qda.pred$class)
  error_qda<-error_qda+(1-sum(diag(qda.table))/(astro.n/K))
}
error_qda<-error_qda/K
(class.fit_b)
cat("error qda =", error_qda)

#Naive Bayes classifier
library(e1071)
error_nbc<-0
for(i in (1:K)){
  data.train<-astro[-folds[[i]],]
  data.test<-astro[folds[[i]],]
  nbc.mod<-naiveBayes(as.factor(class)~.,data=data.train)
  nbc.pred<-predict(nbc.mod,newdata=data.test)
  nbc.table<-table(data.test$class,nbc.pred)
  error_nbc<-error_nbc+(1-sum(diag(nbc.table))/(astro.n/K))
}
error_nbc<-error_nbc/K
cat("error nbc =", error_nbc)

#Subset selection + Logistic Regression
library(nnet)
logr.mod<-multinom(formula =as.factor(class) ~ ra + u + r + i + z + run + redshift + mjd,data=astro.train)
logr.pred<-predict(logr.mod,newdata=astro.test)
logr.table<-table(astro.test$class,logr.pred)
error_logr<-1-sum(diag(logr.table))/ntst
cat("final model : class ~ ra + u + r + i + z + run + redshift + mjd \n")
cat("error logr =", error_logr)

#Decision Tree
library(tree)
astro$class<- as.factor(astro$class)
tree.astro<- tree(class ~.,data=astro.train,control=tree.control(nobs=napp,mindev = 0.0001))
yhat<-predict(tree.astro,newdata=astro.test[,-12],type='class')
perf.tree <-table(astro.test$class,yhat)
error_tree_original <- 1-sum(diag(perf.tree))/ntst
cat("error decision tree =", error_tree_original)

cv.tree.astro<-cv.tree(tree.astro,FUN=prune.misclass)
plot(cv.tree.astro$size,cv.tree.astro$dev/napp,type="b")
prune.astro<-prune.misclass(tree.astro,best=4)
yhat<-predict(prune.astro,newdata=astro.test,type='class')
perf.tree <-table(astro.test$class,yhat)
error_tree_prune<-1-sum(diag(perf.tree))/ntst
plot(prune.astro)
text(prune.astro,pretty=0)
cat("Error of pruned tree with 4 branches =", error_tree_prune)

#bagging
library(randomForest)
bag.astro<-randomForest(class~.,data=astro.train,ntree=500,mtry=17)
yhat.bag<-predict(bag.astro,newdata=astro.test,type='class')
perf.bag <-table(astro.test$class,yhat.bag)
error_bagging<-1-sum(diag(perf.bag))/ntst
cat("error  bagging tree =", error_bagging)

#Random forest
rf.astro<-randomForest(class~.,data=astro,subset=ntrain,mtry=4)
yhat.rf<-predict(bag.astro,newdata=astro.test,type='class')
perf.rf <-table(astro.test$class,yhat.bag)
error_rf <- 1-sum(diag(perf.bag))/ntst
cat("error random Forest = ", error_rf)

#SVM
library(kernlab)
library(e1071)
svm.model <- svm(class~., data = astro.train, type = 'C-classification')
svm.pred <- predict(svm.model, newdata=astro.test)
svm.table <- table(astro.test$class, svm.pred)
error_svm <- 1-sum(diag(svm.table))/(ntst)
cat("Le taux d'erreur du SVM sans noyau : ",error_svm)

svm_test <- function(x,y){
  type <- c('C-classification','one-classification')
  kernel <- c('linear','polynomial','radial','sigmoid')
  pred <- array(0, dim=c(nrow(x),2,4))
  errors <- matrix(0,2,4)
  dimnames(errors) <- list(type, kernel)
  for(i in 1:2){
    for(j in 1:4){
      pred[,i,j] <- predict(object = svm(x, y, type = type[i], kernel = kernel[j]), newdata = x,cost=100)
      if(i > 2) errors[i,j] <- sum(pred[,i,j] != 1)
      else errors[i,j] <- sum(pred[,i,j] != as.integer(y))/nrow(astro)
    }
  }
  return(errors)
}
choix_svm<-svm_test(x=astro[,-12],y=astro$class)
choix_svm

error_svm_linear<-choix_svm[1]
cat("error svm with linear kernel =", error_svm_linear)

#KSVM
ksvm.mod_lap<- ksvm(class~.,data=astro.train,scaled=TRUE,type="C-svc",kernel="laplacedot",prob.model=TRUE, kpar=list(sigma=0.1), C=100,epsilon=0.1 )
ksvm.pred_lap<- predict(ksvm.mod_lap,newdata=astro.test)
ksvm.table_lap<-table(astro.test$class,ksvm.pred_lap)
error_ksvm_laplace<-1-sum(diag(ksvm.table_lap))/ntst
ksvm.mod_gau<- ksvm(class~.,data=astro.train,type="C-bsvc",kernel="rbfdot",prob.model=TRUE,kpar=list(sigma=0.1), C=100,epsilon=0.1)
ksvm.pred_gau<- predict(ksvm.mod_gau,newdata=astro.test)
ksvm.table_gau<-table(astro.test$class,ksvm.pred_gau)
error_ksvm_gau<- 1-sum(diag(ksvm.table_gau))/ntst
ksvm.mod_pol<- ksvm(class~.,data=astro.train,type="C-svc",kernel="polydot",prob.model=TRUE,C=100,epsilon=0.1,kpar=list(degree=1, scale= 1, offset = 1))
ksvm.pred_pol<- predict(ksvm.mod_pol,newdata=astro.test)
ksvm.table_pol<-table(astro.test$class,ksvm.pred_pol)
error_ksvm_poly<-1-sum(diag(ksvm.table_pol))/ntst
cat("error_ksvm_Laplacian = ",error_ksvm_laplace,"\n")
cat("error_ksvm_Gaussian =",error_ksvm_gau,"\n")
cat("error_ksvm_Polynomial =", error_ksvm_poly,"\n")

#conclusion
ERRORs <- c(error_knn, error_lda, error_qda, error_nbc, error_logr,  error_tree_original,error_tree_prune, error_bagging,error_rf,error_svm,error_svm_linear, error_ksvm_laplace, error_ksvm_gau,error_ksvm_poly)
methodes <- c("KNN", "LDA", "QDA", "Naive Bayes classifier",  "Logistic Regression","original tree","pruned tree","bagging tree","random forest", "SVM sans noyau", "Linear Kernel","Laplacian kernel",  "Gaussian kernel " ,"Polynomial Kernel")
barplot(ERRORs,col=c("sandybrown","sandybrown","hotpink","sandybrown","hotpink","hotpink","hotpink", "hotpink","hotpink","sandybrown","steelblue","steelblue","steelblue","hotpink"),ylim=c(0,0.2),width=1,space=1,ylab="ERROR",las=1,main = "error par différentes méthodes")
text(x=seq(2,28,by=2),y=-0.002, srt = 30, adj = 1.2, labels = methodes,xpd = TRUE)
abline(h=0)
cat("La meilleure méthode : ",methodes[which.min(ERRORs)])
cat("\nSon taux d'erreur : ",min(ERRORs))
