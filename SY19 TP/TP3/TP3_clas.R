setwd("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP3")
clas_app <- read.table('D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP3/tp3_a18_clas_app.txt')
reg_app <- read.table('D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP3/tp3_a18_reg_app.txt')
names(clas_app) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21","x22","x23","x24","x25","x26","x27","x28","x29","x30","x31","x32","x33","x34","x5","x6","y")
clas_n <- nrow(clas_app)
clas_p <- ncol(clas_app)
#Subset selection
library(leaps)
library(MASS)

#method forward
# clas_forwardss <- stepAIC(y~., data=clas_app, method='forward', nvmax=36)
# plot(clas_forwardss,scale = "adjr2",main = " Method forward stepwise")
# clas_forwardss_summary <- summary(clas_forwardss)
# 
# #consruction des formulas
# clas_forwardss_predictorsmatrix<-clas_forwardss_summary$which[which(clas_forwardss_summary$adjr2>0.49),]
# clas_forwardss_formula <- c() 
# clas_bayes_formula <- c()
# for(i in 1:nrow(clas_forwardss_predictorsmatrix)){
#   group_predictors_names <- names(which(clas_forwardss_predictorsmatrix[i,TRUE]))
#   group_predictors_names <- group_predictors_names[-(1)]
#   clas_forwardss_formula <- c(clas_forwardss_formula,as.formula(paste("y", paste(group_predictors_names, collapse=" + "), sep=" ~ ")))
#   clas_bayes_formula <- c(clas_bayes_formula,paste(group_predictors_names, collapse=" + "))
#   }

train <- sample(1:clas_n,floor(2*clas_n/3))
clas_train <- clas_app[train, ]
clas_test <- clas_app[-train, ]

#validation-set approach
clas_erreurs <- matrix(0,4,3)
rownames(clas_erreurs) <- c("clas_lda","clas_qda","clas_logreg","clas_NB")
colnames(clas_erreurs) <- c("vs","cv","subset+cv")
#lda avec validation-set approach
clas_model <- lda(y~., data = clas_train)
clas_pred <- predict(clas_model, newdata = clas_test)
clas_perf <- table(clas_test$y,clas_pred$class)
clas_erreurs['clas_lda',1] <- 1-sum(diag(clas_perf))/length(clas_test$y)
clas_erreurs['clas_lda',1]

#qda avec validation-set approach
clas_model <- qda(y~., data = clas_train)
clas_pred <- predict(clas_model, newdata = clas_test)
clas_perf <- table(clas_test$y,clas_pred$class)
clas_erreurs['clas_qda',1] <- 1-sum(diag(clas_perf))/length(clas_test$y)
clas_erreurs['clas_qda',1]

#logistique regression avec validation-set approach
clas_train[,'y']<-clas_train$y-1
clas_test[,'y']<-clas_test$y-1
clas_model <- glm(y~., data = clas_train)
clas_pred <- predict(clas_model, newdata = clas_test, type = "link")
clas_perf <- table(clas_test$y,clas_pred>0.5)
clas_erreurs['clas_logreg',1] <- 1-sum(diag(clas_perf))/length(clas_test$y)
clas_erreurs['clas_logreg',1]
clas_train[,'y']<-clas_train$y+1
clas_test[,'y']<-clas_test$y+1

#Naive Bayes
clas_model <- NaiveBayes(as.factor(y)~., data = clas_train)
clas_pred <- predict(clas_model, newdata = clas_test)
clas_perf <- table(clas_test$y,clas_pred$class)
clas_erreurs['clas_NB',1] <- 1-sum(diag(clas_perf))/length(clas_test$y)
clas_erreurs['clas_NB',1]


#K-folds cross-validation
K <- 10
folds = sample(1:K,clas_n,replace = TRUE)

for(k in 1:K){
  #lda
  clas_model <- lda(y~., data = clas_app[folds != k,])
  clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
  clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
  clas_erreurs['clas_lda',2] <- clas_erreurs['clas_lda',2] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
  
  #qda
  clas_model <- qda(y~., data = clas_app[folds != k,])
  clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
  clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
  clas_erreurs['clas_qda',2] <- clas_erreurs['clas_qda',2] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
  
  #logreg
  clas_app[,'y']<-clas_app$y-1   #y-1
  clas_model <- glm(y~., data = clas_app[folds != k,],family  = binomial)
  clas_pred <- predict(clas_model, newdata = clas_app[folds == k,],  type = "link")
  clas_perf <- table(clas_app[folds == k,]$y,clas_pred>0.5)
  clas_erreurs['clas_logreg',2] <- clas_erreurs['clas_logreg',2] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
  clas_app[,'y']<-clas_app$y+1  #y+1
  
  ##Naive Bayes
  clas_model <- NaiveBayes(as.factor(y)~., data = clas_app[folds != k,])
  clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
  clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
  clas_erreurs['clas_NB',2] <- clas_erreurs['clas_NB',2] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
}



#subset selection
library(klaR)
clas_lda_formula <- stepclass(y~.,data = clas_app,method = "lda",direction = "backward")$formula
clas_qda_formula <- stepclass(y~.,data = clas_app,method = "qda",direction = "backward")$formula
clas_log_formula <- stepclass(y~.,data = clas_app,method = "glm",direction = "backward")$formula
clas_naiveBayes_formula <- stepclass(y~.,data = clas_app,method = "NativeBayes",direction = "backward")$formula
#K-folds cross-validation
#library(e1071)

for(k in 1:K){
    #lda
    clas_model <- lda(clas_lda_formula, data = clas_app[folds != k,])
    clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
    clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
    clas_erreurs['clas_lda',3] <- clas_erreurs['clas_lda',3] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
    
    #qda
    clas_model <- qda(clas_qda_formula, data = clas_app[folds != k,])
    clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
    clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
    clas_erreurs['clas_qda',3] <- clas_erreurs['clas_qda',3] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
    
    #logreg
    clas_app[,'y']<-clas_app$y-1   #y-1
    clas_model <- glm(clas_log_formula, data = clas_app[folds != k,],family  = binomial)
    clas_pred <- predict(clas_model, newdata = clas_app[folds == k,],  type = "link")
    clas_perf <- table(clas_app[folds == k,]$y,clas_pred>0.5)
    clas_erreurs['clas_logreg',3] <- clas_erreurs['clas_logreg',3] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))
    clas_app[,'y']<-clas_app$y+1  #y+1
    
    clas_model <- NaiveBayes(as.factor(y)~., data = clas_app[folds != k,])
    clas_pred <- predict(clas_model, newdata = clas_app[folds == k,])
    clas_perf <- table(clas_app[folds == k,]$y,clas_pred$class)
    clas_erreurs['clas_NB',3] <- clas_erreurs['clas_NB',3] + (1-sum(diag(clas_perf))/length(clas_app[folds == k,]$y))

}


clas_erreurs[,3] <- clas_erreurs[,3]/K
clas_erreurs
barplot(t(clas_erreurs),col=c('red','green','blue'),beside=TRUE,xlab = "algorithmes", ylab = "taux d'erreur")

clas_best_model <- NaiveBayes(as.factor(y)~., data = clas_app)
