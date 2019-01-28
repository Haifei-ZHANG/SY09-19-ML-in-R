reg_n = nrow(reg_app)
library('leaps')
#method forward
reg_backwardss <- regsubsets(y~., data=reg_app, method='backward', nvmax=50)
plot(reg_backwardss,scale = "bic",main = " Method backward stepwise")
reg_backwardss_summary <- summary(reg_backwardss)


#consruction des formulas
reg_backwardss_predictorsmatrix<-reg_backwardss_summary$which[which(reg_backwardss_summary$bic<(-430)),]
reg_backwardss_formula <- c() 
for(i in 1:nrow(reg_backwardss_predictorsmatrix)){
  group_predictors_names <- names(which(reg_backwardss_predictorsmatrix[i,TRUE]))
  group_predictors_names <- group_predictors_names[-(1)]
  reg_backwardss_formula <- c(reg_backwardss_formula,as.formula(paste("y", paste(group_predictors_names, collapse=" + "), sep=" ~ ")))
}
rm("group_predictors_names")

#original linear regression
train <- sample(1:reg_n,floor(2*reg_n/3))
reg_train <- reg_app[train, ]
reg_train.x <- reg_train[, -51]
reg_train.y <- as.vector(reg_train[, 51])
reg_test <- reg_app[-train, ]
reg_test.x <- reg_test[, -51]
reg_test.y <- as.vector(reg_test[, 51])

reg_ordinary_model <- lm(y~., data = reg_train)
reg_ordinary_pred <- predict(reg_ordinary_model,newdata = reg_test)
reg_ordinary_rrs <- sum((reg_test.y - reg_ordinary_pred)^2)/length(reg_test.y)

#K-folds cross-validation
K <- 10
folds = sample(1:K,reg_n,replace = TRUE)
reg_cv <- rep(0,length(reg_backwardss_formula))
#choisir le meilleur model
reg_subset_min_rrs <- Inf
library(MASS)
for(i in 1:length(reg_backwardss_formula)){
  for(k in 1:K){
    #regression
    reg_subset_model <- lm(reg_backwardss_formula[[i]], data = reg_app[folds != k,])
    reg_subset_pred <- predict(reg_subset_model,newdata = reg_app[folds == k,])
    reg_cv[i] <- reg_cv[i] + sum((reg_app$y[folds == k] - reg_subset_pred)^2)
  }
  
  reg_cv[i] <- reg_cv[i]/reg_n
  
  if(reg_cv[i] < reg_subset_min_rrs){
    reg_subset_min_rrs <- reg_cv[i]
    reg_subset_best_formula <- reg_backwardss_formula[[i]]
    reg_subset_best_model <- lm(reg_subset_best_formula, data = reg_app)
  }
}


#ridge regression
library("glmnet")
x<-model.matrix(y~.,reg_app)
y<-reg_app$y 
xapp<-x[train,]
yapp<-y[train]
xtst<-x[-train,]
ytst<-y[-train]
ridge.cv.out<-cv.glmnet(xapp,yapp,alpha=0)
plot(ridge.cv.out, main="ridge")
ridge.fit<-glmnet(xapp,yapp,lambda=ridge.cv.out$lambda.min,alpha=0)
ridge.pred<-predict(ridge.fit,s=ridge.cv.out$lambda.min,newx=xtst)
ridge.rrs <- mean((ytst-ridge.pred)^2)


#lasso
lasso.cv.out<-cv.glmnet(xapp,yapp,alpha=1)
plot(lasso.cv.out,main = "lasso")
lasso.fit<-glmnet(xapp,yapp,lambda=lasso.cv.out$lambda.min,alpha=1)
lasso.pred<-predict(lasso.fit,s=lasso.cv.out$lambda.min,newx=xtst)
lasso.rrs <- mean((ytst-lasso.pred)^2)

