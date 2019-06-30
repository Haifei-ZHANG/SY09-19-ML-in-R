library(EBImage)
library(e1071)
library(OpenImageR)
library(caret)
library(class)
library(kernlab)

read.imags.HOG <- function(path = "./") {
  fns <- list.files(path)
  res <- NULL
  for(i in fns) {
    fn <- paste(path, i, sep = "")
    im <- readImage(fn)
    im <- resize(im,64,64)
    im_HOG<-HOG(im,cells=3,orientations = 6)
    ifelse(!is.null(res),
           res <- rbind(res, im_HOG), res <- im_HOG)
  }
  return(res)
}

car.images.HOG <- read.imags("./car/")
cat.images.HOG <- read.imags("./cat/")
flower.images.HOG <- read.imags("./flower/")

car.images.label<-cbind(car.images,c(rep(0,nrow(car.images))))
cat.images.label<-cbind(cat.images,c(rep(1,nrow(cat.images))))
flower.images.label<-cbind(flower.images,c(rep(2,nrow(flower.images))))
data.imags <- rbind(car.images.label, cat.images.label,flower.images.label)

K=10

#SVM
error_svm<-0
for(i in (1:K)){
 svm_model <- svm(data.imags[-folds[[i]],1:54], y=data.imags[-folds[[i]],55],
           probability = TRUE, cross = 10,
           type = "C-classification", method = "SVM")

  pred <- predict(svm_model,data.imags[folds[[i]],1:54],decision.values = TRUE, probability = TRUE)
  table_svm<-table(data.imags[folds[[i]],55],pred)
  error_svm<-(1-sum(diag(table_svm))/length(folds[[i]]))+error_svm
}
error_svm<-error_svm/K

CC<-c(0.01,0.1,1,5,10,50,100)
N<-length(CC)
err_c_Gau<-rep(0,N)
for(c in 1:N){
  for(i in 1:K){
    model.Gau<-ksvm(x=data.imags[-folds[[i]],1:54],y=as.factor(data.imags[-folds[[i]],55]),type="C-svc",prob.model=TRUE,kernel="rbfdot",C=CC[c],cross=5)
    pred.Gau<-predict(model.Gau,data.imags[folds[[i]],1:54])
    table_Gau<-table(data.imags[folds[[i]],55],pred.Gau)
    err_c_Gau[c]<-((1-sum(diag(table_Gau))/length(folds[[i]])))+err_c_Gau[c]
  }
  err_c_Gau[c]<-err_c_Gau[c]/K
}

err_c_Poly<-rep(0,N)
for(c in 1:N){
  for(i in 1:K){
    model.Poly<-ksvm(x=data.imags[-folds[[i]],1:54],y=as.factor(data.imags[-folds[[i]],55]),type="C-svc",prob.model=TRUE,kernel="polydot",C=CC[c],cross=5)
    pred.Poly<-predict(model.Poly,data.imags[folds[[i]],1:54])
    table_Poly<-table(data.imags[folds[[i]],55],pred.Poly)
    err_c_Poly[c]<-((1-sum(diag(table_Poly))/length(folds[[i]])))+err_c_Poly[c]
  }
  err_c_Poly[c]<-err_c_Poly[c]/K
}


err_c_Mlp<-rep(0,N)
for(c in 1:N){
  for(i in 1:K){
    model.Mlp<-ksvm(x=data.imags[-folds[[i]],1:54],y=as.factor(data.imags[-folds[[i]],55]),type="C-svc",prob.model=TRUE,kernel="tanhdot",C=CC[c],cross=5)
    pred.Mlp<-predict(model.Mlp,data.imags[folds[[i]],1:54])
    table_Mlp<-table(data.imags[folds[[i]],55],pred.Mlp)
    err_c_Mlp[c]<-((1-sum(diag(table_Mlp))/length(folds[[i]])))+err_c_Mlp[c]
  }
  err_c_Mlp[c]<-err_c_Mlp[c]/K
}

#KNN
error_knn<-0
for(j in c(1:10)){
folds<-createFolds(1:nrow(data.imags),K)
for(i in (1:K)){
  knn_model<-knn(data.imags[-folds[[i]],1:54],data.imags[folds[[i]],1:54],data.imags[-folds[[i]],55],k=5)
  res<-table(data.imags[folds[[i]],55],knn_model)
  error_knn<-1-sum(diag(res))/length(folds[[i]])+error_knn
}
}
error_knn<-error_knn/(10*K)



