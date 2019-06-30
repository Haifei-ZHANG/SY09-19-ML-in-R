#EX1 Filtrage de spams
#q1
spam <- read.table('D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP6/spambase.dat')
n <- nrow(spam)
napp <- round(n*2/3)
train <- sample(1:n,napp)
ntest <- n-napp


#q2
library(tree)
tree.spam <- tree(as.factor(V58)~., spam, subset = train)
#, control = tree.control(nobs = napp,mindev = 0.0001)
plot(tree.spam)
text(tree.spam,pretty = 0)
yhat <- predict(tree.spam, newdata = spam[-train,], type = 'class')
ytest <- spam[-train,"V58"]
perf.tree <- table(ytest,yhat)
err <- 1-mean(ytest==yhat)
#1-sum(diag(perf.tree))/ntest

#q3
size <- cv.tree(tree.spam)$size
DEV <- rep(0,length(size))
for(i in 1:10){
  cv.spam <- cv.tree(tree.spam)
  DEV <- DEV+cv.spam$dev
}
DEV <- DEV/10
plot(cv.spam$size, DEV, type = 'b')
prune.spam <- prune.tree(tree.spam, best=size[which.min(DEV)])
plot(prune.spam)
text(prune.spam,pretty = 0)
yhatprune <- predict(prune.spam, newdata = spam[-train,], type = 'class')
table(ytest,yhatprune)
errprune <- 1-mean(ytest==yhatprune)


#q4
library(randomForest)
#bagging
bag.spam <- randomForest(as.factor(V58)~., data = spam, subset = train, mtry=57)
yhatbag <- predict(bag.spam, newdata = spam[-train,], type = 'class')
table(ytest,yhatbag)
errbag <- 1-mean(ytest==yhatbag)

#RF
RF.spam <- randomForest(as.factor(V58)~., data = spam, subset = train, mtry=7)
yhatRF <- predict(RF.spam, newdata = spam[-train,], type = 'class')
table(ytest,yhatRF)
errRF <- 1-mean(ytest==yhatRF)
