#---------------- Exercice 1 ------------------

library('MASS')
n<-length(mcycle$times)

plot(mcycle$times,mcycle$accel)
test.data<-data.frame(times=seq(0,60,1))

summary(mcycle)

# q1

fit1=lm(accel~poly(times,6),data=mcycle)

plot(mcycle$times,mcycle$accel)
lines(mcycle$times,fit1$fitted.values)

fit2=lm(accel~poly(times,8),data=mcycle)

plot(mcycle$times,mcycle$accel)
lines(mcycle$times,fit2$fitted.values)

# etc...

# q2

# 5-fold cross-validation
K<-5
folds=sample(1:K,n,replace=TRUE)
P<-1:10
N<-length(P)
CV1<-rep(0,N)
for(i in (1:N)){
  print(i)
  for(k in (1:K)){
    fit=lm(accel~poly(times,P[i]),data=mcycle[folds!=k,])
    pred<-predict(fit,newdata=mcycle[folds==k,])
    print(pred)
    CV1[i]<-CV1[i]+ sum((mcycle$accel[folds==k]-pred)^2)
  }
  CV1[i]<-CV1[i]/n
}
plot(P,CV1,type='b',xlab='p',ylab='CV error')


# q3
library(splines)
fit=lm(accel~ns(times,df=7),data=mcycle)
plot(mcycle$times,mcycle$accel)
lines(mcycle$times,fit$fitted.values)


DF<-5:20
N<-length(DF)
CV2<-rep(0,N)
folds=sample(1:K,n,replace=TRUE)
for(i in (1:N)){
  print(i)
  for(k in (1:K)){
    fit=lm(accel~ns(times,df=DF[i]),data=mcycle[folds!=k,])
    pred<-predict(fit,newdata=mcycle[folds==k,])
    CV2[i]<-CV2[i]+ sum((mcycle$accel[folds==k]-pred)^2)
  }
  CV2[i]<-CV2[i]/n
}
plot(DF,CV2,type='b',xlab='df',ylab='CV error')

fit=lm(accel~ns(times,df=10),data=mcycle)
plot(mcycle$times,mcycle$accel)
lines(mcycle$times,fit$fitted.values)


# q4

fit=smooth.spline(mcycle$times,mcycle$accel,cv=TRUE)
dfopt<-fit$df
fit=smooth.spline(mcycle$times,mcycle$accel,df=dfopt)
plot(mcycle$times,mcycle$accel)
lines(fit$x,fit$y)

DF<-seq(9,17,0.5)
N<-length(DF)
CV3<-rep(0,N)
for(i in (1:N)){
  print(i)
  for(k in (1:K)){
    fit=smooth.spline(mcycle$times[folds!=k],mcycle$acce[folds!=k],df=DF[i])
    pred<-predict(fit,mcycle$times[folds==k])
    CV3[i]<-CV3[i]+ sum((mcycle$accel[folds==k]-pred$y)^2)
  }
  CV3[i]<-CV3[i]/n
}
plot(DF,CV3,type='b',xlab='df',ylab='CV error')

# q5

fit1=lm(accel~poly(times,10),data=mcycle)
fit2=lm(accel~ns(times,df=12),data=mcycle)
fit3=smooth.spline(mcycle$times,mcycle$accel,df=dfopt)

plot(mcycle$times,mcycle$accel)
lines(mcycle$times,fit1$fitted.values,col='red')
lines(mcycle$times,fit2$fitted.values,lty=1,col='blue')
lines(fit3$x,fit3$y,lty=1,col='green')

min(CV1)
min(CV2)
min(CV3)



#---------------- Exercice 2 ------------------
library(gam)
data(kyphosis)
attach(kyphosis)

# Q1

plot(kyphosis)
boxplot(Age ~Kyphosis)
boxplot(Number ~Kyphosis)
boxplot(Start ~Kyphosis)

stripchart(Age ~Kyphosis,method='jitter')
stripchart(Number ~Kyphosis,method='jitter')
stripchart(Start ~Kyphosis,method='jitter')


# Q2

# splines naturelles
fit<-gam(Kyphosis ~ ns(Age,2)+ ns(Number,2) + ns(Start,2),family='binomial',data=kyphosis,trace=TRUE)
plot(fit,se=TRUE)

# splines de lissage
fit<-gam(Kyphosis ~ s(Age,2)+ s(Number,3) + s(Start,3),family='binomial',data=kyphosis,trace=TRUE)
plot(fit,se=TRUE)

# Q3
N<-nrow(kyphosis)
DF1<-1:3
DF2<-1:3
DF3<-1:3
ERR<-array(0,c(3,3,3))
ERR1<-ERR
for(i in 1:N){
  print(i)
  for(j in 1:3){
    for(k in 1:3){
      for(ell in 1:3){
        fit<-gam(Kyphosis ~ s(Age,j)+ s(Number,k) + s(Start,ell),family='binomial',
                 data=kyphosis[-i,],trace=FALSE)
        pred<-predict(fit,newdata=kyphosis[i,],type='link')
        if(pred>0) ypred<-'present' else ypred<-'absent'
        err<- ypred !=  Kyphosis[i]
        ERR[j,k,ell]<-ERR[j,k,ell]+ err
        fit1<-gam(Kyphosis ~ ns(Age,j)+ ns(Number,k) + ns(Start,ell),family='binomial',
                  data=kyphosis[-i,],trace=FALSE)
        pred1<-predict(fit1,newdata=kyphosis[i,],type='link')
        if(pred1>0) ypred1<-'present' else ypred1<-'absent'
        err1<- ypred1 !=  Kyphosis[i]
        ERR1[j,k,ell]<-ERR1[j,k,ell]+ err1
      }
    }
  }
}

ERR<-ERR/N
ERR1<-ERR1/N

fit<-gam(Kyphosis ~ ns(Age,3)+ ns(Number,2) + ns(Start,1),family='binomial',data=kyphosis,trace=TRUE)
plot(fit,se=TRUE)
