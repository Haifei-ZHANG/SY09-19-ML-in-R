# Exercise 1
#------------------------------
# Question a
Pi<-0.90
miu<-2
sigma<- 1
a=10;
c<-1/(2*a)
n<- 100

component <- sample(0:1, prob = c(1-Pi,Pi), size = n, replace = TRUE)
x<-vector("numeric",n)
y<-component

for(i in 1:n){
  if(component[i] == 1)
    x[i] <- rnorm(1,miu,sigma)
  else
    x[i] <- runif(1,-a,a)
}
boxplot(x)
dotchart(x)


#------------------------------
# Question b
# Observed data Loglikelihood
loglik<- function(theta,x){
  phi <- sapply(x,dnorm,mean=theta[1],sd=theta[2])
  logL <- sum(log(theta[3]*phi+(1-theta[3])*c))
  return(logL)
}

# EM algoritm
em_outlier <- function(x,theta0,a,epsi){
  go_on<-TRUE
  logL0<- loglik(theta0,x)
  t<-0
  c<-1/(2*a)
  n<-length(x)
  print(c(t,logL0))
  while(go_on){
    t<-t+1
    # E-step
    phi <- sapply(x,dnorm,mean=theta0[1],sd=theta0[2])
    y<-  phi*theta0[3]/(phi*theta0[3]+c*(1-theta0[3]))
    # M-step
    S<- sum(y)
    pi<-S/n
    miu<- sum(x*y)/S
    sigma<-sqrt(sum(y*(x-miu)^2)/S)
    theta<-c(miu,sigma,pi)
    logL<-loglik(theta,x) 
    if (logL-logL0 < epsi){
      go_on <- FALSE
    }
    logL0 <- logL
    theta0<-theta
    print(c(t,logL))
  }
  return(list(loglik=logL,theta=theta,y=y))
}


#------------------------------
# Question c

miu0<-mean(x) 
sigma0<-sd(x)
Pi0<-0.5
epsi<-1e-8
theta0<-c(miu0,sigma0,Pi0)
estim<-em_outlier(x,theta0,a,epsi)

plot(x,1-estim$y)


#------------------------------
# Question d

opt<-optim(theta0, loglik, gr = NULL,method = "BFGS", 
           hessian = FALSE,control=list(fnscale=-1),x=x)

print(opt$par)     # estimates computed with optim
print(estim$theta)  # EM





#my methode to realise the EM for GMM
#there are some problem on the loglike
#------------------------------------
Estep <- function(x,C,Pi,miu,sigma){
  y <- dnorm(x,miu,sigma)*Pi/(dnorm(x,miu,sigma)*Pi+C*(1-Pi))
  return (y)
}


Mstep <- function(x,y,miu){
  N1 <- sum(y)
  Pi <- N1/n
  miu <- sum(y*x)/N1
  sigma <- sqrt(sum(y*((x-miu)^2))/N1)
  return(list(Pi,miu,sigma))
}

EM <- function(x,y,C,miu,sigma,Pi,epsi=1e-8){
  LogLikPre = 0
  LogLikNext = 1
  while(abs(LogLikPre-LogLikNext)>epsi){
    LogLikPre <- LogLikNext
    y <- Estep(x,C,Pi,miu,sigma)
    paramettres <- unlist(Mstep(x,y,miu))
    LThetaNext <- sum(paramettres[1]*dnorm(x,paramettres[2],paramettres[3])+(1-paramettres[1])*C)
  }
  return(list(LogLik=LogLikNext,theta=paramettres,y=y))
}

myEstim <- EM(x,y,c,miu0,sigma0,Pi0)
plot(x,1-myEstim$y)






# Exercise 2
#------------------------------
ecoli <- read.table("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP5/E.coli/ecoli.txt")
wine <- read.table("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP5/Wine/wine.txt")
seeds <- read.table("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP5/Seeds/seeds.txt")
names(ecoli) <- c('sequenceName','mcg','gvh','lip','chg','aac','alm1','alm2','y')
names(wine) <- c('y','alcohol','malicAcid','ash','alcalinityOfAsh','magnesium','totalPhenols','flavanoids','nonflavanoidPhenols','proanthocyanins','colorIntensity','hue','OD280/OD315OfDilutedWines','proline')
names(seeds) <- c('area', 'perimeter', 'compactness', 'length', 'width', 'asymmetryCoefficient','lengthGroove','y')
summary(ecoli)
summary(wine)
summary(seeds)
library(mclust)
plot(ecoli)
ecoliMclust <- Mclust(ecoli[,1:8])
summary(ecoliMclust)
plot(ecoliMclust)

wineMclust <- Mclust(wine[,2:14])
summary(wineMclust)
plot(wineMclust)
table(wine[,1],wineMclust$classification)

seedsMclust <- Mclust(seeds[1:7])
summary(seedsMclust)
plot(seedsMclust)
