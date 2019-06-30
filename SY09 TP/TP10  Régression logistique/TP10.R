#1.Régression logistique
log.app <- function(Xapp, zapp, intr, epsi)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  Xapp <- as.matrix(Xapp)
  
  if (intr == T)
  {
    Xapp <- cbind(rep(1,n),Xapp)
    p <- p + 1
  }
  
  targ <- matrix(as.numeric(zapp),nrow=n)
  targ[which(targ==2),] <- 0
  tXap <- t(Xapp)
  
  beta <- matrix(0,nrow=p,ncol=1)
  
  conv <- F
  iter <- 0
  while (conv == F)
  {
    iter <- iter + 1
    bold <- beta
    
    prob <- postprob(beta, Xapp)
    MatW <- diag(as.vector(prob) * as.vector((1-prob)))
    beta <- beta + ginv(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ-prob)
      
    if (norm(beta-bold)<epsi)
    {
      conv <- T
    }
  }
  
  prob <- postprob(beta, Xapp)
  out <- NULL
  out$beta <- beta
  out$iter <- iter
  out$logL <- sum(targ * log(prob) + (1-targ) * log(1-prob))
    
    out
}

log.val <- function(beta, Xtst)
{
  m <- dim(Xtst)[1]
  p <- dim(beta)[1]
  pX <- dim(Xtst)[2]
  
  Xtst <- as.matrix(Xtst)
  
  if (pX == (p-1))
  {
    Xtst  <- cbind(rep(1,m),Xtst)
  }
  
  prob <- postprob(beta,Xtst)
  prob <- cbind(prob,1-prob)
  pred <- max.col(prob)
  
  out <- NULL
  out$prob <- prob
  out$pred <- pred
  
  return(out)
}

postprob <- function(beta, X)
{
  X <- as.matrix(X)
  prob <-  exp(X%*%beta)/(1+exp(X%*%beta))
  return(prob)
}

prob.log <- function(param, X, z, niveaux)
{
  discretisation=50
  deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
  deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
  grille <- as.matrix(grille)
  
  # calcul des valeurs de la fonction 
  valf <- log.val(param, grille)$prob[,1]
  plot(X, col=c("red","green","blue","magenta","orange")[z])
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
}



#############################################################################################################
#############################################################################################################
#2.Régression logistique quadratique.

prob.log2 <- function(param, X, z, niveaux)
{
  discretisation=50
  deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
  deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  grille <- poly(as.matrix(grille), degree=2, raw=T)
  
  # calcul des valeurs de la fonction 
  valf <- log.val(param, grille)$prob[,1]
  plot(X, col=c("red","green","blue","magenta","orange")[z])
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
}
############################################################################################################
############################################################################################################
synth1_40 <- read.csv("./donnees/Synth1-40.csv")
X = synth1_40[,1:2]
Z = synth1_40[,3]
log.params <- log.app(X,Z,intr = T, epsi = 1e-5)
# Affichage des frontière de décisions
niv = 1:10 / 10.
library(MASS)
prob.log(param=log.params$beta, X=X, z=Z,niveaux = niv)

X_quadratique = poly(as.matrix(X),degree=2,raw = T)
log2.params <- log.app(X_quadratique,Z,intr = T, epsi = 1e-5)
prob.log2(param=log2.params$beta, X=X, z=Z,niveaux = niv)


############################################################################################################
############################################################################################################
#Application
source("./fonctions/separ1.R")

Pima <- read.csv("./donnees/Pima.csv")
Pima_separe <- separ1(scale(Pima[,1:7],center = T, scale = F),Pima[,8])
Pima_params <- log.app(Pima_separe$Xapp,Pima_separe$zapp,intr = T, epsi = 1e-5)
Pima_pred <- log.val(Pima_params$beta,Pima_separe$Xtst)
cat("error on Pima is ",1-sum(diag(table(Pima_pred$pred, Pima_separe$ztst)))/length(Pima_separe$ztst))

bcw <- read.csv("./donnees/bcw.csv")
bcw_separe <- separ1(bcw[,1:9],bcw[,10])
bcw_params <- log.app(bcw_separe$Xapp,bcw_separe$zapp,intr = T, epsi = 1e-5)
bcw_pred <- log.val(bcw_params$beta,bcw_separe$Xtst)
cat("error on bcw is ",1-sum(diag(table(bcw_pred$pred, bcw_separe$ztst)))/length(bcw_separe$ztst))
 