adq.app <- function(Xapp, zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  g <- max(unique(zapp))
  
  param <- NULL
  param$MCov <- array(0, c(p,p,g))
  param$mean <- array(0, c(g,p))
  param$prop <- rep(0, g)
  
  for (k in 1:g)
  {
    indk <- which(zapp==k)
    X_k = Xapp[indk,]
    param$MCov[,,k] <- cov(X_k)
    param$mean[k,] <- colMeans(X_k)
    param$prop[k] <- length(indk)/n
  }
  
  param
}

adl.app <- function(Xapp, zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  g <- max(unique(zapp))
  
  param <- adq.app(Xapp, zapp)
  MCov <- array(0, c(p,p))
  for (k in 1:g)
  {
    MCov <- MCov + param$prop[k]*param$MCov[,,k]
  }
  param$MCov <- array(rep(MCov,g), dim=c(p,p,g))
  
  param
}

nba.app <- function(Xapp, zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  g <- max(unique(zapp))
  
  param <- adq.app(Xapp, zapp)
  
  for (k in 1:g)
  {
    param$MCov[,,k] <-  diag(diag(param$MCov[,,k]))
  }
  
  param
}

mvdnorm <- function(X, mu, Sigma)
{
  X <- as.matrix(X)
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  B <- chol(Sigma)
  U <- (X-matrix(rep(mu,n),nrow=n,byrow=T))%*%ginv(B)
  
  dens <- exp(-rowSums(U*U)/2) * (2*pi)^(-p/2) / det(B)
}

ad.val <- function(param, Xtst)
{
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  g <- length(param$prop)
  
  out <- NULL
  
  prob <- matrix(0, nrow=n, ncol=g)
  
  for (k in 1:g)
  {
    mu_k = param$mean[k,]
    MCov_k = param$MCov[,,k]
    prop_k = param$prop[k]
    prob[,k] = prop_k * mvdnorm(Xtst, mu_k, MCov_k)
  }

  prob = prob / rowSums(prob)
  pred = max.col(prob)
  
  
  out$prob <- prob
  out$pred <- pred
  
  out
}


prob.ad <- function(param, X, z, niveaux)
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
  
  # calcul des valeurs de la fonction 
  valf <- ad.val(param, grille)$prob[,1]
  plot(X, col=c("red","green","blue","magenta","orange")[z])
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
}


synth1_40 <- read.csv("./donnees/Synth1-40.csv")

X = synth1_40[,1:2]
z = synth1_40[,3]
# Apprentissage sur tout le jeu de données
adq.params = adq.app(Xapp = X, zapp = z)
adl.params = adl.app(Xapp = X, zapp = z)
nba.params = nba.app(Xapp = X, zapp = z)
# Affichage des frontière de décisions
niv = 1:10 / 10.
library(MASS)
prob.ad(param = adq.params, X = X, z = z,niveaux = niv)
prob.ad(param = adl.params, X = X, z = z,niveaux = niv)
prob.ad(param = nba.params, X = X, z = z,niveaux = niv)


#Application
source("./fonctions/separ1.R")
source("./fonctions/separ2.R")
synth1_1000 <- read.csv("./donnees/Synth1-1000.csv")
synth2_1000 <- read.csv("./donnees/Synth2-1000.csv")
synth3_1000 <- read.csv("./donnees/Synth3-1000.csv")

ad.models = function(X,z, model, niter=20) {
  #’ Détermination des erreurs pour un model donné
  #’
  #’ @param X : jeu de données
  #’ @param z : les étiquettes du jeu de données
  #’ @param model : modèle à utiliser
  #’ @param niter : le nombre d’estimation à réaliser
  #’ @param test_size : proportion à considérer pour l’ensemble de test
  error = function(z1,z2) {
    #’ Calcule l’erreur de classification entre deux
    #’ vecteurs d’étiquettes.
    misClassified = 1 * (z1 != z2)
    sum(misClassified) / length(z1)
  }
  errApp = c()
  errTest = c()
  for (i in 1:niter) {
    donn = separ1(X,z)
    Xapp = donn$Xapp
    Xtst = donn$Xtst
    zapp = donn$zapp
    ztst = donn$ztst
    params = model(Xapp,zapp)
    zappClass = ad.val(params,Xapp)$pred
    ztstClass = ad.val(params,Xtst)$pred
    errApp[i] = error(zapp,zappClass)
    errTest[i] = error(ztst,ztstClass)
  }
  estErrApp = mean(errApp)
  estErrTest = mean(errTest)
  # Objet de retour
  errors = NULL
  errors$estErrApp = estErrApp
  errors$estErrTest = estErrTest
  errors$errApp = errApp
  errors$errTest = errTest
  errors
}

dataSets = c("Synth1-1000","Synth2-1000","Synth3-1000")
loadData = function(dataSet) {
  #’ Charge un jeu de données de deux dimensions
  dataSetName = paste("./donnees/",dataSet,".csv",sep = "")
  donn = read.csv(dataSetName)
  p = dim(donn)[2] -1
  X = donn[,1:p]
  z = donn[,p+1]
  data = NULL
  data$X = X
  data$z = z
  data
}

for(i in 1:length(dataSets)) {
  dataSet = dataSets[i]
  data = loadData(dataSet)
  X = data$X
  z = data$z
  # Apprentissage avec (2/3,1/3)
  adl.res = ad.models(X,z,adl.app)
  adq.res = ad.models(X,z,adq.app)
  nba.res = ad.models(X,z,nba.app)
  list.err = list(adl.err.app = adl.res$errApp,
                  adl.err.test = adl.res$errTest,
                  adq.err.app = adq.res$errApp,
                  adq.err.test = adq.res$errTest,
                  nba.err.app = nba.res$errApp,
                  nba.err.test = nba.res$errTest)
  boxplot(list.err,main = paste("Erreur sur",dataSet), las=2)
}


for(i in 1:length(dataSets)) {
  dataSet = dataSets[i]
  data = loadData(dataSet)
  X = data$X
  z = data$z
  levels = c(0.1, 0.3, 0.45,0.5, 0.55,0.7, 0.9)
  adl.params = adl.app(X, z)
  adq.params = adq.app(X, z)
  nba.params = nba.app(X, z)
  prob.ad(adl.params, X,z, levels)
  prob.ad(adq.params, X,z, levels)
  prob.ad(nba.params, X,z, levels)
}
