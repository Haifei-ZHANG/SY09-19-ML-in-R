distXY <- function(X, Y) {
  nx <- dim(X)[1]
  ny <- dim(Y)[1]
  h.x <- rowSums(X^2)
  h.y <- rowSums(Y^2)
  ones.x <- rep(1, nx)
  ones.y <- rep(1, ny)
  D2xy <- h.x %*% t(ones.y) - 2 * X %*% t(Y) + ones.x %*% t(h.y)
}


kppv.val <- function(Xapp, zapp, K, Xtst)
{
  Xapp <- as.matrix(Xapp)
  zapp <- as.numeric(zapp)
  Xtst <- as.matrix(Xtst)
  
  napp <- dim(Xapp)[1]
  ntst <- dim(Xtst)[1]
  p <- dim(Xapp)[2]
  g <- max(zapp)
  
  # calcul des distances 
  d2 <- distXY(Xtst, Xapp)
  d2sor <- t(apply(d2, 1, sort))
  
  
  # distance seuil (distance au Kieme plus proche voisin) 
  seuil <- d2sor[,K]
  
  # plus proches voisins 
  is.ppv <- (d2<=matrix(rep(seuil,napp),nrow=ntst,byrow=F))
  # classes des K plus proches voisins
  cl.ppv <- matrix(rep(zapp,ntst),nrow=ntst,byrow=T)*is.ppv
  
  scores <- matrix(0,nrow=ntst,ncol=g)
  for (k in 1:g)
  {
    scores[,k] <- apply(cl.ppv==k,1,sum)
  }
  
  # classement en fonction du centre le plus proche 
  return (zpred <- apply(scores, 1, which.max))
}


kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
  taux <- rep(0,length(nppv))
  
  ind <- 0
  for (k in nppv)
  {
    ind <- ind+1
    zval.pred <- kppv.val(Xapp, zapp, k, Xval)
    taux[ind] <- mean(zval.pred==zval) 
  }
  return (kopt <- nppv[which.max(taux)])
}

front.kppv <- function(X, z, K, discretisation=50)
{
  deltaX <- (max(X[,1])-min(X[,1]))/discretisation
  deltaY <- (max(X[,2])-min(X[,2]))/discretisation
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
  valf <- kppv.val(X, z, K, grille)
  plot(X, col=c("red","green","blue","magenta","orange")[z], asp=1)
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}

separ2 <- function(X, z)
{
  g <- max(z)
  
  Xapp <- NULL
  zapp <- NULL
  Xval <- NULL
  zval <- NULL
  Xtst <- NULL
  ztst <- NULL
  
  for (k in 1:g)
  {
    indk <- which(z==k)
    ntot <- length(indk)
    napp <- round(ntot/2)
    nval <- round(ntot/4)
    ntst <- ntot-napp-nval
    
    itot <- sample(indk)
    iapp <- itot[1:napp]
    ival <- itot[(napp+1):(napp+nval)]
    itst <- itot[(napp+nval+1):ntot]
    
    Xapp <- rbind(Xapp, X[iapp,])
    zapp <- c(zapp, z[iapp])
    Xval <- rbind(Xval, X[ival,])
    zval <- c(zval, z[ival])
    Xtst <- rbind(Xtst, X[itst,])
    ztst <- c(ztst, z[itst])
  }
  
  res <- NULL
  res$Xapp <- Xapp
  res$zapp <- zapp
  res$Xval <- Xval
  res$zval <- zval
  res$Xtst <- Xtst
  res$ztst <- ztst
  
  res
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


Xapp <- read.csv("./donnees/Synth1-1000.csv")
zapp <- Xapp$z
Xapp <- Xapp[,-3]
Xval <- read.csv("./donnees/Synth1-500.csv")
zval <- Xval$z
Xval <- Xval[,-3]
Xtst <- read.csv("./donnees/Synth1-100.csv")
ztst <- Xtst$z
Xtst <- Xtst[,-3]

Kopt <- kppv.tune(Xapp, zapp, Xval, zval, seq(from=1,to=100,by=2))
front.kppv(Xapp, zapp, Kopt, 20)
ztst.pred <- kppv.val(Xapp, zapp, Kopt, Xtst)
table(ztst,ztst.pred)



syn40 <- read.csv("./donnees/Synth1-40.csv")
syn100 <- read.csv("./donnees/Synth1-100.csv")
syn500 <- read.csv("./donnees/Synth1-500.csv")
syn1000 <- read.csv("./donnees/Synth1-1000.csv")

data <- separ2(syn1000[,-3],syn1000[,3])
Kopt <- kppv.tune(data$Xapp, data$zapp, data$Xval, data$zval, seq(from=1,to=100,by=2))
ztst.pred <- kppv.val(data$Xapp, data$zapp, Kopt, data$Xtst)
table(data$ztst,ztst.pred)

kseq <- rep(0,20)
for( i in c(1:20)){
  data <- separ2(syn1000[,-3],syn1000[,3])
  Kopt <- kppv.tune(data$Xapp, data$zapp, data$Xval, data$zval, seq(from=1,to=100,by=2))
  kseq[i] <- Kopt
}
Kopt <- getmode(kseq)
