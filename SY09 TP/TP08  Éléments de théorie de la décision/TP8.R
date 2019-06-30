#Implémentation du modèle génératif
generateData = function(n,pi,lambda,theta) {
  # Génération des appartenances aux populations
  n1 = rbinom(n=1,size = n,prob = pi[1])
  n2 = n - n1
  Z = array(0,dim = c(n,1))
  Z[1:n1] = 1
  Z[(n1+1):n] = 2
  # Génération des vecteurs aléatoires x1,..., xn1
  X = array(data = 0,dim = c(n,2))
  X[1:n1,1] = rexp(n1,lambda[1])
  X[1:n1,2] = rexp(n1,lambda[2])
  # Génération des vecteurs aléatoires xn1+1,..., xn
  X[(n1+1):n,1] = rexp(n2,theta[1])
  X[(n1+1):n,2] = rexp(n2,theta[2])
  data = NULL
  data$X = X
  data$Z = Z
  data$n1 = n1
  data$n2 = n2
  data$n = n
  data
}

# Paramètres des lois
lambda = c(1,2)
theta = c(2,4)
# Probabilité à priori
pi = c(0.6,0.4)
n = 1000
data = generateData(n,pi,lambda, theta)
X = data$X
Z = data$Z
n1 = data$n1
n2 = data$n2

front.bayes = function(X,Z,pi,lambda,theta) {
  C = - log(prod(theta)/prod(lambda) * pi[2]/pi[1])
  # Tracé de la droite
  vect = matrix(lambda - theta)
  a = - vect[1]/vect[2]
  b = C / vect[2]
  plot(X,col=c("orange","blue")[Z],main = paste("Simulation avec n=",n," points",sep = ""))
  abline(b,a)
}

front.bayes(X,Z,pi,lambda,theta)



#Éstimation du taux d’erreur de Bayes
C = - log(prod(theta)/prod(lambda) * pi[2]/pi[1])
vect = matrix(lambda - theta)
res = X %*% vect
# Classe w1 (z=0) si vect^T x =< C
inferedClass = (res > C) +1
# Erreur de classification
bayesError = sum(inferedClass != Z) / n
print(bayesError)

tauxError1 = sum((inferedClass != Z)[Z==1]) / n1
tauxError2 = sum((inferedClass != Z)[Z==2]) / n2
print(tauxError1)
print(tauxError2)


#Comparaison avec K plus proches voisins
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

separ1 <- function(X, z)
{
  ntot <- dim(X)[1]
  
  iapp <- sample(1:ntot, round(2/3*ntot))
  itst <- setdiff(1:ntot, iapp)
  
  Xapp <- X[iapp,]
  zapp <- z[iapp]
  Xtst <- X[itst,]
  ztst <- z[itst]
  
  out <- NULL
  out$Xapp <- Xapp
  out$zapp <- zapp
  out$Xtst <- Xtst
  out$ztst <- ztst
  
  out
}

dataKNN = separ1(data$X,data$Z)
KNN_pred = kppv.val(dataKNN$Xapp, dataKNN$zapp, K=14, dataKNN$Xtst)
KNN_erreur = mean(KNN_pred!=dataKNN$ztst)
print(KNN_erreur)
 