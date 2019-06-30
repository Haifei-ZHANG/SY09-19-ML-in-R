kppv.val <- function(Xapp, zapp, K, Xtst)
{
	Xapp <- as.matrix(Xapp)
	Xtst <- as.matrix(Xtst)

	napp <- dim(Xapp)[1]
	ntst <- dim(Xtst)[1]
	p <- dim(Xapp)[2]

	# calcul des distances 
	d2 <- distXY(Xtst, Xapp)
	d2sor <- t(apply(d2, 1, sort))

	# distance seuil pour chaque individu de l'ensemble 
	# de test (distance a son Kieme plus proche voisin) 
	seuil <- d2sor[,K]

	# identification des plus proches voisins 
	is.ppv <- (d2<=matrix(rep(seuil,napp),nrow=ntst,byrow=F))

  # valeur de f pour les K plus proches voisins
  z.kppv <- matrix(rep(as.numeric(zapp),ntst),nrow=ntst,byrow=T)*is.ppv

  g <- nlevels(zapp)
  scores <- matrix(0,nrow=ntst,ncol=g)
  for (k in 1:g)
	{
		scores[,k] <- apply(z.kppv==k,1,sum)
  }

  # classement dans la classe majoritaire 
  zpred <- as.factor(apply(scores, 1, which.max))
}
