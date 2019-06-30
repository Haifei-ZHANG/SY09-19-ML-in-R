front.kppp <- function(X, z, Xpr, zpr, K, discretisation=50)
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
    valf <- kppv.val(Xpr, zpr, K, grille)
    plot(X, col=c("red","green","blue","magenta","orange")[z], asp=1)
    points(Xpr, col=c("red","green","blue","magenta","orange")[zpr], pch=19)
    contour(grilleX, grilleY, matrix(as.numeric(valf),nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
