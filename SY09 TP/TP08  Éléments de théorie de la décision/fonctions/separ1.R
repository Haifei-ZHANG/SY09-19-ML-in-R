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
