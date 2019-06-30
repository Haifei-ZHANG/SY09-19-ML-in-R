mvdnorm <- function(X, mu, Sigma)
{
	X <- as.matrix(X)

	n <- dim(X)[1]
	p <- dim(X)[2]

	B <- chol(Sigma)
	U <- (X-matrix(rep(mu,n),nrow=n,byrow=T))%*%ginv(B)

	dens <- exp(-rowSums(U*U)/2) * (2*pi)^(-p/2) / det(B)
}
