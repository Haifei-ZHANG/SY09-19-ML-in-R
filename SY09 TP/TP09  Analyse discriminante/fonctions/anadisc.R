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

		param$MCov[,,k] <- 
		param$mean[k,] <- 
		param$prop[k] <- 
	}

	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- adq.app(Xapp, zapp)

	MCov <- 
	for (k in 1:g)
	{
		MCov <- 
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
		param$MCov[,,k] <- 
	}

	param
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
		prob[,k] <- 
	}
	prob <- 
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
