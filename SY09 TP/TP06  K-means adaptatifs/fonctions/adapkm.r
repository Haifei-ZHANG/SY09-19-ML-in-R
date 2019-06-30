adapkm <- function(X, K=2, rhok = rep(1,K), iter.max=100, nstart=1, epsi=1e-5)
{
  X <- as.matrix(X)

  n <- dim(X)[1]
  p <- dim(X)[2]

  best.dtot <- Inf

  for (iess in 1:nstart)
  {
    muk <- X[sample(1:n,K),]

    # initialisation : utilisation de la distance euclidienne 
    Sig <- array(rep(diag(rep(1,p)+1e-5),K), dim=c(p,p,K))

    diff <- epsi+1
    iter <- 0

    while ((diff>epsi)&(iter<=iter.max))
    {
      iter <- iter+1

      dMah <- matrix(0, nrow=n, ncol=K)
      for (k in 1:K)
      {
        dMah[,k] <- 
      }

      clus <- 

      mukold <- muk

      for (k in 1:K)
      {
        muk[k,] <- 
        Sig[,,k] <- 
        # regularisation 
#        Sig[,,k] <- Sig[,,k] + diag(p)*1e-5*mean(diag(Sig[,,k]))
        # normalisation 
        Sig[,,k] <- 
      }

      dtot <- 0
      for (k in 1:K)
      {
        dtot <- dtot + 
      }

      diff <- 
    }

    if (dtot<best.dtot)
    {
      best.dtot <- dtot
      best.iter <- iter
      best.clus <- clus
      best.muk <- muk
      best.Sig <- Sig
    }
  }

  outp <- NULL
  outp$dtot <- best.dtot
  outp$iter <- best.iter
  outp$cluster <- best.clus
  outp$centers <- best.muk
  outp$MatCov <- best.Sig
  outp
}
