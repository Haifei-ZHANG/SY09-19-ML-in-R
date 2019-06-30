synth1 <- read.csv("./donnees/Synth1.csv")
synth2 <- read.csv("./donnees/Synth2.csv")
synth3 <- read.csv("./donnees/Synth3.csv")

X <- read.csv("./donnees/Synth1.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]

distXY <- function(X, Y, M=diag(dim(X)[2]))
{
  if (!is.matrix(X))
  {
    X <- matrix(X, nrow=1)
  }
  if (!is.matrix(Y))
  {
    Y <- matrix(Y, nrow=1)
  }
  
  nx <- dim(X)[1]
  ny <- dim(Y)[1]
  h.x <- rowSums((X%*%t(chol(M)))^2)
  h.y <- rowSums((Y%*%t(chol(M)))^2)
  ones.x <- rep(1, nx)
  ones.y <- rep(1, ny)
  
  D2xy <- h.x %*% t(ones.y) - 2 * X %*% M %*% t(Y) + ones.x %*% t(h.y)
}


adapkm <- function(X, K=2, rhok = rep(1,K), iter.max=100, nstart=1, epsi=1e-1)
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
        dMah[,k] <- distXY(X,muk[k,],Sig[,,k])
      }
      
      clus <- apply(dMah,1, which.min)
        
        mukold <- muk
      
      for (k in 1:K)
      {
        muk[k,] <- mean(X[which(clus==k),])
          Sig[,,k] <- cov.wt(X[which(clus==k),],method = "ML")$cov
          # regularisation 
          #        Sig[,,k] <- Sig[,,k] + diag(p)*1e-5*mean(diag(Sig[,,k]))
          # normalisation 
          Sig[,,k] <- ((rhok[k]*det(Sig[,,k]))^(-1/p))*Sig[,,k]
      }
      
      dtot <- 0
      for (k in 1:K)
      {
        dtot <- dtot + sum((X[which(clus==k),]-muk[k])^2)
      }
      
      diff <- sqrt(sum((muk-mukold)^2)) 
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

adapkm(X)
