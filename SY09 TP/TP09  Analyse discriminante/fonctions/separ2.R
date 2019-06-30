separ2 <- function(X, z)
{
  ntot <- dim(X)[1]

  iapp <- sample(1:ntot, round(1/2*ntot))
  ival <- sample(setdiff(1:ntot,iapp), round(1/4*ntot))
  itst <- setdiff(1:ntot, c(iapp,ival))

  Xapp <- X[iapp,]
  zapp <- z[iapp]
  Xval <- X[ival,]
  zval <- z[ival]
  Xtst <- X[itst,]
  ztst <- z[itst]

  out <- NULL
  out$Xapp <- Xapp
  out$zapp <- zapp
  out$Xval <- Xval
  out$zval <- zval
  out$Xtst <- Xtst
  out$ztst <- ztst

  out
}
