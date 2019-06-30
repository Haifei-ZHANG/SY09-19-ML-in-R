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
