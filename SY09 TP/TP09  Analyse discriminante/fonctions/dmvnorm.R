dmvnorm <- function (x, mu, Sigma, log = FALSE, tol = 1e-06) {
  if (is.vector(x)) 
    x = t(as.matrix(x))
  n = length(mu)
  if (is.vector(mu)) {
    p <- length(mu)
    if (is.matrix(x)) {
      mu <- matrix(rep(mu, nrow(x)), ncol = p, byrow = TRUE)
    }
  }
  else {
    p <- ncol(mu)
  }
  if (!all(dim(Sigma) == c(p, p)) || nrow(x) != nrow(mu)) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE) 
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1]))) 
    stop("Sigma is not positive definite")
  z <- t(x - mu)
  logdetS <- try(determinant(Sigma, logarithm = TRUE)$modulus,
                 silent=TRUE)
  attributes(logdetS) <- NULL
  iS <- MASS::ginv(Sigma)
  ssq <- diag(t(z) %*% iS %*% z)
  loglik <- -(n * (log(2*pi)) +  logdetS + ssq)/2
  if (log) return(loglik) else return(exp(loglik))
}
