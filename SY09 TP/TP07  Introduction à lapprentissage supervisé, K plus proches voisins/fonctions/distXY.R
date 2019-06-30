distXY <- function(X, Y) {
    nx <- dim(X)[1]
    ny <- dim(Y)[1]
    h.x <- rowSums(X^2)
    h.y <- rowSums(Y^2)
    ones.x <- rep(1, nx)
    ones.y <- rep(1, ny)
    D2xy <- h.x %*% t(ones.y) - 2 * X %*% t(Y) + ones.x %*% t(h.y)
}
