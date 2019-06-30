
library(MASS)
source("is.passage.r")

notes <- read.table("donnees/notes.txt", header=T)

A1 <- matrix(c(1/2,1/2,0,0,0,
				0,0,1/2,1/2,0,
				1/2,-1/2,0,0,0,
				0,0,1/2,-1/2,0,
				0,0,0,0,1),
				nrow=5)

B1 <- A1/matrix(rep(sqrt(apply(A1*A1, 2, sum)), 5), nrow=5, byrow=T)

A2 <- matrix(c(1/2,0,1/2,0,0,
               0,1/2,0,1/2,0,
               1/2,0,-1/2,0,0,
               0,1/2,0,-1/2,0,
               0,0,0,0,1),
             nrow=5)

B2 <- A2/matrix(rep(sqrt(apply(A2*A2, 2, sum)), 5), nrow=5, byrow=T)
