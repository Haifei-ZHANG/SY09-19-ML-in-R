is.passage <- function(M)
{
    dimM <- dim(M)
    flag <- matrix(1,nrow=dimM[1],ncol=dimM[1])

    for (i in 1:(dimM[1]-1))
    {
        for (j in (i+1):dimM[1])
        {
            flag[i,j] <- sum(M[,i]*M[,j])
            flag[j,i] <- flag[i,j]
        }
    }

    print(flag)
}