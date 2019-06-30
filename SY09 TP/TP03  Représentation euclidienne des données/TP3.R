notes <- read.table("donnees/notes.txt", header=T)

plot(math~scie, data=notes, pch=20, asp=1)
text(notes[,c("scie","math")], row.names(notes), pos=1)

plot(fran~lati, data=notes, pch=20, asp=1)
text(notes[,c("lati","fran")], row.names(notes), pos=1)

plot(notes$math)
text(notes[,c("math")], row.names(notes), pos=1)

moyen_sci <- (notes$math+notes$scie)/2
moyen_lit <- (notes$fran+notes$lati)/2

plot(moyen_sci,moyen_lit, pch=20, asp=1)
plot(notes)
boxplot(notes)

hist(notes$math)
hist(notes$scie)
hist(notes$fran)
hist(notes$lati)
hist(notes$d.m)

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

notes1 <- as.matrix(notes)%*%A1
colnames(notes1)<-c("moyen_matscie","moyen_franlati","diff_mathscie","diff_franlati","d.m")
plot(notes1[,c(1,2)], pch=20, asp=1)

notes_revnir <- nouvelles_notes%*%solve(A1)
colnames(notes_revnir)<-c("math","scie","fran","lati","d.m")


notes2 <- as.matrix(notes)%*%B1
notes3 <- as.matrix(notes)%*%B2

plot(as.data.frame(notes2))
plot(as.data.frame(notes3))
plot(notes)

cov_notes <- cov(notes)*(length(notes[,1])-1)/length(notes[,1])
inertie_notes <- sum(diag(cov_notes))

cov_notes1 <- cov(notes1)*(length(notes1[,1])-1)/length(notes1[,1])
inertie_notes1 <- sum(diag(cov_notes1))

cov_notes2 <- cov(notes2)*(length(notes2[,1])-1)/length(notes2[,1])
inertie_notes2 <- sum(diag(cov_notes2))

cov_notes3 <- cov(notes3)*(length(notes3[,1])-1)/length(notes3[,1])
inertie_notes3 <- sum(diag(cov_notes3))

plot(notes2[,c(1,2)], pch=20, asp=1)
plot(notes3[,c(1,3)], pch=20, asp=1)
