#notes <- read.table("donnees/notes.txt", header=T)
notes = read.csv("donnees/sy02-p2016.csv",header = TRUE)
summary(notes)
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
median <- merge(moy.median, std.median)
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
final <- merge(moy.final, std.final)
correcteurs <- merge(median, final, all=T)

corr.acp <- correcteurs[-c(2,8),]

#ACP avec R
notes_quantitives <- notes[,c(3,6,8,10)]
sum(is.na(notes_quantitives$niveau))
notes_quantitives[is.na(notes_quantitives$note.median),"note.median"] <- mean(notes_quantitives$note.median[-which(is.na(notes_quantitives$note.median))])
sum(is.na(notes_quantitives$note.median))
notes_quantitives$note.median <- notes_quantitives$note.median-mean(notes_quantitives$note.median)
notes_quantitives[is.na(notes_quantitives$note.final),"note.final"] <- mean(notes_quantitives$note.final[-which(is.na(notes_quantitives$note.final))])
sum(is.na(notes_quantitives$note.final))
notes_quantitives$note.final <- notes_quantitives$note.final-mean(notes_quantitives$note.final)
notes_quantitives[is.na(notes_quantitives$note.totale),"note.totale"] <- mean(notes_quantitives$note.totale[-which(is.na(notes_quantitives$note.totale))])
sum(is.na(notes_quantitives$note.totale))
notes_quantitives$note.totale <- notes_quantitives$note.totale-mean(notes_quantitives$note.totale)
cov_notes <- cov(notes_quantitives,notes_quantitives)

eigen_decomposition <- eigen(cov_notes)
valeurs_p <- eigen_decomposition$values
vecteur_p <- eigen_decomposition$vectors
C <- as.matrix(notes_quantitives)%*%vecteur_p

I1 <- valeurs_p[1]/sum(valeurs_p)
I2 <- valeurs_p[2]/sum(valeurs_p)
I3 <- valeurs_p[3]/sum(valeurs_p)
I4 <- valeurs_p[4]/sum(valeurs_p)

E1 <- valeurs_p[1]/sum(valeurs_p)
E2 <- sum(valeurs_p[c(1,2)])/sum(valeurs_p)
E3 <- sum(valeurs_p[c(1:3)])/sum(valeurs_p)
E4 <- sum(valeurs_p[c(1:4)])/sum(valeurs_p)

plot(C[,1],C[,2])

plot(notes_quantitives[,1],notes_quantitives[,2])

representation_variables <- cov(notes_quantitives,C)
plot(representation_variables[,1],representation_variables[,2])
biplot(representation_variables[,c(1,2)],representation_variables[,c(3,4)])

X1 <- C[,1]%*%t(vecteur_p[,1])
X2 <- C[,c(1,2)]%*%t(vecteur_p[,c(1,2)])
X3 <- C[,c(1,2,3)]%*%t(vecteur_p[,c(1,2,3)])
X4 <- C%*%t(vecteur_p)

prcomp(notes_quantitives,scale=T)
princomp(notes_quantitives)



library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
summary(crabsquant)
plot(crabsquant)
crabsquant[,1] <- crabsquant[,1]/(crabsquant[,2]+crabsquant[,1])
crabsquant[,3] <- crabsquant[,3]/(crabsquant[,4]+crabsquant[,3])
crabsquant[,5] <- crabsquant[,5]/(crabsquant[,1]+crabsquant[,2]+crabsquant[,3]+crabsquant[,4])
#crabsquant <- crabsquant[,-2]
#crabsquant <- crabsquant[,-4]
#cor??????????????????,cov???????????????
crab_cor <- cor(crabsquant,method = "pearson")
cor.test(crabsquant[,1],crabsquant[,2])
cor.test(crabsquant[,3],crabsquant[,4])
t.test(crabsquant[,3],crabsquant[,4])

crab_prcomp <- prcomp(crabsquant,scale=T)
crab_princomp <- princomp(crabsquant)
plot(as.data.frame(crab_prcomp$x))
plot(as.data.frame(crab_princomp$scores))


#Pima
Pima <- read.csv("donnees/Pima.csv", header=T)
Pima_z <- Pima$z
summary(Pima)
plot(Pima[,])
a <- list((1:nlevels(Pima$z))[Pima$z],"red")
class(a) <- c("my_class",class(a))
class(a)
"my_class"<- function(x,i) x[[i]]
biplot(Pima[,1],Pima[,2],col=a)



#
mut <- read.csv("donnees/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
repe <- cmdscale(mut,k=2,eig=TRUE)
repe$points
repe$eig
plot(repe$points)
text(repe$points,labels = row.names(mut))
plot(Shepard(mut,repe$points))
abline(0,1)
