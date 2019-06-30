check <- function(data) {
   stopifnot(colnames(data)[1] == "Sepal.Length",
             colnames(data)[2] == "Sepal.Width",
             colnames(data)[3] == "Petal.Length",
             colnames(data)[4] == "Petal.Width",
             colnames(data)[5] == "Species",
             nrow(data) == 150,
             ncol(data) == 5,
             is.numeric(data[,1]),
             is.numeric(data[,2]),
             is.numeric(data[,3]),
             is.numeric(data[,4]),
             is.factor(data[,5]))
    print("Chargement OK")
}
### Q1 Chargement d'un jeu de données
iris1 = read.csv("donnees/iris1.data",sep = '&',header = TRUE)
iris2 = read.delim("donnees/iris2.data",header = TRUE)
iris3 = read.csv("donnees/iris3.data",sep = ';')
iris4 = read.table("donnees/iris4.data",header = T)
iris5 = read.csv("donnees/iris5.data",sep = ',',header = TRUE)
iris5[,4] <- as.numeric(iris5[,4])
check(iris1)
check(iris2)
check(iris3)
check(iris4)
check(iris5)

### Q2 Conversion de types
class(iris1[,1])
class(iris1[,1])

class(read.table(text='TRUE')[,1])
class(read.table(text='F')[,1])
class(read.table(text='A')[,1])
class(read.table(text='3.14')[,1])
class(read.table(text='2')[,1])

class(read.table(text=as.character("a"))[,1])
sapply(iris, class)

load('donnees/iris.Rdata')
sapply(iris6, class)
check(iris6)
iris6[,4] <- as.numeric(iris6[,4])
check(iris6)

### Q3 Recodage de facteurs
sy02 = read.csv("donnees/sy02-p2016.csv")
sapply(sy02,class)
sy02$resultat <- factor(sy02$resultat,levels = c("ABS","F","FX","E","D","C","B","A"), ordered = TRUE)


### Q4 Valeurs spéciales
10^(10^10)
1/0
0/0


### Q5 Chargement du jeu de données babies
babies <- read.table('donnees/babies23.data',head=T)
names(babies)[c(7, 5, 8, 10, 12, 13, 21, 11)]<- c("bwt", "gestation", "parity", "age", "height", "weight","smoke", "education")
hist(babies$age)
babies[babies$bwt==999, "bwt"] <- NA
babies[babies$gestation==999, "gestation"] <- NA
babies[babies$age==99, "age"] <- NA
babies[babies$height==99, "height"] <- NA
babies[babies$weight==999, "weight"] <- NA
babies[babies$smoke==9, "smoke"] <- NA
babies[babies$education==9, "education"] <- NA
babies$smoke <- as.factor(babies$smoke)
smoke <- which(levels(babies$smoke)==1)
levels(babies$smoke)[smoke]<-"Smoking"
levels(babies$smoke)[-smoke]<- "NonSmoking"
