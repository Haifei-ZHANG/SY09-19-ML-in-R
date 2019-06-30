### Q1
iris = read.csv("donnees/iris.data",sep = '&',header = TRUE)
data(iris)
class(iris)
names(iris)
iris[, 1]
iris$Sepal.Length
class(iris[, 1])
class(iris$Species)
summary(iris)
apply(iris[, 1:4], 2, mean)
cor(iris[, 1:4])
print(cor(iris[, 1:4]), digits = 3)
plot(iris)
boxplot(iris)

def.par <- par(no.readonly = T)
par(mfrow = c(2, 2))
for (i in 2:5) {
  hist(iris[, i])
}
par(def.par)

barplot(summary(iris$Species))

x11()
#quartz() # ou x11()
plot(iris[, 1:4], col = c("red", "green", "blue")[iris$Species])
x11()
#quartz() # ou x11()
pairs(iris[, 1:4], main = "Iris de Fisher", pch = 21, bg = c("red", "green3", "blue")[iris$Species])

attach(iris)
# Histogrammes avec les espèces
inter <- seq(min(Petal.Length), max(Petal.Length), by = (max(Petal.Length) - min(Petal.Length))/10)
h1 <- hist(plot = F, Petal.Length[Species == "setosa"], breaks = inter)
h2 <- hist(plot = F, Petal.Length[Species == "versicolor"], breaks = inter)
h3 <- hist(plot = F, Petal.Length[Species == "virginica"], breaks = inter)
barplot(rbind(h1$counts, h2$counts, h3$counts), space = 0, legend = levels(Species), main = "LoPe", col = c("blue", "red", "yellow"))
# Graphique sur un fichier Postscript
postscript("exemple.eps", horizontal = F, width = 12/2.5, height = 12/2.5)
pairs(iris[2:5], main = "Les Iris", pch = 21, bg = c("red", "green3", "blue")[Species])
dev.off()
detach(iris)

hist.factor<-function(qualitative,quantitative){
  inter <- seq(min(quantitative), max(quantitative), by = (max(quantitative) - min(quantitative))/10)
  i <- 1
  hist <- c()
  while(!is.na(levels(qualitative)[i])){
    tmp <- hist(plot = F, quantitative[qualitative == levels(qualitative)[i]], breaks = inter)
    hist <- rbind(hist,tmp$counts)
    i <- i+1
  }
  mycolors = sample(length(colors()),i-1,replace = F)
  barplot(hist, space = 0, legend = levels(qualitative), main = "LoPe", col = mycolors)
}
hist.factor(iris$Species, iris$Petal.Length)
hist.factor(iris$Species, iris$Sepal.Length)


### Q2
sy02 = read.csv("donnees/median-sy02-p2014.csv",header = F)
names(sy02) <- c("branche","note")
sy02[sy02$note=='ABS','note'] <- NA
na <- which(is.na(sy02$note))
sy02 <- sy02[-na,]
levels(sy02$branche)
sy02$branche <- as.factor(substr(sy02$branche, 1, 2))
sy02$note <- as.numeric(as.character(sy02$note))
class(sy02$branche)
class(sy02$note)
hist.factor(sy02$branche, sy02$note)
summary(sy02)

boxplot(sy02$note~sy02$branche)
table(sy02$branche,sy02$note)

### Q3
babies <- read.table('donnees/babies23.data',head=T)
summary(babies)

names(sy02)<-c('branche','note')
sy02[sy02$note=='ABS',][2]<-NA
sy02_pas_na<-sy02[!is.na(sy02$note),]

sy02_pas_na$branche<-as.factor(substr(sy02_pas_na$branche, 1, 2))
sy02_pas_na$note<-as.numeric(as.character(sy02_pas_na$note))

hist.factor(sy02_pas_na$branche,sy02_pas_na$note)
