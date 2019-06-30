#1.1
mutation <- read.csv("donnees/mutations2.csv")
h1 <- hclust(dist(mutation), method = "average")
plot(h1)
h2 <- hclust(dist(mutation), method = "centroid")
h3 <- hclust(dist(mutation), method = "median")
h4 <- hclust(dist(mutation), method = "ward.D2")
plot(h2)
plot(h3)
plot(h4)
cutree(h4,k=2)
cutree(h4,k=3)

#1.2
library(datasets)
iris_hclust <- hclust(dist(iris[,1:4]), method = "ward.D2")
plot(iris_clust)
iris_result <- cutree(iris_hclust, k=3)
iris_result[iris_result==1] <- "setosa"
iris_result[iris_result==2] <- "versicolor"
iris_result[iris_result==3] <- "virginica"
iris_result
table(iris_result,iris[,5])

#1.3
library(cluster)
iris_diana <- diana(iris[,1:4])
plot(iris_diana)
iris_result1 <- cutree(iris_diana, k=3)
iris_result1[iris_result1==1] <- "setosa"
iris_result1[iris_result1==2] <- "versicolor"
iris_result1[iris_result1==3] <- "virginica"
iris_result1
table(iris_result1,iris[,5])


#2.1
iris_kmeans1 <- kmeans(iris[,-5],centers = 2)
iris_kmeans2 <- kmeans(iris[,-5],centers = 3)
iris_kmeans3 <- kmeans(iris[,-5],centers = 4)
plot(iris_kmeans2$cluster)

critere <- rep(0,10)
critere1 <- rep(0,10)
for (i in c(1:100)){
  for (k in c(1:10)){
    iris_kmeans <- kmeans(iris[,-5],centers = k)
    critere[k] <- critere[k]+iris_kmeans$tot.withinss
    critere1[k] <- critere1[k]+(iris_kmeans$betweenss/iris_kmeans$totss)
  }
}
critere <- critere/100
critere1 <- critere1/100
plot(critere)
plot(critere1)
