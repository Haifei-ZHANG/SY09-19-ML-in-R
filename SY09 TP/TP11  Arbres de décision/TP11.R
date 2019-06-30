library(rpart)
library(rpart.plot)
front.tree <- function(model, data, class = NULL, predict_type = "class",
                       resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

separ1 <- function(data, X, z)
{
  ntot <- dim(X)[1]
  
  iapp <- sample(1:ntot, round(2/3*ntot))
  itst <- setdiff(1:ntot, iapp)
  
  app <- data[iapp,]
  Xapp <- X[iapp,]
  zapp <- z[iapp]
  Xtst <- X[itst,]
  ztst <- z[itst]
  
  out <- NULL
  out$app <- app
  out$Xapp <- Xapp
  out$zapp <- zapp
  out$Xtst <- Xtst
  out$ztst <- ztst
  
  out
}

#sur data Synth1-1000
synth = read.csv("./donnees/Synth1-1000.csv")
synth[,3] = as.factor(synth[,3])
synth = separ1(synth,synth[,c(1,2)],synth[,3])

control=rpart.control(minsplit=1, cp=0)
synth_tree = rpart(z~.,data=synth$app,control = control)
rpart.plot(synth_tree)
erreur_complet = 1-sum(diag(table(synth$ztst,rpart.predict(synth_tree,newdata = synth$Xtst,type = 'class'))))/length(synth$ztst)

erreur_prune = rep(0,nrow(synth_tree$cptable))


for (i in 1:nrow(synth_tree$cptable)){
    synth_prune_tree = prune(synth_tree,cp=synth_tree$cptable[i,1])
    erreur_prune[i] = 1-sum(diag(table(synth$ztst,rpart.predict(synth_prune_tree,newdata = synth$Xtst,type = 'class'))))/length(synth$ztst)
}

synth_best_tree = prune(synth_tree,cp = synth_tree$cptable[which.min(erreur_prune),1])
rpart.plot(synth_best_tree)

#sur pima
pima = read.csv("donnees/Pima.csv", header=T)
pima[,8] = as.factor(pima[,8])
pima = separ1(pima,pima[,-8],pima[,8])

control=rpart.control(minsplit=1, cp=0)
pima_tree = rpart(z~.,data=pima$app,control = control)
rpart.plot(pima_tree)
erreur_complet_pima = 1-sum(diag(table(pima$ztst,rpart.predict(pima_tree,newdata = pima$Xtst,type = 'class'))))/length(pima$ztst)

erreur_prune_pima = rep(0,nrow(pima_tree$cptable))


for (i in 1:nrow(pima_tree$cptable)){
  pima_prune_tree = prune(pima_tree,cp=pima_tree$cptable[i,1])
  erreur_prune_pima[i] = 1-sum(diag(table(pima$ztst,rpart.predict(pima_prune_tree,newdata = pima$Xtst,type = 'class'))))/length(pima$ztst)
}

pima_best_tree = prune(pima_tree,cp = pima_tree$cptable[which.min(erreur_prune_pima),1])
rpart.plot(pima_best_tree)
