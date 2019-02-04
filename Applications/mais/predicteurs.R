classifieur_astronomie <- function(dataset) {
  # Chargement de l'environnement
  load("env.Rdata")
  predictions <- predict(mais.bestmodel, newdata = dataset)
  return(predictions)
}
