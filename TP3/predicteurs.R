classifieur <- function(dataset) {
  # Chargement de l'environnement
  load("env.Rdata")
  clas_pred_test <- predict(clas_best_model, newdata = dataset)
  predictions <- clas_pred_test$class
  return(predictions)
}
