#Le modèle est: Y=??0+??1X1+??2X2+??

n <- 1000
sig <- 0.01

b0 <- 1
b1 <- 2
b2 <- -3
betas <- c(b0, b1, b2)

#Ensemble d'apprentissage
x1i <- runif(n)
x2i <- runif(n)

#Observations
yi <- b0 + b1*x1i + b2*x2i + rnorm(n, mean=0, sd=sig)

#Modèle linéaire
df <- data.frame(yi, x1i, x2i)
lm.model <- lm(yi~x1i+x2i, data=df)

#Intervalles de confiance et de prédiction en (x1,x2)(x1,x2)
x1 <- runif(1)
x2 <- runif(1)

predict(lm.model, int="p", newdata=data.frame(x1i=x1, x2i=x2))

predict(lm.model, int="c", newdata=data.frame(x1i=x1, x2i=x2))


#Simulation de l'intervalle de confiance

x1 <- runif(1)
x2 <- runif(1)

simulation <- function() {
# Ensemble d'apprentissage
  x1i <- runif(n)
  x2i <- runif(n)
  
# Observations
  yi <- b0 + b1*x1i + b2*x2i + rnorm(n, mean=0, sd=sig)
  
# Modèle linéaire
  df <- data.frame(yi, x1i, x2i)
  lm.model <- lm(yi~x1i+x2i, data=df)
  
# Nouvelle observation au point (x1, x2)
  y <- b0 + b1*x1 + b2*x2 + rnorm(1, mean=0, sd=sig)
  Ey <- b0 + b1*x1 + b2*x2
  
  ci <- predict(lm.model, int="c", newdata=data.frame(x1i=x1, x2i=x2))[1, 2:3]
  
# Dedans ou pas ?
  return((ci[1] < Ey) & (Ey < ci[2]))
}

sim <- replicate(1000, simulation())
mean(sim)

#Simulation de l'intervalle de prédiction

simulation <- function() {
  # Ensemble d'apprentissage
  x1i <- runif(n)
  x2i <- runif(n)
  
  # Observations
  yi <- b0 + b1*x1i + b2*x2i + rnorm(n, mean=0, sd=sig)
  
  # Modèle linéaire
  df <- data.frame(yi, x1i, x2i)
  lm.model <- lm(yi~x1i+x2i, data=df)
  
  # Nouvelle observation au point (x1, x2)
  y <- b0 + b1*x1 + b2*x2 + rnorm(1, mean=0, sd=sig)
  Ey <- b0 + b1*x1 + b2*x2
  
  ci <- predict(lm.model, int="p", newdata=data.frame(x1i=x1, x2i=x2))[1, 2:3]
  
  # Dedans ou pas ?
  return((ci[1] < y) & (y < ci[2]))
}

sim <- replicate(1000, simulation())
mean(sim)