setwd("D:/A-UTC/1PEDAGOGIQUE/A2018/SY19/TP/TP2")
spam <- read.table('spambase.dat')
n <- nrow(spam)
p <- ncol(spam)
#names <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference", "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$", "char_freq_#", "capital_run_length_average", "capital_run_length_longest", "capital_run_length_total", "is_spam")
names(spam) <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference", "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$", "char_freq_#", "capital_run_length_average", "capital_run_length_longest", "capital_run_length_total", "is_spam")
#train <- sample(1:n, size = floor(2*n/3))
train <- sample(1:n,floor(2*n/3))
spam.train <- spam[train, ]
spam.train.x <- spam.train[, -p]
spam.train.z <- as.vector(spam.train[, p])
spam.test <- spam[-train, ]
spam.test.x <- spam.test[, -p]
spam.test.z <- as.vector(spam.test[, p])

#Apprentissage du modèle de l'analyse discriminante linéaire et prédictions sur l'ensemble de test
library(MASS)
lda.model <- lda(is_spam ~ ., data = spam.train)
pred <- predict(lda.model, newdata = spam.test)

perf <- table(spam.test.z,pred$class)
taux.erreur <- 1-sum(diag(perf))/(n-floor(2*n/3))

library(pROC)
plot(roc(spam.test.z, as.vector(pred$x)))

#Apprentissage par régression logistique
logreg.model <- glm(is_spam ~ ., data = spam.train, family = binomial)
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
logreg.pred <- predict(logreg.model, newdata = spam.test, type = "link")
#pred <- predict(logreg.model, newdata = spam.test.x, type = "response")
plot(roc(spam.test.z, as.vector(logreg.pred)), add = TRUE, col = "red")

logreg.perf <- table(spam.test.z,logreg.pred>0.5)
logreg.taux.erreur <- 1-sum(diag(logreg.perf))/(n-floor(2*n/3))
glm.roc.curve <- roc(set.test$V58, as.vector(glm.pred.letter))
plot(glm.roc.curve,col="red", add=TRUE)

#IC
perr1 <- taux.erreur
perr2 <- logreg.taux.erreur
perr1+2*sqrt(perr1*(1-perr1)/(n-floor(2*n/3)))
perr1-2*sqrt(perr1*(1-perr1)/(n-floor(2*n/3)))
perr2+2*sqrt(perr2*(1-perr2)/(n-floor(2*n/3)))
perr2-2*sqrt(perr2*(1-perr2)/(n-floor(2*n/3)))

#partie2 Comparaison ADL-ADQ sur les données simulées
#Définition des paramètres
mu1 <- c(0, 0, 0)
mu2 <- c(1, 1, 1)
Sigma1 <- diag(3)
Sigma2 <- 0.8 * diag(3)

#ensemble de test
nt <- 10000
n1 <- rbinom(1, nt, 0.5)
n2 <- nt - n1
x1 <- mvrnorm(n = n1, mu = mu1, Sigma = Sigma1)
x2 <- mvrnorm(n = n2, mu = mu2, Sigma = Sigma2)
xtst <- rbind(x1, x2)
ytst <- c(rep(1, n1), rep(2, n2))
test <- data.frame(cbind(xtst, ytst))
names(test)[4] <- "y"
N <- c(seq(30, 1000, 50), seq(2000, 10000, 2000), 20000)  # taille de l'ens. d'appr.
M <- 20  # nombre de répétitions
err.lda <- matrix(0, length(N), M)
err.qda <- matrix(0, length(N), M)
for (i in 1:length(N)) {
  n <- N[i]
  for (j in 1:M) {
    n1 <- rbinom(1, n, 0.5)
    n2 <- n - n1
    x1 <- mvrnorm(n = n1, mu = mu1, Sigma = Sigma1)
    x2 <- mvrnorm(n = n2, mu = mu2, Sigma = Sigma2)
    x <- rbind(x1, x2)
    y <- c(rep(1, n1), rep(2, n2))
    train <- data.frame(cbind(x, y))
    
    res.lda <- lda(y ~ ., data = train)
    pred.lda <- predict(res.lda, newdata = test)
    err.lda[i, j] <- mean(pred.lda$class != ytst)
    
    res.qda <- qda(y ~ ., data = train)
    pred.qda <- predict(res.qda, newdata = test)
    err.qda[i, j] <- mean(pred.qda$class != ytst)
    # print(c(i, j, err.lda[i, j], err.qda[i, j]))
  }
  
}

Err.lda <- rowMeans(err.lda)
Err.qda <- rowMeans(err.qda)

plot(N, Err.lda, type = "l", ylim = range(Err.lda, Err.qda), log = "x")
lines(N, Err.qda, lty = 2)
#L'analyse discriminante linéaire est plus performante que 
#l'analyse discriminante quadratique lorsque l'ensemble d'apprentissage 
#est petit même si les hypothèses sont a priori plus favorables ? l'ADQ 
#(matrices de variance-covariance conditionnellement ? la classe différentes). 
#Cela est du au fait que l'ADQ comprend environ deux fois plus de paramètres ? estimer que l'ADL. 
#Leur estimation n'est pas très précise du fait de la taille réduite de l'ensemble d'apprentissage.

#En revanche, lorsque l'ensemble d'apprentissage est plus conséquent,
#l'estimation des paramètres de l'ADQ devient plus précise et l'ADL, 
#dont les conditions d'application ne sont pas rigoureusement vérifiées devient 
#moins performante.


