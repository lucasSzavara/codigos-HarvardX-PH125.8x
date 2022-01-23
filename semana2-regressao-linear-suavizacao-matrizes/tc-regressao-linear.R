library(tidyverse)
library(caret)

# Criar um dataset
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


y <- dat$y
set.seed(1, sample.kind="Rounding")
rmses <- replicate(100, {
  # Cria dataset de treino e teste
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  # Treina o modelo
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  # Calcula o RMSE
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmses); sd(rmses)


# Refazendo para datasets maiores:

set.seed(1, sample.kind="Rounding")
mean_rmses <- function (n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))


  y <- dat$y
  rmses <- replicate(100, {
    # Cria dataset de treino e teste
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    # Treina o modelo
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    # Calcula o RMSE
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(avg = mean(rmses), sd = sd(rmses))
}

n <- c(100, 500, 1000, 5000, 10000)
sapply(n, mean_rmses)

# Vemos que aumentar a amostra não implica em um menor erro, mas numa menor variabilidade do erro. O que acontece se
# replicarmos isso com uma correlação maior entre X e Y?
set.seed(1, sample.kind="Rounding")
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


y <- dat$y
set.seed(1, sample.kind="Rounding")
rmses <- replicate(100, {
  # Cria dataset de treino e teste
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  # Treina o modelo
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  # Calcula o RMSE
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(rmses); sd(rmses)


# Teste com modelo usando duas variáveis como preditoras
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
y <- dat$y

set.seed(1, sample.kind="Rounding")
# Cria dataset de treino e teste
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
# Treina o modelo usando apenas x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
# Calcula o RMSE
sqrt(mean((y_hat - test_set$y)^2))

# Treina o modelo usando apenas o x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
# Calcula o RMSE
sqrt(mean((y_hat - test_set$y)^2))

# Treina o modelo usando x_1 e x_2
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
# Calcula o RMSE
sqrt(mean((y_hat - test_set$y)^2))
# Vemos que usar as duas variáveis diminui consideravelmente o erro

# Teste com dois preditores com alta correlação
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

# Mas que essa melhoria só existe quando a correlação entre as variáveis preditoras é baixa