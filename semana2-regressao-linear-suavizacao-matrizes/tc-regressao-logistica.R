library(caret)

set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5,
                      mu_0 = 0, mu_1 = 2,
                      sigma_0 = 1,  sigma_1 = 1){

  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)

  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

set.seed(1, sample.kind="Rounding")
mu_1 <- seq(0, 3, len=25)
dats <- map(mu_1, function(mu1) {
  make_data(mu_1 = mu1)
})

accuracies <- map(dats, function (dat) {
  glm_fit <- dat$train %>%
    glm(y ~ x, data=., family = "binomial")

  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor(levels=c(0, 1))
  confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]]
})

plot(mu_1, accuracies)
