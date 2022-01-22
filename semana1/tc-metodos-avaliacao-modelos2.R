library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Separar em treinamento e teste:
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Buscar qual parâmetro unico tem o melhor resultado para um modelo simples:
seplen_quebras <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
seplen_precisao <- map_dbl(seplen_quebras, function(x) {
  y_hat <- ifelse(train$Sepal.Length > x, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
seplen_max <- max(seplen_precisao)

sepwid_quebras <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
sepwid_precisao <- map_dbl(sepwid_quebras, function(x) {
  y_hat <- ifelse(train$Sepal.Width > x, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
sepwid_max <- max(sepwid_precisao)


petwid_quebras <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
petwid_precisao <- map_dbl(petwid_quebras, function(x) {
  y_hat <- ifelse(train$Petal.Width > x, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
petwid_max <- max(petwid_precisao)


petlen_quebras <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
petlen_precisao <- map_dbl(petlen_quebras, function(x) {
  y_hat <- ifelse(train$Petal.Length > x, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
petlen_max <- max(petlen_precisao)

best_cutoff <- petlen_quebras[which.max(petlen_precisao)]
best_cutoff

# Avaliar no dataset de teste
y_hat <- ifelse(test$Petal.Length > best_cutoff, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
mean(y_hat == test$Species)


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)


# Explorando o dataset:
plot(iris, pch=22, bg=iris$Species)


best_cutoff_wid <- petwid_quebras[which.max(petwid_precisao)]
best_cutoff_wid

y_hat <- ifelse(test$Petal.Length > best_cutoff & test$Petal.Width > best_cutoff_wid, 'virginica', 'versicolor') %>% factor(levels = levels(train$Species))
mean(y_hat == test$Species)
