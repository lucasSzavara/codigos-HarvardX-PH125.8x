library(caret)
library(dslabs)
data('heights')

set.seed(1, sample.kind = "Rounding")

y <- heights$sex
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- heights[test_index,]
train <- heights[-test_index,]

ks <- seq(1, 101, 3)
library(purrr)
accuracy <- sapply(ks, function(k){
  fit <- knn3(sex ~ ., data = train, k = k)
  y_hat <- predict(fit, test, type = "class")
  F_meas(data = y_hat, reference = test$sex)
})
ks[which.max(accuracy)]
max(accuracy)

# Ex2
library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")

y <- tissue_gene_expression$y
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
tge_df <- as.data.frame(tissue_gene_expression)
test <- tge_df[test_index,]
train <- tge_df[-test_index,]

ks <- seq(1, 11, 2)
accuracy <- sapply(ks, function(k){
  fit <- knn3(y ~ ., data = train, k = k)
  y_hat <- predict(fit, test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test$y)
  cm_test$overall["Accuracy"]
})
ks[which.max(accuracy)]
max(accuracy)
