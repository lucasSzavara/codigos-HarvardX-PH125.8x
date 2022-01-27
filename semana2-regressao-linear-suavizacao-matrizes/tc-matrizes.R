library(dslabs)
mnist <- read_mnist()
images <- mnist$train$images

# Proporção de cinza
sum(images > 50 & images < 205) / (60000*784)


y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")