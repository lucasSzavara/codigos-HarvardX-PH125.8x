library(dslabs)
data(tissue_gene_expression)

d <- dist(tissue_gene_expression$x)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))
