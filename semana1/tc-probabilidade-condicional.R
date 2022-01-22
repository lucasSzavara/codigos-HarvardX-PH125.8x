set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Probabilidade de um teste positivo:
mean(test)

# Probabilidade de um indivíduo estar doente se o teste é negativo
mean(disease[test == 0])

# Probabilidade de estar doente se o teste é positivo
mean(disease[test == 1])

# Comparação da prevalência de doenças nos casos com testes positivos em relação ao caso geral
mean(disease[test == 1]) / mean(disease)
