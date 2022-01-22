library(dslabs)
data(heights)

# Exibe
heights

# Número de linhas
nrow(heights)

# Linha 777
heights[777, ]

# Sexo da linha 777
heights$sex[777]
heights[777, 1]

# Maior altura
max(heights$height)

# Linha com menor altura
which.min(heights$height)

# Altura média e mediana
mean(heights$height)
median(heights$height)

# Conta frequencias
table(heights$sex)

# Indivíduos com mais de 78 polegadas
maiores <- heights[heights$height > 78, ]
nrow(maiores)
sum(maiores$sex == 'Female')
