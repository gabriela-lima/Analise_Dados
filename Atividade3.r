v1 <- c(3,7,-5,-7)
v2 <- c(1:50)
v3 <- c(v1,v2)
xx <- c(TRUE, 2)
x <- c(1:4)
y <- c(2:3)
z <- c(x*y)

##Competicao entre joana e maria att 3
nomes <- c("Maria","Joana")
abd <- c(42, 38)
sal <- c(102, 178)
susp <- c(38, 71)
correr <- c(38, 71)
desp <- c(97, 70)

##Padronizacao
abd.n = (abd - mean(abd))/sd(abd)
sal.n = (sal - mean(sal))/sd(sal)
susp.n = (susp - mean(susp))/sd(susp)
correr.n = (correr - mean(correr))/sd(correr)
desp.n = (desp - mean(desp))/sd(desp)

##Media padronizada
media = (abd.n + sal.n + susp.n + correr.n + desp.n)/5

##Maximo da media
max(media)

##Mostrar o nome de quem tiver a media igual ao maximo
nomes[max(media)== media]

##Carregar arquivo
load("vetor.RData")

##Media aritimetica do vetor sem os NA
mean(vetor01, na.rm=TRUE)

##Mediana do vetor sem os NA
median(vetor01, na.rm = TRUE)

#Desvio padrao do vetor sem oa NA
sd(vetor01, na.rm = TRUE)

##Quantidade de faltosos (NA)
sum(is.na(vetor01))

#Quantidade de pessoas que tiraram mais que 7 e menos que 8
sum(vetor01 > 7.00 & vetor01 < 8.00, na.rm = TRUE)

##Quantidade de pessoas que tiraram mais que 9 ou menos que 1
sum(vetor01 > 9.00 | vetor01 < 1.00, na.rm = TRUE)

##Funcoes que retiram os NA do vetor
vetor01 <- vetor01[!is.na(vetor01)]

vetor01[!is.na(vetor01)] -> vetor01

vetor01 <- vetor01[-which(is.na(vetor01))]

sum(is.na(vetor01)) /15000

