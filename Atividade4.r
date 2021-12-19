##Exercicio 4 (fatores)
##Cria fator com F e M repetidos 100 vezes cada
a <- factor(rep(c("F","M"),each=100))

##Cria fator de Drinks
drinks <- factor(c("beer","beer","wine","water"))

##Proporcao de beer em drinks
mean(drinks == "beer")
##Muda todos os elementos beer para water
levels(drinks)[1] <- "water"

##Exercicio 4 (Lista)

##Criando uma lista
lista_pessoas <- list(nomes=c("Jo�o","Paula","Maria","Ingrid","Jos�","Marcos"), pesos = c(80,65,70,58,78,70),alturas = c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70))

##Adicionar imc a uma lista
lista_pessoas$imc <- c(27.681, 23.588, 25.711, 22.656, 25.180, 24.221)

##Mostrar o primeiro elemento da lista dentro do vetor elemento 1
lista_pessoas[[1]][1]

##Mostrar a estrutura da lista lista_pessoas
str(lista_pessoas)

##Mostrar
lista_pessoas[[4]][3]

##Criar lista
lista<-lapply(airquality, function(x){mean(x)})

##Mostrar temperaturas contidadas na lista
lista$Temp

##Exercicio 4 (Matriz)

##Load de database
load("chuvas.RData")

##Media das chuvas 30 dias
mean(colMeans(chuvas))


sum(chuvas[,12])
sum(chuvas[,14])

##calculam o volume de chuvas do munic�pio 81 (mun_81) nos primeiros 10 dias observados
aux<-chuvas["mun_81" , ]; aux<-sum(aux[1:10])
aux<-chuvas[81 , ]; aux<-sum(aux[1:10])
aux<-chuvas[81 , ]; aux<-sum(aux[seq(1,10,1)])
aux<-chuvas["mun_81" , ]; aux<-sum(aux[c(1:10)])
