#Projeto publicado: https://rpubs.com/Gabriela_lima/6788683
#Base de dados: https://www.kaggle.com/kerneler/starter-fifa-20-ultimate-team-cb8ba4b6-b

#FIFA 20

---
title: "Projeto_2va"
author: "Gabriela Lima"
date: "17/10/2020"
output: html_document
---
# FIFA 20: Ultimate Team {.tabset}

## Introdução 
FIFA Ultimate Team é um jogo eletrônico onde o objetivo é montar a melhor equipe possível no seu time. Os jogadores são cartas que possuem tanto estatísticas reais quanto simuladas que podem ser usadas não só no jogo, mas também na vida real. Ou seja, com isso podemos perceber algumas disparidades existentes dentro do jogo assim como fora.

Neste projeto serão analisadas as informações contidas no dataset ["FIFA 20 - Ultimate Team players dataset"](https://www.kaggle.com/stefanoleone992/fifa-20-ultimate-team-players-dataset?select=fut_bin20_players.csv) disponível no site [Kaggle](https://www.kaggle.com/) que possui 18.775 jogadores com 95 atributos diferentes. 

Inicialmente planeja-se obter informações como: a porcentagem das posições dos jogadores brasileiros, a porcentagem das pontuações gerais dos jogadores, a quantidade de jogadores por faixa etária, entre outros dados que podem ser levantados dessa base.

## Bibliotecas
As bibliotecas que foram utilizadas durante esse projeto.
```{r message=FALSE, warning=FALSE}
library("DT") # Usado nas tabelas com os jogadores
library("plotly") # Usado nas tabelas com os jogadores
```


## Limpeza de Dados
A última atualização dessa [base de dados](https://www.kaggle.com/stefanoleone992/fifa-20-ultimate-team-players-dataset?select=fut_bin20_players.csv) foi na data 07/12/2019 com 18.775 jogadores com 95 atributos diferentes cujo dado foi extraído do site público [futbin](https://futbin.com).

Há uma quantidade grande de jogadores duplicados (cerca de 5800 jogadores) que não serão utilizados na análise de dados, por isso ele foram excluidos da base.

Também há uma grande quantidade de colunas com atributos in game que não serão necessárias durante a fase de análise de dados e por isso foram excluídas da base de dados. Além disso, algumas colunas foram transformadas em factor.

 
```{r}
#importando a base de dados
base <- read.csv("D:\\Projetos\\R\\Projeto 2va\\base de dados\\fut_bin20_players.csv", encoding = "UTF-8") 

# Tornando em um fator
base$quality <- as.factor(base$quality)
base$pref_foot <- as.factor(base$pref_foot)
base$att_workrate <- as.factor(base$att_workrate)
base$def_workrate <- as.factor(base$def_workrate)
base$position <- as.factor(base$position)

# Excluindo as colunas
base <- base[,- c(81:83, 1,6,13,17,62)]
base <- base[, -c(13:54, 56:75)]
base <- base[, -c(17, 21, 25)]

# Retirando os players duplicados
duplicados <-which(duplicated(base$player_name))
base <- base[-duplicados,]

```

Após a limpeza a base de dados possui 12.968 jogadores com 22 atributos diferentes.

Na coluna 13 se encontra o pé preferido do jogador, 1 e 2 se encontra o nome conhecido e o nome completo, respectivamente e as colunas de 8 a 12 que possuem a nacionalidade, a posição, a idade, altura e peso.

```{r}
datatable(base[1:3,1:5])
datatable(base[1:3, 6:10])
datatable(base[1:3,11:15])
datatable(base[1:3, 16:22])
```




## Análise de Dados

O histograma abaixo mostra a porcentagem da quantidade de jogadores em um determinado intervalo de pontuação geral, e com isso podemos notar que a maior parte dos jogadores estão com a pontuação geral entre 65 e 70, com uma quantidade escassa dos que passam de 80.
```{r}
#Histograma da Pontuação Geral
hist(base$overall, col = rainbow(9),xlab = "Pontuação Geral",
ylab = "Porcentagem", ylim = c(0,0.065) ,
  main = "Histograma de Pontuação Geral", probability = T)
densityPont<-density(base$overall)
lines(densityPont)

```

O gráfico de Pizza baixo mostra a porcentagem do intervalo de idade dos jogadores que estão disponíveis nas carta do jogo. Com isso, podemos perceber a pequena porcentagem de jogadores disponíveis com menos de 21 anos.
```{r}
#Grafico de Pizza Idade
minorAge <- (base[base$age > 17 & base$age <= 21,])
mediumAge <- (base[base$age > 21 & base$age <= 26,])
normalAge <-  (base[base$age > 26 & base$age <= 31,])
olderAge <-  (base[base$age > 31,])
# Fazendo a porcentagem
x <- c(sum(minorAge$age), sum(mediumAge$age), sum(normalAge$age), sum(olderAge$age))
percent <- round(x/sum(x)*100)
label <- paste(percent,"%",sep="")
# O grafico
pie(x,labels=label, main="Porcentagem das Idades", col=c("blue", "red", "green", "yellow"), radius = 1)
# Legenda do grafico
legend("topleft",
        legend=c("Jogadores com 17 a 21", "Jogadores com 22 a 26",
                 "Jogadores com 27 a 31", "Jogadores com mais de 31"),
        cex=0.59, fill=c("blue", "red", "green", "yellow"))

```

O gráfico abaixo mostra a porcentagem da perna preferida entre jogadores com uma pontuação geral maior que 80 em intervalos de idade. Dessa forma, conseguimos notar a diferença entre jogadores com maior habilidade que são destros e canhotos em diferentes faixas etárias.
```{r}
#Grafico de barras
minorAge <- minorAge[minorAge$overall > 80,]
mediumAge <- mediumAge[mediumAge$overall > 80,]
normalAge <- normalAge[normalAge$overall > 80,]
olderAge <- olderAge[olderAge$overall > 80,]

# Usam o pe esquerdo
pel <- c(nrow(minorAge
                [minorAge$pref_foot == "Left",])/nrow(minorAge), nrow(mediumAge[mediumAge$pref_foot == "Left",])/nrow(mediumAge), nrow(normalAge[normalAge$pref_foot == "Left",])/nrow(normalAge), nrow(olderAge[olderAge$pref_foot == "Left",])/nrow(olderAge))
# Usam o pe direito
per <- c(1 - pel)

matriz <- matrix(c(pel, per), nrow = 2, ncol = length(pel), byrow = T)

barplot(matriz,col = c("blue", "red"), main = "Perna preferida por jogadores com pontuação geral acima de 80",
        names.arg = c("17-21", "22-26", "27-31", "31+"),xlab = "Idade",ylab = "Porcentagem",ylim = c(0,1), cex.names = .75, las = 2,beside = T)
legend("topright", pch = c(15,15), col = c("blue", "red"), legend = c("Canhoto","Destro"), cex = 0.6)

```

A base de dados possui os preços das cartas em 3 dispositivos diferentes, o computador, o Playstation 4 e o Xbox. Com o preço máximo e mínimo das cartas em cada dispositivo na edição de 2020.

Dessa forma o gráfico abaixo mostra os 10 jogadores mais valiosos e suas respectivas médias de valor nos diferentes dispositivos assim como uma última média que mostra a média das médias nos dispositivos.
```{r}
#Os 10 jogdores mais valiosos do jogo
valiosos <- base[order(base$pc_max, decreasing = T),]
valiosos <- valiosos[c(1:10),]
# Media dos dispositivos disponiveis na base
meanPs4 <- rowSums(valiosos[,c(15,16)])/2
meanXbox <- rowSums(valiosos[,c(18,19)])/2
meanPc <- rowSums(valiosos[,c(21,22)])/2

matrizV <- matrix(c(meanPs4, meanXbox, meanPc), nrow = 3, ncol = 10, byrow = T)
# Media geral
meanGeral <- colSums(matrizV)/3

matrizV <- rbind(matrizV, meanGeral)
# Tirar o "e" do grafico
options(scipen = 999)
# Ajustando o grafico
par(mar=c(6, 5, 2, 5))
barplot(matrizV, names.arg = valiosos$player_name, las = 2, col = c("blue", "green", "red", "black"), main = "Média do preço dos 10 jogadores mais valiosos", beside = T, ylim = c(0,10000000))

legend("topright", pch = c(15,15,15,15), col =  c("blue", "green", "red", "black"), legend = c("PS4","Xbox", "PC", "Geral"), cex = 0.6)

```

O gráfico abaixo mostra o percentual de brasileiros disponíveis no jogo que jogam nas posições possíveis. Podemos observar que as posições favoritas dos jogadores brasileiros são: CB (Zagueiro Central) e ST (Atacante).  
```{r}
# Quantidade de brasileiros em posicoes
brasileiros <- base[base$nationality == "Brazil",]
somaB <-  c(sum(brasileiros$position == "CAM"), sum(brasileiros$position == "ST"), sum(brasileiros$position == "LW"), sum(brasileiros$position == "LB"), sum(brasileiros$position == "GK"), sum(brasileiros$position == "CDM"), sum(brasileiros$position == "CF"), sum(brasileiros$position == "CM"), sum(brasileiros$position == "CB"), sum(brasileiros$position == "RW"), sum(brasileiros$position == "RB"), sum(brasileiros$position == "RWB"), sum(brasileiros$position == "RM"), sum(brasileiros$position == "LM"), sum(brasileiros$position == "LF"), sum(brasileiros$position == "LWB"), sum(brasileiros$position == "RF"))

# O percentual
percentB <- round(somaB/sum(somaB)*100)
labelB <- paste(percentB,"%",sep="")

pie(somaB,labels=labelB, main="Porcentagem das Posições dos Jogadores Brasileiros", col= c("white", "burlywood4", "cadetblue1", "chartreuse", "chocolate", "thistle", "bisque2", "blue", "blueviolet", "darkgray", "gray1", "deeppink2", "red", "violet", "lightblue3", "yellow", "orange"), radius = 1)

legend("topleft",
        legend=c("CAM", "ST",
                 "LW", "LB", "GK", "CDM", "CF", "CM", "CB", "RW", "RB", "RWB", "RM", "LM", "LF", "LWB", "RF"),
        cex=0.59, fill= c("white", "burlywood4", "cadetblue1", "chartreuse", "chocolate", "thistle", "bisque2", "blue", "blueviolet", "darkgray", "gray1", "deeppink2", "red", "violet", "lightblue3", "yellow", "orange"))


```

A tabela abaixo mostra a quantidade de cartas de jogadores disponíveis de cada nacionalidade. Com isso, podemos perceber a disparidade existente entre a quantidade de jogadores presentes no jogo vindos da Europa e da América do Sul quando comparada com os outros continentes.

```{r message=FALSE, warning=FALSE}
# Tabela com a quantidade de jogadores de cada pais
pais <- base$nationality
pais <- data.frame(pais = pais)

y <-pais %>% 
  group_by(pais) %>%
  summarise(quantidade = length(pais))
datatable(y,caption ="Nacionalidade")

```



## Conclusão


De acordo com os dados obtidos durante a fase de análise de dados, podemos concluir que existe uma grande desigualdade entre o número de jogadores de países da Europa e América do Sul que estão presentes nas cartas quando se comparado aos de outros continentes. 

Também podemos inferir que o preço das cartas dos jogadores varia de acordo com o dispositivo e não apenas com a pontuação geral ou nível de raridade.

Podemos notar que existem 2 posições que ficam vazias levando em consideração apenas jogadores de nacionalidade brasileira existentes no jogo. São as posições: RWB e LF.

