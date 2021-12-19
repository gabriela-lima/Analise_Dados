#Blibliotecas utilizadas no projeto
library("DT") # Usado nas tabelas com os jogadores
library("plotly") # Usado nas tabelas com os jogadores

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

#Tabelas
datatable(base[1:3,1:5])
datatable(base[1:3, 6:10])
datatable(base[1:3,11:15])
datatable(base[1:3, 16:22])

#Histograma da Pontuação Geral
hist(base$overall, col = rainbow(9),xlab = "Pontuação Geral",
ylab = "Porcentagem", ylim = c(0,0.065) ,
  main = "Histograma de Pontuação Geral", probability = T)
densityPont<-density(base$overall)
lines(densityPont)

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
        
        # Tabela com a quantidade de jogadores de cada pais
pais <- base$nationality
pais <- data.frame(pais = pais)

y <-pais %>% 
  group_by(pais) %>%
  summarise(quantidade = length(pais))
datatable(y,caption ="Nacionalidade")
