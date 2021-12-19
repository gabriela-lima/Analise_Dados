---
title: "Atividade 12"
author: "Gabriela Lima"
date: "03/10/2020"
output: html_document
---

# Atividade 12 {.tabset}

## Questão 1

```{r}
MRT_1F <-c(517.1468515630205, 85.13094142168089, 30.333207896694553, 12.694776264558937, 3.3041601673945418, 1.1823111717498882, 1.1892293502386786)

MRT_3F <-c(156.68929936163462, 11.540837783562276, 0.4512835621696538, 0.4509797929766453, 0.4502068233039181, 0.4496185276300172, 0.4543157082191288)

MRT_5F <-c(83.90319666471157, 0.3068151086494968, 0.30522314133037304, 0.3072588968084928, 0.30655265997285697, 0.3055812715727718, 0.3053297166713006)

MRT_10F <-c(29.55430642951759, 0.19832832665772515, 0.1971923924717474, 0.19796648905716516, 0.19615594370806338, 0.2034569237883263, 0.19617420889447737)

MRT_15F <-c(11.317736530583566, 0.167364215666193, 0.16172168266811013, 0.16701085329580515, 0.1598052657153692, 0.1645934043532696, 0.16216563797118075)

MRT_sem_F <-c(11.93430909937736, 0.6095414637034009, 0.6060645101029295, 0.612167181646899, 0.6146761002685637, 0.6096747087200697, 0.6125810476877268)

clock <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 3)

colors <- c("#E6E6E6", "#666666")

plot(clock,MRT_1F,type = "o",pch = 4 , xlab = "Time Between Things requests(seconds)", ylab = "Response Time(sec.)")

lines(MRT_3F,type = "o",col = "yellow" , pch= 11)

lines(MRT_5F,type = "o",col = "red" , pch= 1)

lines(MRT_10F,type = "o",col = "blue" , pch= 2)

lines(MRT_15F,type = "o",col = "purple" , pch= 5)

lines(MRT_sem_F,type = "o",col = "green" , pch= 4)

legend("topright", pch = c(4,11,1,2,5,4), col = c("black","yellow","red","blue","purple","green"),legend = c("1 fog","3 fog","5 fog","10 fog","15 fog","w/o fog"))

matriz<- c(MRT_1F,MRT_3F,MRT_5F,MRT_10F,MRT_15F,MRT_sem_F)
values <- matrix(matriz, nrow = 6, ncol = 7, byrow = T)

par(mfrow=c(3,2))

matrix<- c(MRT_1F,MRT_sem_F)
matrix <- matrix(matrix, nrow = 2,ncol = 7, byrow = T)
barplot(matrix,names.arg = clock,xlab = "Time Between Things requests(seconds)", ylab = "Response Time(s)", col=colors,beside = T)
legend("topright",pch = c(15,15), col = colors, legend = c("1 fog","w/o fog"))

matrix<- c(MRT_3F,MRT_sem_F)
matrix <- matrix(matrix, nrow = 2,ncol = 7, byrow = T)
barplot(matrix,names.arg = clock,xlab = "Time Between Things requests(seconds)", ylab = "Response Time(s)", col=colors,beside = T)
legend("topright",pch = c(15,15), col = colors, legend = c("3 fog","w/o fog"))

matrix<- c(MRT_5F,MRT_sem_F)
matrix <- matrix(matrix, nrow = 2,ncol = 7, byrow = T)
barplot(matrix,names.arg = clock,xlab = "Time Between Things requests(seconds)", ylab = "Response Time(s)", col=colors,beside = T)
legend("topright",pch = c(15,15), col = colors, legend = c("5 fog","w/o fog"))

matrix<- c(MRT_10F,MRT_sem_F)
matrix <- matrix(matrix, nrow = 2,ncol = 7, byrow = T)
barplot(matrix,names.arg = clock,xlab = "Time Between Things requests(seconds)", ylab = "Response Time(s)", col=colors,beside = T)
legend("topright",pch = c(15,15), col = colors, legend = c("10 fog","w/o fog"))

matrix<- c(MRT_15F,MRT_sem_F)
matrix <- matrix(matrix, nrow = 2,ncol = 7, byrow = T)
barplot(matrix,names.arg = clock,xlab = "Time Between Things requests(seconds)", ylab = "Response Time(s)", col=colors,beside = T)
legend("topright",pch = c(15,15), col = colors, legend = c("15 fog","w/o fog"))
```


## Questão 2

```{r}
cor <- c("#ff0000","#00ff00","#0000ff")
n <- c("$10-19","$20-29","$30-39","$40-49")
m<- matrix(c(53.8,33.9,2.6,0,43.6,54.2,60.5,21.4,2.6,11.9,63.8,78.6),nrow = 3,ncol = 4, byrow = T)

barplot(m, n.arg = n, col = cor)
legend("topright",pch= c(15,15,15), col = cor, legend = c("Good","Very Good","Excelent"))
```

## Questão 3

```{r}
temp <- (airquality[airquality$Month==5,]$Temp -32)/1.8
hist(temp, main = "Temperatura do mês", xlab = "Temperatura",ylab = "Frequência",col = rainbow(5))
den <- density(temp)
lines(den)
```

## Questão 4

```{r}
vendas <- read.table("https://training-course-material.com/images/8/8f/Sales.txt",header=TRUE)
pct<- round(vendas$SALES/sum(vendas$SALES)*100)
lbls<- paste(pct,"%",sep = "")
pie(vendas$SALES,labels = lbls,main = "Vendas paises",col = rainbow(6))
legend("topleft",legend = vendas$COUNTRY,fill = rainbow(6))
```

## Questão 5

```{r}
boxplot(InsectSprays$count ~ InsectSprays$spray,main = "Spray de Insetos", xlab = "Spray",ylab = "Quantidade", col = "yellow",outline = F)
```


## Questão 6

```{r}
plot(mtcars$wt,mtcars$mpg, main= "Carros",xlab = "Peso(wt)", ylab = "Milhas percorridas(mpg)", pch = 17, col = mtcars$carb)
```


## Questão 7
