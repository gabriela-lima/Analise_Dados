
iris
a <- apply(iris[ , 1:4], 1, FUN = mean)
b <- apply(iris, 2, FUN = mean)
c <- colMeans(iris)
d <- apply(iris[ , 1:4], 2, FUN = mean)
e <- sapply(iris[ , 1:4], 2, FUN = mean)
f <- mapply(iris[ , 1:4], mean)
g <- apply(iris[1:4, ], 2, FUN = mean)


a <- tapply(iris$Petal.Length, iris$Species, mean)
b<- tapply(iris[,3], iris$Species, mean)
c <- with(iris, tapply(Petal.Length, Species, mean))
d <- sapply(iris, 2, mean)
e <- mapply(iris$Petal.Length, iris$Species, mean)
f<- mean(iris$Petal.Length, iris$Species)


i <- 0
for (i in 1:4){ next}
a<- c("Rural","Amo")
mapply(rep,c("Rural","Amo"), 10:1)

for(i in 1:length(1:3)){
  for(j in 1:10){
    print(j+i-1)
  }
}

student.df = data.frame (nome= c ("Sue", "Eva", "Henry", "Jan"), sexo= c ("f", "f", "m", "m"), anos= c (21,15,17,19))

student.df$menor<-ifelse(student.df$sexo=="m"&student.df$anos<18,"V","F")

USArrests
lapply(X=USArrests[,1:4],FUN=sum)

x = 0
a = 0
b = -5
if(a>0){
  if(b<0){
    x = x + 5
  } else if(a > 5){
    x = x + 4
  }else{
    x = x + 3
  }
}else{
  x = x+2
}

print(x)

x = 0

while(x<100){
  x = x+2
}

print(x)
