mouse.color <- c("purple", "red", "yellow", "brown")
mouse.weight <- c(23,21,18,26)
mouse.info = data.frame(colour = mouse.color, weight = mouse.weight)

str(mouse.info)

mouse.info[4,1]

View(airquality)

aux <- subset(airquality, subset = airquality$Month == 5)
aux1 <- aux[complete.cases(aux),]
min(aux1$Ozone)

x <- subset(airquality, subset = airquality$Ozone >25 & airquality$Temp <90)
x1 <- x[complete.cases(x),]
mean(x1$Solar.R)

y <- airquality[complete.cases(airquality),]
str(airquality)

genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))

str(genomas)

b <- subset(genomas, subset = genomas$Chromosomes > 1 & genomas$Plasmids >0)

a <- factor(genomas$Groups)

cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))

str(cancer_stats)

c <- subset(cancer_stats, subset = cancer_stats$Class == "Digestive System" & cancer_stats$Female.Cases > cancer_stats$Male.Cases)

d <- subset(cancer_stats, subset = cancer_stats$Male.Deaths > 0)

max(d$Male.Deaths/d$Male.Cases)

e <- d$Male.Deaths/d$Male.Cases

View(e)

e <- d$Female.Deaths/d$Female.Cases

