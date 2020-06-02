setwd(dir="C:/Users/Aurelio Diaz/Documents/5_DOUTORADO/Bosques Ibéricos/Digestiones")

datdig <- read.table("190520_Resultados_analisis.csv", sep = ";", header = T, dec = ",")
str(datdig)
head(datdig)

#para poder juntar varias columnas las he de transformar pues los vectores factores no se juntan 
#con otros por lo que hay que transformar a caracter los dos para despues juntarlos

datdig$Provincia<-as.character(datdig$Provincia)
datdig$Parcela<-as.character(datdig$Parcela)
str(datdig)

#para crear una columna con las parcelas y provincias, le he metido el orden de las muestras 
#para que no me de problemas despues en el nombre de las filas

library(tidyverse)
datdig1<-unite(datdig, Local, c(1:3), sep = " ", remove = TRUE)
head(datdig1)
str(datdig1)
summary(datdig1)

#cambiamos el nombre de las filas por la columa local previamente montada, 
#no puede haber valores repetidos lo cual me ha complicado la vida.

rownames(datdig1)=datdig1$Local

datdig1<-datdig1[,-(1:2)]
head(datdig1)

#realizamos una transformacion en los parámetros de mayores valores absolutos
# asi al máximo del K, Ca y Mn se le retira el valor de cada muestra
datdig1$K<-max(datdig1$K) -datdig1$K
datdig1$Mg<-max(datdig1$Mg) -datdig1$Mg
datdig1$Ca<-max(datdig1$Ca) -datdig1$Ca
datdig1$Mn<-max(datdig1$Mn) -datdig1$Mn
head(datdig1)


#para ver las diferentes variables
library(TeachingDemos) 

faces2(datdig1, nrows=7)


panel.hist <- function(x, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(usr[1:2], 0, 1.5) ) 
  h <- hist(x, plot = FALSE) 
  breaks <- h$breaks; nB <- length(breaks) 
  y <- h$counts; y <- y/max(y) 
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...) 
} 
pairs(datdig1,diag.panel=panel.hist)

library(PerformanceAnalytics)
chart.Correlation(datdig1)
rs.cor <- (cor(x=datdig1, method="pearson",use="complete.obs"))
rs.cor

cor(datdig1)
dig.pc<-princomp(datdig1,cor=TRUE) 
summary(dig.pc,loadings=TRUE)

# Es lo mismo que calcular los autovalores y autovectores de S
S = cor(datdig.valores) 
eigen(S)

dig.pc$scores[,1:3]

par(pty="s") 
plot(dig.pc$scores[,1],dig.pc$scores[,2], 
     ylim=range(dig.pc$scores[,1]), 
     xlab="PC1",ylab="PC2",type="n",lwd=2) 
text(dig.pc$scores[,1],dig.pc$scores[,2], 
     labels=abbreviate(row.names(datdig1)),cex=0.5,lwd=2)


plot(dig.pc$scores[,1],dig.pc$scores[,3], 
     ylim=range(dig.pc$scores[,1]), 
     xlab="PC1",ylab="PC3",type="n",lwd=4) 
text(dig.pc$scores[,1],dig.pc$scores[,3], 
     labels=abbreviate(row.names(datdig1)),cex=0.5,lwd=4)


plot(dig.pc$scores[,2],dig.pc$scores[,3], 
     ylim=range(dig.pc$scores[,2]), 
     xlab="PC2",ylab="PC3",type="n",lwd=2) 
text(dig.pc$scores[,2],dig.pc$scores[,3], 
     labels=abbreviate(row.names(datdig1)),cex=0.5,lwd=2)


#para relacionarlo con una de las variables
par(mfrow=c(2,3)) 
plot(dig.pc$scores[,1], datdig$P, xlab="PC1") 
plot(dig.pc$scores[,2], datdig$P, xlab="PC2") 
plot(dig.pc$scores[,3], datdig$P, xlab="PC3")

plot(dig.pc$scores[,1], datdig$Ca, xlab="PC1") 
plot(dig.pc$scores[,2], datdig$Ca, xlab="PC2") 
plot(dig.pc$scores[,3], datdig$Ca, xlab="PC3")

summary(lm(datdig$P~dig.pc$scores[,1]+dig.pc$scores[,2]+dig.pc$scores[,3]))
summary(lm(datdig$Ca~dig.pc$scores[,1]+dig.pc$scores[,2]+dig.pc$scores[,3]))

plot(dig.pc$scores[,2],datdig$P,xlab="PC1",ylab="P") 



pca <- prcomp(datdig1, scale = TRUE)
names(pca)
pca$center
pca$rotation
head(pca$x)
biplot(x = pca, scale = 0, cex = 0.4, col = c("blue4", "brown3"))


