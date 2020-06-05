###This script shows the statistical analysis between the nutrient variables obtained to quercus leave
###digestions (P, K, Na, Ca, Mg, Fe, Cu, Zn, & Mn)
###The analysis made were supported in PCA (Principal Component Analysis), and
###ANOVA (Analysis of Variance) between all the nutrients, however the ANOVA
### analysis was just done between the PCA scores.
###The PCA analysis needed logistical tranformation

getwd()
setwd(dir="C:/Users/Aurelio Diaz/Documents/Onedrive_Aurelio/OneDrive/Doctorate/quercus_informations/Inputs_database")

datdig <- read.table("190520_Resultados_analisis.csv", sep = ";", header = T, dec = ",")
str(datdig)
head(datdig)

#insert new dataframe with biomass informations
plot30<-read.csv("plot30.csv")

#para poder juntar varias columnas las he de transformar pues los vectores factores no se juntan 
#con otros por lo que hay que transformar a caracter los dos para despues juntarlos
datdig$Provincia<-as.character(datdig$Provincia)
datdig$Parcela<-as.character(datdig$Parcela)
str(datdig)

#para crear una columna con las parcelas y provincias, le he metido el orden de las muestras 
#para que no me de problemas despues en el nombre de las filas
datdig1<-unite(datdig, Local, c(1:3), sep = " ", remove = TRUE)
head(datdig1)

#cambiamos el nombre de las filas por la columa local previamente montada, 
#no puede haber valores repetidos lo cual me ha complicado la vida.
rownames(datdig1)=datdig1$Local
head(datdig1)
datdig1<-datdig1[,-(1:2)]
head(datdig1)
names(datdig1)

#analisis de anova
#para eso el vector Parcela debe estar como caracter sino no lo separar? en niveles o factores
#firstly we can study the nutrients graphics trough boxplot and after by histogram
ggplot(data = datdig, aes(x = Parcela, y = P, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = K, fill = Parcela, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Ca, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Mg, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Na, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Mn, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Cu, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Fe, color = Parcela)) +
  geom_boxplot() +
  theme_bw()
ggplot(data = datdig, aes(x = Parcela, y = Zn, color = Parcela)) +
  geom_boxplot() +
  theme_bw()

par(mfrow = c(3,3))
hist(datdig$P)
hist(datdig$Ca)
hist(datdig$Mg)
hist(datdig$K)
hist(datdig$Na)
hist(datdig$Mn)
hist(datdig$Fe)
hist(datdig$Cu)
hist(datdig$Zn)

#para poder aplicar un ANOVA se ha de verificar si cumple las siguientes condiciones 
#Normalidad de la muestra, como en la muestra la variable tiene menos de 50 casos aplicaremos Shapiro Wilt (shapiro.test)
#sino aplicariamos Kolmogorof(lillie.test)

lillie.test(datdig$P)#no es normal
lillie.test(datdig$K)#es una variable normal
lillie.test(datdig$Na)#no es normal
lillie.test(datdig$Ca)#no es normal
lillie.test(datdig$Mg)#no es normal
lillie.test(datdig$Mn)#no es normal
lillie.test(datdig$Fe)#no es normal
lillie.test(datdig$Cu)#no es normal
lillie.test(datdig$Zn)#es una variable normal 

#homocedasticidad, Dado que hay un grupo que se encuentra en el l?mite 
#para aceptar que se distribuye de forma normal, el test de Fisher y el de Bartlett 
#no son recomendables. En su lugar es mejor emplear un test basado en la mediana 
#test de Levene o test de Fligner-Killeen
#siendo valores de p-value superiores a 0.05. no hay evidencias significativas 
#de falta de homocedasticidad en ninguno de los dos test

fligner.test(K~Parcela, datdig)

leveneTest(K~Parcela, datdig, center="median")

#Ahora aplicamos el anova para las dos variables que cumplen las condiciones
#para eso aplicamos Tukey y vemos entre que grupos de parcelas hay diferencias 
#significativas en el contenido de Potasio y análogamente al Zinco

anova_one_k<-aov(K~Parcela, data = datdig)
summary(anova_one_k)#hay diferencias significativas
pairwise.t.test(datdig$K, datdig$Parcela, p.adj = "bonf")#aplicando bonferroni
TukeyHSD(anova_one_k)#para ver las diferencias entre las parcelas
#HSD.test(anova_one_k, "K") no genera resultados, posiblemente por la ingente
#cantidad de combinaciones creadas entre las parcelas (tratamientos)
#LSD.test(anova_one_k, "K", p.adj = "none")#aqui pasa lo mismo
plot(anova_one_k)

anova_one_Zn<-aov(Zn~Parcela, data = datdig)
summary(anova_one_Zn)#hay diferencias significativas
plot(anova_one_Zn)
TukeyHSD(anova_one_Zn)

boxplot(datdig$K~datdig$Parcela, col="red")
boxplot(datdig$Zn~datdig$Parcela, col="yellow")

#library(clusterSim) para normalizar y centrar las variables (scale & center)
#datdig1.norm<-data.Normalization (datdig1,type="n1",normalization="column")
#no ha funcionado por lo que aplicamos una transformacion en base logaritmica

datdig1.log<-log(datdig1)
head(datdig1.log)
chart.Correlation(datdig1.log)

#mejora substancialmente la normalizacion de las variables a excepcion 
#del P, Mn y Cu
lillie.test(datdig1.log$P)#no cumple para normalidad
lillie.test(datdig1.log$K)#cumple para normalidad
lillie.test(datdig1.log$Na)#cumple
lillie.test(datdig1.log$Ca)#cumple
lillie.test(datdig1.log$Mg)#cumple
lillie.test(datdig1.log$Mn)#no cumple
lillie.test(datdig1.log$Fe)#cumple
lillie.test(datdig1.log$Cu)#no cumple
lillie.test(datdig1.log$Zn)#cumple 

#PCA

#vamos usar el codigo prcomp con las nuevas variables donde 
#ha mejorado el % de los Pc
res.pca<-prcomp(datdig1.log, scale=T, center = T)
print(res.pca)
summary(res.pca)
plot(res.pca)

parce<-datdig$Parcela

ss<-get_eigenvalue(res.pca)
var.contri<-get_pca_var(res.pca) ## variable contributioin to the PCA 
var.contri$contrib
var.contri$coord
var.contri$cos2
var.contri$cor

ind.contrib <- get_pca_ind(res.pca) ## Individual contribution to the PCA
ind.contrib$contrib
ind.contrib$coord
ind.contrib$cos2

ind.datdig1<-cbind(ind.contrib$coord, parce)
ind.datdig1<-as.data.frame(ind.datdig1)
ind.datdig1$parce<-as.factor(ind.datdig1$parce)
ind.datdig1$Dim.1<-as.numeric((ind.datdig1$Dim.1))
ind.datdig1$Dim.2<-as.numeric((ind.datdig1$Dim.2))

print(ind.datdig1)

#Anova between plots (ind) and pca 
Anova.dim1<-aov(Dim.1~parce,data=ind.datdig1) #dim.1 like PCA1
Anova.dim1

str(ind.datdig1)
summary(Anova.dim1)
TukeyHSD(Anova.dim1)
anova.groups.1<-HSD.test(Anova.dim1, "parce", group=TRUE)
anova.groups.1
PCA1<-anova.groups.1$groups

#now, apply the same code for the PCA2(dim.2)
Anova.dim2<-aov(Dim.2~parce,data=ind.datdig1) #Dim.2 like PCA2
Anova.dim2

summary(Anova.dim2)
TukeyHSD(Anova.dim2)
anova.groups.2<-HSD.test(Anova.dim2, "parce", group=TRUE)
anova.groups.2
PCA2<-anova.groups.2$groups


IndiS <- Indi

g<- ggbiplot(res.pca, obs.scale = 1, var.scale = 1, 
              groups = datdig$Parcela, ellipse = TRUE, 
              circle = FALSE, repel =TRUE)
g<- g + labs(x=expression(PCA[1]~~group("(",63.3~"%",")")),
             y= expression(PCA[2]~~group("(",12.6~"%",")")))
g<-g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  ## delete background
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           axis.text=element_text(size=16),
           legend.text=element_text(size=16),
           legend.direction = 'horizontal',
           legend.position="top",
           axis.title.x=element_text(size=16),
           axis.title.y=element_text(size=16)
)

g <- g + scale_x_continuous(limits = c(-7,7))
g<- g + scale_color_brewer(palette="Paired")
g<-g + annotate("text", label="ND", y=-1.05, x= -6.2, colour= "red", size =2) 
g<-g+ annotate("text", label="PI", y=-0.87, x=-6.2, colour= "red", size =2)
g<-g + annotate("text", label="NW", y=-0.52, x=-6.2, colour= "red", size =2)
g<-g + annotate("text", label="QS", y=-0.02, x=-6.2, colour= "red", size =2)
g<-g + annotate("text", label="RS", y=0.19, x=-6.2, colour= "red", size =2)
g<-g + annotate("text", label="RI", y=0.34, x=-6.2, colour= "red", size =2)
g<-g+ annotate("text", label="LO", y=0.86, x=-6.2, colour= "red", size =2)
g<-g+ annotate("text", label="IT", y=1.12, x=-6.2, colour= "red", size =2)

g<-g + annotate("text", label="ND", y=-3, x=-2.2, colour= "red", size =2) 
g<-g+ annotate("text", label="PI", y=-3, x=0.74, colour= "red", size =2)
g<-g + annotate("text", label="NW", y=-3, x=-1.6, colour= "red", size =2)
g<-g + annotate("text", label="QS", y=-3, x=-2.6, colour= "red", size =2)
g<-g + annotate("text", label="RS", y=-3, x=3.1, colour= "red", size =2)
g<-g + annotate("text", label="RI", y=-3, x=1.7, colour= "red", size =2)
g<-g+ annotate("text", label="LO", y=-3, x=-0.25, colour= "red", size =2)
g<-g+ annotate("text", label="IT", y=-3, x=0.59, colour= "red", size =2)

g<- g + geom_segment(aes(x = -6.5, y = -1.2, xend = -6.5, yend = -0.3), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -6.6, y = -0.9, xend = -6.6, yend = 0), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -6.7, y = -0.6, xend = -6.7, yend = 0.45), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -6.8, y = -0.1, xend = -6.8, yend = 0.98), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -6.9, y = 0.23, xend = -6.9, yend = 1.2), colour= "black", size =0.8)

g<- g + geom_segment(aes(x = -2.7, y = -3.3, xend = -1.5, yend = -3.3), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -2.3, y = -3.4, xend = -0.25, yend = -3.4), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = -0.4, y = -3.5, xend = 0.91, yend = -3.5), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = 0.58, y = -3.6, xend = 1.93, yend = -3.6), colour= "black", size =0.8)
g<- g + geom_segment(aes(x = 1.7, y = -3.7, xend = 3.32, yend = -3.7), colour= "black", size =0.8)


           
           
           
#para ver las diferentes variables
faces2(datdig1.log, nrows=7)

#some graphics about PCA
fviz_eig(res.pca)

#para cada individuo, el grafico es poco informativo, sirve para pocas muestras(media?)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind
res.ind$coord          # Coordinates
head(res.ind$coord[, 1:3])
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

#any graphics studying the possible conrrelations between the diferent variables
panel.hist <- function(x, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(usr[1:2], 0, 1.5) ) 
  h <- hist(x, plot = FALSE) 
  breaks <- h$breaks; nB <- length(breaks) 
  y <- h$counts; y <- y/max(y) 
  rect(breaks[-nB], 0, breaks[-1], y, col="blue", ...) 
} 
pairs(datdig1.log,diag.panel=panel.hist)

chart.Correlation(datdig1)
rs.cor <- (cor(x=datdig1, method="pearson",use="complete.obs"))
rs.cor

cor(datdig1)
cor(datdig1.log)


#var.coord = loadings * the component standard deviations
#var.cos2 = var.coord^2
#var.contrib. The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)

#helper function
var_coord_func<-function(loadings, comp.sdev){
  loadings*comp.sdev
}

#compute coordinates
loadings<-res.pca$rotation
sdev<-res.pca$sdev
var.coord<-t(apply(loadings, 1, var_coord_func, sdev))
head(var.coord[,1:3])

#Compute coss2
var.cos2 <- var.coord^2
head(var.cos2[,1:3])

#Compute contributions
comp.cos2<-apply(var.cos2, 2, sum)
contrib<-function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib<-t(apply(var.cos2,1,contrib, comp.cos2))
head(var.contrib[,1:3])
var.contrib[, 1:3]


#for calculate the PCA results for each individuals, three steps
#coordinates of individuals. It had already been calculated in 184 line
ind.coord<-res.pca$x
head(ind.coord[,1:3])

#cos2 of individuals
#1 square of the distance between an individual and the PCA center of gravity
#PCA center os gravity
center.pca<-res.pca$center
scale.pca<-res.pca$scale
getdistance<- function(ind_row, center.pca, scale.pca){
  return(sum(ind_row-center.pca)/scale.pca^2)
}
d2<-apply(datdig1.log, 1, getdistance, center.pca, scale.pca)

#Compute the cos2. The sum of each row is 1
cos2.pca<-function(ind.coord, d2){return(ind.coord^2/d2)}
ind.cos2 <- apply(ind.coord, 2, cos2.pca, d2)
head(ind.cos2[, 1:4])




'''
#otro modo de hacerlo es con princomp
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

plot(dig.pc$scores[,2],datdig$P,xlab="PC1",ylab="P") ''' 

