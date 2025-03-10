
################################################################################
###########################  DIVERSIDAD BETA  ##################################
################################################################################

###Cargar datos

data<-read.csv("data/datos.csv")
data

#Filtrar y transformar a presencia/ausencia con TRUE/FALSE

data1<-data[ ,c(2:13)]>0
data1

##pre
a<-data1[4,]
b<-data1[5,]
jaccard<-function(a,b){#Dado el conjunto un conjunto A y uno B
  inter<-sum(a&b==TRUE)
  union<-sum(a|b==TRUE)
  res<-inter/union
  print(res)
}
jaccard(a,b)

####Para Jaccard####

#Hacer una función

jaccard<-function(base){
ind_jaccard<-c(1)

for (i in 2:nrow(base)){
  i1<-sum(base[1,] & base[2:nrow(base),]==TRUE)
  u1<-sum(base[i,]| base[2:nrow(base),]==TRUE)
  r1<-c(i1/u1)
  ind_jaccard<-c(ind_jaccard,r1)
}
ind_jaccard

ind_jaccard2<-c(0,1)
for (a in 3:7) {
  i2<-sum(base[2,] & base[3:nrow(base),]==TRUE)
  u2<-sum(base[a,]| base[3:nrow(base),]==TRUE)
  r2<-c(i2/u2)
  ind_jaccard2<-c(ind_jaccard2,r2)
}
ind_jaccard2

ind_jaccard3<-c(0,0,1)
for (b in 4:7) {
  i3<-sum(base[3,] & base[4:nrow(base),]==TRUE)
  u3<-sum(base[b,]| base[4:nrow(base),]==TRUE)
  r3<-c(i3/u3)
  ind_jaccard3<-c(ind_jaccard3,r3)
}
ind_jaccard3


ind_jaccard4<-c(0,0,0,1)
for (c in 5:7) {
  i4<-sum(base[4,] & base[5:nrow(base),]==TRUE)
  u4<-sum(base[c,]| base[5:nrow(base),]==TRUE)
  r4<-c(i4/u4)
  ind_jaccard4<-c(ind_jaccard4,r4)
}
ind_jaccard4

ind_jaccard5<-c(0,0,0,0,1)
for (y in 6:7) {
  i5<-sum(base[5,] & base[6:nrow(base),]==TRUE)
  u5<-sum(base[y,]| base[6:nrow(base),]==TRUE)
  r5<-c(i5/u5)
  ind_jaccard5<-c(ind_jaccard5,r5)
}
ind_jaccard5

ind_jaccard6<-c(0,0,0,0,0,1)
for (d in 7) {
  i6<-sum(base[6,] & base[7:nrow(base),]==TRUE)
  u6<-sum(base[d,]| base[7:nrow(base),]==TRUE)
  r6<-c(i6/u6)
  ind_jaccard6<-c(ind_jaccard6,r6)
}
ind_jaccard6

ind_jaccard7<-c(0,0,0,0,0,0,1)

mat_jaccard<-cbind(ind_jaccard,ind_jaccard2,ind_jaccard3,ind_jaccard4,ind_jaccard5,ind_jaccard6,ind_jaccard7)
mat_jaccard
rownames(mat_jaccard)<-c("p1","p2","p3","p4","p5","p6","p7")
colnames(mat_jaccard)<-c("p1","p2","p3","p4","p5","p6","p7")

mat_jaccard##Matriz con los índices de Jaccard entre poblaciones
}

jaccard(data1)

##Guardar los datos una carpeta con terminación .csv
write.csv(mat_jaccard,file = "mat_dist_csv/matriz_distancia_indxJaccard.csv")


####Bray-Curtis####

##Con vegdist que vive en vegan 

library(lattice)
library(permute)
library(vegan)

#Filtrar

data2<-data[ ,c(2:13)]
data2

#Matriz
bd <- vegdist(data2, method = "bray")
bd

##pre
data3<-split(data2, 1:nrow(data2))##tengo una lista
data3
class(data3)

###Especies comunes

data3$`1`>0 & data3$`2` >0->com

