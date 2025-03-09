
################################################################################
###########################  DIVERSIDAD BETA  ##################################
################################################################################

###Cargar datos

data<-read.csv("data/datos.csv")
data

#Filtrar y transformar a presencia/ausencia

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

ind_jaccard2<-c(0.5694444,1)
for (a in 3:7) {
  i2<-sum(base[2,] & base[3:nrow(base),]==TRUE)
  u2<-sum(base[a,]| base[3:nrow(base),]==TRUE)
  r2<-c(i2/u2)
  ind_jaccard2<-c(ind_jaccard2,r2)
}
ind_jaccard2

ind_jaccard3<-c(0.9761905,0.9062500,1)
for (b in 4:7) {
  i3<-sum(base[3,] & base[4:nrow(base),]==TRUE)
  u3<-sum(base[b,]| base[4:nrow(base),]==TRUE)
  r3<-c(i3/u3)
  ind_jaccard3<-c(ind_jaccard3,r3)
}
ind_jaccard3


ind_jaccard4<-c(0.5857143,0.5178571,0.07317073,1)
for (c in 5:7) {
  i4<-sum(base[4,] & base[5:nrow(base),]==TRUE)
  u4<-sum(base[c,]| base[5:nrow(base),]==TRUE)
  r4<-c(i4/u4)
  ind_jaccard4<-c(ind_jaccard4,r4)
}
ind_jaccard4

ind_jaccard5<-c(0.7454545,0.5800000,0.07317073,0.5384615,1)
for (y in 6:7) {
  i5<-sum(base[5,] & base[6:nrow(base),]==TRUE)
  u5<-sum(base[y,]| base[6:nrow(base),]==TRUE)
  r5<-c(i5/u5)
  ind_jaccard5<-c(ind_jaccard5,r5)
}
ind_jaccard5

ind_jaccard6<-c(0.9761905 ,0.9062500, 0.09375000, 0.7368421, 0.6666667,1)
for (d in 7) {
  i6<-sum(base[6,] & base[7:nrow(base),]==TRUE)
  u6<-sum(base[d,]| base[7:nrow(base),]==TRUE)
  r6<-c(i6/u6)
  ind_jaccard6<-c(ind_jaccard6,r6)
}
ind_jaccard6

ind_jaccard7<-c(0.5857143, 0.5576923, 0.07317073, 0.4375000, 0.4210526, 0.1111111,1)

mat_jaccard<-cbind(ind_jaccard,ind_jaccard2,ind_jaccard3,ind_jaccard4,ind_jaccard5,ind_jaccard6,ind_jaccard7)
mat_jaccard
rownames(mat_jaccard)<-c("p1","p2","p3","p4","p5","p6","p7")
colnames(mat_jaccard)<-c("p1","p2","p3","p4","p5","p6","p7")


##Vamos a usar el paquete devtools para calcular las distancias entre los índices 
library(usethis)
library(devtools)

mat_jaccard##Matriz con los índices de Jaccard entre poblaciones

###Matriz de distancias con la función dist que vive en devtools
mat_dist_jaccard<-dist(mat_jaccard)
mat_dist_jaccard

}

jaccard(data1)

##Guardar los datos una carpeta con terminación .csv
write.csv(mat_dist_jaccard,file = "mat_dist_csv/matriz_distancia_indxJaccard.csv")
