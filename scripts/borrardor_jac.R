data<-read.csv("data/datos.csv")
data

#Filtrar y transformar a presencia/ausencia con TRUE/FALSE

data1<-data[ ,c(2:13)]>0
data1
jaccard<-function(x,y){#Dado el conjunto un conjunto X y uno Y
  inter<-sum(x&y==TRUE)
  union<-sum(x|y==TRUE)
  res<-inter/union
  print(res)
}
jaccard(a,b)

  ind_jac<-c()
  for (i in 1:nrow(data1)){
    for (j in i:nrow(data1)) {
      inter<-sum(data1[i,] & data1[j,])
      union<-sum(data1[i,] | data1[j,])
      res<-inter/union
      ind_jac<-c(ind_jac,res)
    }
  }
  
  ind_jac
  
  matd_jaccard<-matrix(ind_jac,nrow = 6,ncol = 6)
  rownames(matd_jaccard)<-c("p2","p3","p4","p5","p6","p7")
  colnames(matd_jaccard)<-c("p1","p2","p3","p4","p5","p6")
  print(matd_jaccard)
#ahhhhhhhhh
