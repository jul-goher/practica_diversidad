##########  Datos a tabla

#Diversidad de Shannon, ajuste de Pielou
shan_pielou <- function(x) { {
  n <- length(x)
  total <- sum(x)
  shannon <- 0
  
  for (i in 1:n) {
    shannon <- shannon + ( (-1) * ( (x[i] / total) * log(x[i] / total) ) ) #ciclo for para no limitar el número de especies
  }
  #Pielou 
  pielou <- shannon / (log(n)) #x tiene que ser necesariamente un vector !!
}
  #Redondeo de los índices
  r_shannon <- round (shannon, 2)
  r_pielou <- round (pielou, 2)
  
  return ( 
    list (shannon = r_shannon,
          pielou = r_pielou) 
  )

}

#Diversidad de Simpson 
simpson <- function (x) {
  N <- sum(x)
  simpson <- 0
  
  for (i in 1:length(x)) { #ya corregí el error 
    simpson <- simpson + ( (x[i] * (x[i] - 1) ) / ( N * (N - 1) ) )
  }
  #Inverso de Simpson 
  inv_simpson <- (1/simpson)
  
  #índice de Gini-Simpson
  gini <- (1 - simpson) #No estoy segura si es -inverso o -simpson, dejé el último
  
  #Redondeo de los índices
  r_simpson <- round (simpson, 2)
  r_inv_simpson <- round (inv_simpson, 2)
  r_gini <- round(gini, 2)
  
  return ( list (simpson = r_simpson, 
                 inv_simpson = r_inv_simpson, 
                 gini = r_gini)
  )
}


#Diversidad de CHAO1
chao1 <- function (x) {
  s_obs <- length (x)
  f1 <- sum (x == 1)
  f2 <- sum (x == 2)
  
  chao1 <- s_obs + ( f1^2 / ( 2 * f2) )
  
  r_chao1 <- round (chao1, 2)
 
 return (r_chao1)
 
}

#Base de datos 
#Como está seperado por ";" y no comas, copié el código que aparece al importar el
datos <- read_delim("data/datos.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)
View (datos)


########## INDICES POR POBLACIÓN  ##########

####            Población 1
pob1 <- datos[1, 2:13]
pob1

pob1_shan <- shan_pielou(pob1)
pob1_shan #Da los valores correctos, pero añade la palabra "marino" al inicio

pob1_simpson <- simpson(pob1)
pob1_simpson

pob1_chao <- chao1(pob1)


#Confirmar los índices con vegan
install.packages("vegan")
library(vegan)
shannon_veg_pob1 <- diversity(pob1, index = "shannon")
shannon_veg_pob1 #son iguales al de la función 

chao1_veg_pob1 <- estimateR(pob1)["S.chao1",]
chao1_veg_pob1 #La función calcula 13, el de vegan calcula 12.3

####            Población 2 

pob2 <- datos[2, 2:13]
pob2

pob2_shan <- shan_pielou(pob2)
pob2_simpson <- simpson(pob2)
pob2_chao <- chao1(pob2)

####            Población 3
pob3 <- datos[3, 2:13]
#sólo tiene fichas naranjas, así que hay que remover los ceros
pob3 <- pob3[pob3 > 0]
pob3

pob3_shan <- shan_pielou(pob3)
pob3_shan
pob3_simpson <- simpson(pob3)
pob3_simpson
pob3_chao <- chao1(pob3)
pob3_chao #Da un valor de NaN

####            Población 4
pob4 <- datos[4, 2:13]
pob4
#0 fichas rosas y amarillas, entonces:
pob4 <- pob4[pob4 > 0]
pob4

pob4_shan <- shan_pielou(pob4)
pob4_simpson <- simpson(pob4)
pob4_chao <- chao1(pob4)

####            Población 5 
pob5 <- datos[5, 2:13]
#Eliminar ceros
pob5 <- pob5[pob5 > 0]
pob4

pob5_shan <- shan_pielou(pob5)
pob5_simpson <- simpson(pob5)
pob5_chao <- chao1(pob5)

####            Población 6 
pob6 <- datos[6, 2:13]
#Sólo tiene 66 fichas color pasto
pob6 <- pob6[pob6 > 0]
pob6

pob6_shan <- shan_pielou(pob6)
pob6_simpson <- simpson(pob6)
pob6_chao <- chao1(pob6)

####            Población 7 
pob7 <- datos[7, 2:13]
#Cero fichas amarillas y naranjas
pob7 <- pob7[pob7 > 0]
pob7

pob7_shan <- shan_pielou(pob7)
pob7_simpson <- simpson(pob7)
pob7_chao <- chao1(pob7)

              #######################################
############## ÍNDICES ALFA DE TODAS LAS POBLACIONES ####################
              ######################################

#Elimine list, no corría con esa
#Puse as.numeric porque creo que lo tomaba en cuenta como un vector aún y salían 7 columnas por cada index, en vez de 7 rows

indices_alpha <- data.frame( 
  poblaciones = c("P1", "P2", "P3", "P4", "P5", "P6", "P7"), 
  
  shannon = as.numeric (c (pob1_shan$shannon, pob2_shan$shannon, pob3_shan$shannon, pob4_shan$shannon,
                           pob5_shan$shannon, pob6_shan$shannon, pob7_shan$shannon) ), 
  
  pielou = as.numeric (c (pob1_shan$pielou, pob2_shan$pielou, pob3_shan$pielou, pob4_shan$pielou,
                          pob5_shan$pielou, pob6_shan$pielou, pob7_shan$pielou) ), 
  
  simpson = as.numeric (c (pob1_simpson$simpson, pob2_simpson$simpson, pob3_simpson$simpson, pob4_simpson$simpson,
                           pob5_simpson$simpson, pob6_simpson$simpson, pob7_simpson$simpson) ), 
  
  inv_simpson = as.numeric ( c (pob1_simpson$inv_simpson, pob2_simpson$inv_simpson, pob3_simpson$inv_simpson, pob4_simpson$inv_simpson,
                                pob5_simpson$inv_simpson, pob6_simpson$inv_simpson, pob7_simpson$inv_simpson) ), 
  
  gini = as.numeric (c (pob1_simpson$gini, pob2_simpson$gini, pob3_simpson$gini, pob4_simpson$gini,
                        pob5_simpson$gini, pob6_simpson$gini, pob7_simpson$gini) ), 
  
  chao1 = as.numeric (c (pob1_chao, pob2_chao, pob3_chao, pob4_chao, pob5_chao, pob6_chao, pob7_chao) )
)

print(indices_alpha) #Ahora sí imprime bien el data.frama







