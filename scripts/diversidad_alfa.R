##########  Datos a tabla

#Diversidad de Shannon, ajuste de Pielou

shan_pielou <- function(x) { {
  n <- length(x)
  total <- sum(x)
  shannon <- 0
  
  for (i in 1:n) {
    shannon <- shannon + ( (-1) * ( (x[i] / total) * log(x[i] / total) ) ) #ciclo for para no limitar el número de especies
  }
  
  pielou <- shannon / (log(n)) #x tiene que ser necesariamente un vector !!
}
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
  
  r_simpson <- round(simpson, 2)
  
  return (r_simpson)
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


####            Población 1 - perro

pob1_abundancias <- c(18, 4, 4, 10, 15, 5, 3, 3, 2, 2, 1, 1)

#índices
shan_pielou (pob1_abundancias)
simpson (pob1_abundancias)
chao1 (pob1_abundancias)



####            Población 2 - carita feliz


####            Población 1 - 



#################### TABLA DE ÍNDICES ALFA ###########################

#Función que haga una data.frame con los índices calculados 
indices_alfa <- function (x) {
  data.frame (
    shannon = shan_pielou(x)$shannon, 
    pielou = shan_pielou(x)$pielou, 
    simpson = simpson (x), 
    chao1 = chao1(x)
  )
}
indices_alfa(pob1_abundancias)


#Prototipo función basurita :p
pob1 <- data.frame(
  color = c("azul marino", "azul intermedio", "azul claro", "verde oscuro",
            "verde claro", "fucsia", "amarillo", "rosa", "naranja", "rojo",
            "cian", "pastel"),
  no.fichas = pob1_abundancias, 
  probabilidad =  for (i in 1:length(x)) { (x[i] / sum(x) )
  }
  shannon =  
    simpson =
    chao1 = 
)



