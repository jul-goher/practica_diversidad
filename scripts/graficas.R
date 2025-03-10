library(ggplot2) # Hacer las graficas
library(tidyr) # Pasar a formato largo los valores 
library(dplyr) # Para hacer las curvas 

###############################################################
#
#              CURVA DE RAREFACCION
#
###############################################################
# Cargar base de datos
datos <- read.csv("datos.csv")

# Seleccion de solo los datos de muestreos
muestreos <- datos[ ,c(1,16:25)] 
# Invertir el orden de las columnas y renglones 
muestreos2 <- as.data.frame(t(muestreos[ , c(2:11)]))
# Renombrar columnas de acuerdo a la pob a la que corresponden
colnames(muestreos2) <- c(muestreos$poblacion) 
# Agregar una columna para numero de muestreo
muestreos2$muestreo <- c(1:10)

muestreos_prueba <- muestreos2 %>% 
  pivot_longer(cols = starts_with("P"), # Selec. todas las col que empiezan con P 
               names_to = "poblacion", # Poblacion
               values_to = "riqueza") # Sp
# Conservar unicamente los valores >0
muestreos_prueba <- muestreos_prueba[muestreos_prueba$riqueza > 0, ]

# CURVA DE RAREFACCIÓN

rarefac <- ggplot(muestreos_prueba, aes(x = muestreo, y = riqueza, color = poblacion)) +
  geom_point() +  # Puntos
  geom_line() + # Union de los puntos por lineas rectas
  geom_smooth(method = "loess", se = FALSE) + # Ajuste de los puntos a una curva
  labs(title = "Curva de rarefacción", x = "Muestreos", y = "Riqueza") + # Nomb de ejes y graf
  theme_minimal() # Estilo de la graf

print(rarefac)

###############################################################
#
#               RANK-ABUNDANCE CURVE
#
###############################################################

for (x in 1:7) {
  spp1 <- datos[x,c(1:14)] # Seleccionar poblacion
  spp1.1 <- as.data.frame(t(spp1[2:13])) # Cambiar orden de los valores
  colnames(spp1.1) <- "Abundancia" # Nombrar col
  
  spp1.1 <- spp1.1 %>%
    arrange(desc(Abundancia)) # Ordenar de forma decreciente
  
  spp1.1$spp <- c(1:length(spp1.1$Abundancia)) # Numerar spp
  
  spp1.1 <- spp1.1[spp1.1$Abundancia != 0, ] # Quitar spp NO presentes
  
  
  rank_ab <- ggplot(spp1.1, aes(x = spp, y = Abundancia, colour = spp)) +
    geom_point() +  # Puntos
    geom_line() + # Linea de union
    labs(title = "Rank-abundance", x = "Spp", y = "Abundancia")
  print(rank_ab)
}

