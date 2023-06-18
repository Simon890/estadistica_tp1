#### Trabajo Practico Probabilidad y Estadistica Parte 2 ####

install.packages("ggplot2")
install.packages("gridExtra")
install.packages("DescTools")
install.packages("nortest")
install.packages("ggridges")
install.packages("dplyr")

library(gridExtra)
library(ggplot2)
library(DescTools)
library(nortest)
library(ggridges)
library(dplyr)
 
wd = "/home/Shannon/Documents/Academic/IA_3_1"
setwd(wd)

#### Actividad 1 ####










#### Actividad 2 ####

## Definicion del problema

# El problema consiste en determinar si un tramo de ruta de 5 km ya construido cumple con las exigencias de calidad en términos de espesor y resistencia.
# Se desea analizar si la resistencia a la compresión y el espesor promedio de la ruta cumplen con los requisitos establecidos.
# Además, se pretende evaluar la homogeneidad de la ruta en términos de espesor y resistencia en los diferentes sectores del tramo.

# Población: Tramo completo de ruta de 5 km que ya ha sido construido.
# Muestra: 100 puntos seleccionados aleatoriamente tomados a lo largo de ese tramo donde se realizaron mediciones de espesor y resistencia.

# Variables:
# - Resistencia a la compresión: Variable cuantitativa continua. Representa la capacidad de la ruta para soportar cargas y resistir la compresión medido en MPa.
# - Espesor: Variable cuantitativa continua. Indica el grosor de la capa de la ruta en cm.

# Estadisticos:
# - Al menos el 5% de los puntos de la muestra deben tener una resistencia a la compresión que 30 MPa.
# - El espesor promedio debe ser igual a 22 cm.
# - Homogeneidad de la muestra.


## Planificacion

# El objetivo es determinar si la ruta cumple con las exigencias de calidad en cuanto a espesor y resistencia, y 
# evaluar la homogeneidad de la ruta en términos de espesor y resistencia en los diferentes sectores del tramo.
# Para ello debemos analizar las muetras y obtener los parametros muestrales, y con ellos estimar los parámetros poblacionales.

# Los estudios que se realizaran:
# - Descriptivo: Realizaremos un análisis descriptivo de los datos recopilados para comprender la distribución y características de las variables.
# - Inferencial: Utilizaremos técnicas de inferencia estadística para hacer estimaciones sobre los parámetros poblacionales .
# - Homogeneidad: Dividiremos el tramo en sectores y realizaremos análisis comparativos para evaluar la homogeneidad de la ruta.


## Recolecion Datos:

# Los datos son provistos por el enunciado. Consta de una tabla de 100 registros (puntos) donde se almacena el Punto, la Resistencia en MPa y el Espesor en cm.


ruta = read.csv("./ejercicio2_tp2.csv")
ruta = as.data.frame(ruta)

# Muestra de las primeras filas
head(ruta)



## Analisis:


# Tratamiento Primario

str(ruta)

# Las columnas Espesor y Resistencia son de tipo string
ruta$Espesor <- as.numeric(gsub(",", ".", ruta$Espesor))
ruta$Resistencia <- as.numeric(gsub(",", ".", ruta$Resistencia))

# Renombrar la columna
colnames(ruta)[1] = 'Punto'



# Analisis Descriptivo

summary(ruta)

# Los datos ahora parecen estar bien. Veamos en un grafico sus distribuciones
hist_espesor <- ggplot(ruta, aes(x = Espesor)) + geom_histogram()
hist_resistencia <- ggplot(ruta, aes(x = Resistencia)) + geom_histogram()
scatterplot <- ggplot(ruta, aes(x = Espesor,  y = Resistencia)) + geom_point()

# Crear un boxplot para la variable de resistencia
boxplot_resistencia <- ggplot(ruta, aes(x = factor(0), y = Resistencia)) +
  geom_boxplot() +
  labs(x = "", y = "Resistencia (MPa)")

# Crear un boxplot para la variable de espesor
boxplot_espesor <- ggplot(ruta, aes(x = factor(0), y = Espesor)) +
  geom_boxplot() +
  labs(x = "", y = "Espesor (cm)")

# Visualizar los gráficos
grid.arrange(hist_espesor, hist_resistencia, scatterplot, boxplot_espesor, boxplot_resistencia,
             layout_matrix = rbind(c(1, 2), c(3, 3), c(4, 5)))

# Vemos que ambas variables son aproximadamente normales. Que hay poca correlacion entre las mismas y algunas medidas de resumen

# Correlacion entre las variable
cor(ruta$Espesor, ruta$Resistencia)


# Calcular el porcentaje de puntos que tienen una resistencia a la compresión mayor o igual a 30 MPa 
ruta$ResistenciaMayorIgualA30 <- ifelse(ruta$Resistencia >=30 ,1 ,0)
porcentaje_resistencia_mayor_igual_30 <- sum(ruta$ResistenciaMayorIgualA30)/nrow(ruta)*100
porcentaje_resistencia_mayor_igual_30

# Para esta muestra, el 82% de los puntos tienen resistencia a la compresion mayor a 30MPa

# Calcular el promedio del espesor
promedio_espesor <- mean(ruta$Espesor)
promedio_espesor

# El promedio de espesor de la muestra no es exactamente 22, pero si muy similar



# Anaisis Inferencial

# Probemos la normalidad de la variable Resistencia
qqnorm(ruta$Resistencia)
qqline(ruta$Resistencia)

ad.test(ruta$Resistencia)

# Los puntos son cernanos a la linea en el centro, y su p-value=0.419

# Probemos la normalidad de la variable Espesor
qqnorm(ruta$Espesor)
qqline(ruta$Espesor)

ad.test(ruta$Espesor)

# Muchos puntos son cercanos a la linea y su p-value=0.485

# Ambos poseen valores p-value > 10, por lo que no podemos descartar la normalidad


# Vimos que el promedio muestral es de 22.41197 != 22. Pero, podemos asegurar que esto se cumple para toda la poblacion?
MeanCI(ruta$Espesor, sd=NULL, method="classic", conf.level=0.99)

# Como no conocemos el desvio estandar poblacional dejamos que R lo estime y utilice un 99% de confianza para estimar el intervalo
# Podemos asegurar entonces, que con un 99% de confianza, el espesor medio de la ruta es de 22.41197 ± 1.19834 centimetros


# El requerimiento era que al menos el 5% de los puntos de la muestra deben tener una resistencia a la compresión mayory que 30 MPa,
# y como calculamos conporcentaje_resistencia_mayor_igual_30, vemos que el porcentaje de puntos mayores a 30MPa es de 82%.
# Pero, podemos asumir que eso ocurre en toda la poblacion?
BinomCI(sum(ruta$ResistenciaMayorIgualA30), nrow(ruta), conf.level = 0.99, method = "wald")

# Podemos asegurar con un 99% de confianza que la proporcion de puntos que tienen una resistencia a la compresion mayor a 30 MPa es de 0.82 ± 0.0989601 

# Por lo tanto, podemos concluir que al menos el requerimiento de resistencia se cumple para toda la ruta.
# Pero en cambio, del espesor no podemos asegurar que se cumpla ya que puede estar entre 21.213 y 23.610.



# Analisis de Homogeneidad

# para analizar la homogeidad podemos comparar las distribuciones de las variables directamente
resistencia_mean <- mean(ruta$Resistencia)
resistencia_sd <- sd(ruta$Resistencia)
espesor_mean <- mean(ruta$Espesor)
espesor_sd <- sd(ruta$Espesor)

res <- ggplot(ruta) + 
       geom_density(aes(Resistencia), fill = "#B4E4FF", alpha = 0.5) +
       theme_bw() + ggtitle("Resistencia")  +
       annotate("text", x = resistencia_mean, y = 0.1, label = paste("Mean =", round(resistencia_mean, 2), "\nSD =", round(resistencia_sd, 2)), hjust = 0, vjust = 0)

esp <- ggplot(ruta) + 
       geom_density(aes(Espesor), fill = "#E8A0BF", alpha = 0.5) + 
       theme_bw() + ggtitle("Espesor") +
       annotate("text", x = espesor_mean, y = 0.1, label = paste("Mean =", round(espesor_mean, 2), "\nSD =", round(espesor_sd, 2)), hjust = 0, vjust = 1)

grid.arrange(res, esp, ncol = 1)

# Ambas son aproximadente normales y la variabilidad de ambas son relativamente bajas. Podemos asumir que son homogeneas.
# Notar que la desviacion de Espesor es mayor, no solo por que es menos homegenea, sino tambien por que su rango de valores es mas amplio.

# Para analizar la homogeneidad tambien puede ser util separar la muestra por sectores.

# Dividir el tramo en 4 partes iguales según el orden en el que se registraron los puntos
ruta$Sector <- cut(ruta$Punto, breaks = 4, labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4"))

# Calcula la desviacion de cada sector
espesor_sd <- ruta %>% group_by(Sector) %>% summarise(sd = sd(Espesor))

# Crear un raincloud plot para la variable de espesor
espesor_por_sector <- ggplot(ruta, aes(x = Espesor, y = Sector)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.1)) +
  geom_boxplot(width = 0.2, alpha = 0.3) +
  geom_density_ridges(aes(fill = Sector), scale = 0.9, alpha = 0.4) +
  scale_color_brewer(palette = "Pastel1") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Espesor (cm)", y = "Sector") +
  theme(legend.position = "none") +
  geom_text(data = espesor_sd, aes(x = Inf, y = Sector, label = paste("SD =", round(sd, 2))), hjust = 1.1, vjust=1)

# Calcula le desviacion estandar de cada sector
resistencia_sd <- ruta %>% group_by(Sector) %>% summarise(sd = sd(Resistencia))

# Crear un raincloud plot para la variable de resistencia
resistencia_por_sector <- ggplot(ruta, aes(x = Resistencia, y = Sector)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.1)) +
  geom_boxplot(width = 0.2, alpha = 0.3) +
  geom_density_ridges(aes(fill = Sector), scale = 0.9, alpha = 0.4) +
  scale_color_brewer(palette = "Pastel1") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Resistencia (MPa)", y = "Sector") +
  theme(legend.position = "none") +
  geom_text(data = resistencia_sd, aes(x = Inf, y = Sector, label = paste("SD =", round(sd, 2))), hjust = 1.1, vjust=1)

# Visualizar los gráficos
grid.arrange(espesor_por_sector, resistencia_por_sector, nrow = 2)

# El grafico nos muestra que para la variable Espesor la variablidad es homogenea para los sectores 1, 2 y 4. Para el sector 3 el valor es algo diferente
# Para la variable Resistencia, la variablidad se mantiene en valores cercanos a 1.






