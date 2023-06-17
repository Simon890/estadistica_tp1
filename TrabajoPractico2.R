#### Trabajo Practico Probabilidad y Estadistica Parte 2 ####

### Actividad 1 ###


### Actividad 2 ###

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
# - Correlación: Investigaremos la relación entre el espesor y la resistencia de la ruta.


## Recolecion Datos:

# Los datos son provistos por el enunciado. Consta de una tabla de 100 registros (puntos) donde se almacena el Punto, la Resistencia en MPa y el Espesor en cm.

install.packages("ggplot2")
install.packages("gridExtra")
install.packages("DescTools")
install.packages("nortest")

library(gridExtra)
library(ggplot2)
library(DescTools)
library(nortest)
 
wd = "/home/Shannon/Documents/Academic/IA_3_1"
setwd(wd)

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

# Visualizar los cinco gráficos
grid.arrange(hist_espesor, hist_resistencia, scatterplot, boxplot_espesor, boxplot_resistencia,
             layout_matrix = rbind(c(1, 2), c(3, 3), c(4, 5)))

# Calcular el porcentaje de puntos que tienen una resistencia a la compresión mayor o igual a 30 MPa 
ruta$ResistenciaMayorIgualA30 <- ifelse(ruta$Resistencia >=30 ,1 ,0)
porcentaje_resistencia_mayor_igual_30 <- sum(ruta$ResistenciaMayorIgualA30)/nrow(ruta)*100
porcentaje_resistencia_mayor_igual_30

# Calcular el promedio del espesor
promedio_espesor <- mean(ruta$Espesor)
promedio_espesor

# Correlacion entre las variable
cor(ruta$Espesor, ruta$Resistencia)


# Anaisis Inferencial

# Probemos la normalidad de la variable Resistencia
qqnorm(ruta$Resistencia)
qqline(ruta$Resistencia)

ad.test(ruta$Resistencia)

# Probemos la normalidad de la variable Espesor
qqnorm(ruta$Espesor)
qqline(ruta$Espesor)

ad.test(ruta$Espesor)

# Ambos poseen valores p-value > 10, por lo que no podemos descartar la normalidad

# Vimos que el promedio muestral es de 22.41197 != 22. Pero, podemos asegurar que esto se cumple para toda la poblacion?
MeanCI(ruta$Espesor, sd=NULL, method="classic", conf.level=0.99)

