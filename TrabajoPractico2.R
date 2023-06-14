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
library(gridExtra)
library(ggplot2)
 
wd = "C:/Users/Fabio/OneDrive/Documentos/Academic/TUIA/IA31"
setwd(wd)

ruta = read.csv("./ejercicio2_tp2.csv")
ruta = as.data.frame(ruta)

# Muestra de las primeras filas
head(ruta)


## Analisis:

# Tratamiento Primario

summary(ruta)
str(ruta)

# Las columnas Espesor y Resistencia son de tipo string
ruta$Espesor <- as.numeric(gsub(",", ".", ruta$Espesor))
ruta$Resistencia <- as.numeric(gsub(",", ".", ruta$Resistencia))

# Renombrar la columna
colnames(ruta)[1] = 'Puntos'

# Los datos ahora parecen estar bien. Veamos en un grafico sus distribuciones
hist_espesor <- ggplot(ruta, aes(x = Espesor)) + geom_histogram()
hist_resistencia <- ggplot(ruta, aes(x = Resistencia)) + geom_histogram()
scatterplot <- ggplot(ruta, aes(x = Espesor,  y = Resistencia)) + geom_point()

grid.arrange(hist_resistencia, hist_espesor, scatterplot, nrow = 3)

