# Ejercicio 3 | Opción A | Dataset E

# La vitamina D es un nutriente muy importante para la salud humana ya que es 
# una prohormona implicada en el desarrollo de diversos tejidos del organismo, 
# entre otras funciones; como la absorción de calcio. En forma natural se 
# obtiene a través de la exposición directa de la piel a los rayos UVB solares. 
# Asimismo, se puede adquirir a través de diferentes alimentos, como peces 
# grasos, cereales, lácteos, aceite de hígado de bacalao, huevos, entre otros. 
# Un laboratorio se encuentra desarrollando complementos vitamínicos para 
# garantizar que los niveles de esta vitamina, entre sus consumidores, se 
# mantengan por encima de lo recomendado (20 microgramos/día). Como control del 
# proceso, seleccionaron aleatoriamente 150 comprimidos (uno de cada lote 
# disponible en ese momento en el depósito) y midieron el contenido de vitamina D.

# ¿Considera que los complementos vitamínicos cumplen con las especificaciones 
# en relación al contenido de vitamina D?  ¿Qué medidas (parámetros) serían de 
# interés en este caso?

# A) Plantee el problema, defina población, variable y parámetro/s de interés.
# B) Plantee un objetivo en términos de dicho/s parámetro/s.
# C) Analice exhaustivamente los datos (incluya dos gráficos). Responda al 
# objetivo planteado. Indique si sus conclusiones son preliminares o definitivas. 



# Primeros descargamos e importamos los paquetes que vamos a utilizar
install.packages("qcc")
install.packages("ggplot2")
install.packages("readr")
library("qcc")
library("ggplot2")
library("readr")

# Seteamos el directorio de trabajo, que puede ir variando dependiendo donde se ejecute
setwd("D:\\Datos basura\\R Scripts")

# Luego importamos el dataset en una variable para poder trabajarlo dentro de R
df <- read.csv2("ej3.csv")


# Antes de comenzar a resolver las actividades vamos a considerar que las
# preguntas que se realizaron entre el enunciado que explica la situación del
# laboratorio y los ejercicios A, B y C se van a responder dentro de los
# ejercicios asignados. Decidimos esta medida para evitar redundancia de datos.


# Ejercicio A
# La población está compuesta por todas las pildoras contenedoras de Vitamina D
# que realice este laboratorio aunque la muestra solo consta de 150 pildoras.

# La variable es la cantidad de Vitamina D (en mg) que contiene cada pildora.
# Esta variable es de tipo cuantitativa continua ya que mide una canitdad numerica.

# El parametro de interes es el porcentaje de pildoras que superan la cantidad
# recomendada de vitamina D.


# Ejercicio B

# El objetivo es observar si el porcentaje de pildoras que superen la cantidad
# de Vitamina D recomendada es suficiente como para asumir que la pildora es
# "confiable". Si bien el enunciado no nos brinda un porcentaje a superar nosotros
# vamos a asumir que a partir de 80% es un porcentaje aceptable para poder
# confiar en estas pildoras.


# Ejercicio C
# Asignamos un valor de mayor o menor en relacion al contenido de vitamina D.
df$resumen_contenido <- ifelse(df$Cant.Vitamina.D >= 20, "Mayor", "Menor")

# Creamos un nuevo DF en donde tenemos los valores de frecuencia y porcentaje
# correspondientes a la relacion del contenido de vitamina D en cada pildora.
df_porc <- as.data.frame(table(df$resumen_contenido))
colnames(df_porc) <- c("Cantidad", "Frec")
df_porc$Porc <- round(df_porc$Frec/sum(df_porc$Frec) * 100, 2)

# Realizamos un gráfico de torta para ver los resultados del mismo.
torta <- pie(df_porc$Porc, df_porc$Cantidad, col = c("#aee637", "#ef3353"), main = "Relación del contenido de vitamina D sobre lo recomendado entre todas las pildoras")

# Ahora pasaremos a ver si es que el porcentaje de pildoras que superan el contenido
# recomendado por día es el suficiente como para confiar en las pildoras distribuidas
# por el laboratorio.
barplot_df <- barplot(df_porc$Porc, ylab = "Porcentaje", xlab = "Relación", main = "Relación del contenido de vitamina D sobre lo recomendado entre todas las pildoras", col = c("#aee637", "#ef3353"), ylim = c(0, 100))
axis(1, at = barplot_df, labels=df_porc$Cantidad, cex.axis=0.9)
text(barplot_df, df_porc$Porc + 3, labels = paste(df_porc$Porc, "%"))

# Trazamos una linea recta que determinará si el porcentaje de pildoras que cumplen
# con al menos el contenido recomendado son lo suficiente para asumir la confiabilidad
# de las pildoras del laboratorio
abline(h = 80, lty = 2, col = "blue")
text(83, "Porcentaje de confiabilidad ", col = "blue", font = 2)

# Una vez realizado los gráficos podemos dar como finalizado este problema y podemos
# concluir de manera definitiva que las pildoras no son confiables, si bien la mayoría
# cumple con lo que se estipula no es suficiente para poder confiar en que estas pildoras
# puedan proveer el contenido de vitamina D que se necesita por día.
