##########################
# TRABAJO PRÁCTICO Nro 1 #
##########################


# Seteamos el directorio de trabajo
wd = "Guardar acá el directorio de trabajo correspondiente"
setwd(wd)


# Instalación de paquetes
install.packages("qcc")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

library("qcc")
library("ggplot2")
library("readr")
library("dplyr")


###############
# Actividad 1 #
###############

# Población: Estudiantes de Probabilidad y Estadística de la Comisión 2

### Objetivos: ###
# 1) Porcentaje de la cantidad de formación académica alcanzada (cualitativa)
# 2) Cuántos alumnos tienen Álgebra y Cálculo aprobadas? (cualitativa)
# 3) Cuál es el promedio del colesterol en sangre de los alumnos que nacieron en Marzo? (cuantitativa)

### Regategorizaciones ###
# En el ejercicio 1.3 se va a convertir el colesterol en tres variables cualitatativas:
# Colesterol bajo: Si el colesterol es menor o igual a 65
# Colesterol normal: Si el colesterol es mayor a 65 y menor o igual a 70
# Colesterol alto: Si el colesterol es mayor a 70

### Variables mal medidas o categorizadas ###
# Mes de nacimiento: Los valores de esta variable poseen distintos formatos lo cual dificulta el uso estadístico. Lo mejor hubiera sido guardar todo como "Enero", "Febrero", "Marzo"... o su valor numérico: "1", "2", "3"...
# Grupo sanguíneo: Los valores tienen diferentes formatos lo cual dificulta el trabajo. Ejemplo: 0+, 0(+), 0 (+), 0( -)
# Gusto por el color verde: En este caso, para obtener más información, hubiera sido mejor preguntar qué color es el que se prefiere. Luego, se puede contar la cantidad de "Verdes" que aparecen y saber a cuántas personas les gusta el color verde. Esto se podría repetir por cada color.
# Tiene Álgebra y Cálculo aprobadas?: Esta variable sería mejor tenerla separada en dos variables distintas: "Álgebra aprobada" y "Cálcula aprobada". De esta manera, para las personas que pusieron "no", se podría saber si tienen al menos una aprobada, lo cual permite realizar diferentes anlálisis.

# Cargar archivo csv en una tabla
encuesta = read.delim("./ejercicio1.csv", header = TRUE, sep=",")

# Renombrar columnas
colnames(encuesta)[1] = "res_nro"
colnames(encuesta)[2] = "legajo"
colnames(encuesta)[3] = "mes_nacimiento"
colnames(encuesta)[4] = "sodio_orina"
colnames(encuesta)[5] = "colesterol_sangre"
colnames(encuesta)[6] = "vit_d_sangre"
colnames(encuesta)[7] = "form_academica"
colnames(encuesta)[8] = "area_estudio"
colnames(encuesta)[9] = "hobby"
colnames(encuesta)[10] = "grupo_sangre"
colnames(encuesta)[11] = "gusta_verde"
colnames(encuesta)[12] = "ejercicio_frecuencia"
colnames(encuesta)[13] = "alg_calc_aprobadas"
colnames(encuesta)[14] = "rinde_flotantes"

# Reemplazar la coma por punto. Ejemplo: 4,5 => 4.5
encuesta$colesterol_sangre = scan(text = encuesta$colesterol_sangre, dec=",", sep=".")

### Respuesta 1:
encuesta_frame = as.data.frame(table(encuesta$form_academica))
encuesta_frame$porc = round(encuesta_frame$Freq/sum(encuesta_frame$Freq) * 100, 2)

encuesta_barplot = barplot(encuesta_frame$porc, 
                           ylab="Porcentaje",
                           main = "Porcentaje de la formación académica de los alumnos",
                           xlab="Tipo de formación",
                           col = c("#98D8AA", "#10A19D", "#F7D060", "#FF6D60", "#D82148"), ylim = c(0, 50))
axis(1, at = encuesta_barplot, labels=encuesta_frame$Var1, cex.axis=0.9)
text(encuesta_barplot, encuesta_frame$porc + 1, labels = paste(encuesta_frame$porc, "%"))

### Respuesta 2:
# Aplicamos un criterio de inclusión en el cual las respuestas que sean distintas de "Si" o "No" 
# No se tienen en cuenta (Trabajamos con la muestra y no con la población)
encuesta_filtrada = subset(encuesta, encuesta$alg_calc_aprobadas == "Sí" | encuesta$alg_calc_aprobadas == "No")
encuesta_frame2 = as.data.frame(table(encuesta_filtrada$alg_calc_aprobadas))

encuesta_pie = pie(encuesta_frame2$Freq,
                   labels = paste(c("Desaprobado: ", "Aprobado: "), encuesta_frame2$Freq),
                   main = "Cantidad de personas que aprobaron algebra y calculo", col = c("#FF6D60", "#609966"))

### Respuesta 3
encuesta_colesterol = encuesta[, c("res_nro", "colesterol_sangre")]
encuesta_colesterol$promedio = mean(encuesta_colesterol$colesterol_sangre)

plot(encuesta_colesterol$res_nro,
     encuesta_colesterol$colesterol_sangre,
     type="l", lwd=2, col="#009EFF",
     main = "Colesterol en sangre", xlab = "Nro Respuesta", ylab="Colesterol en sangre")
lines(encuesta_colesterol$res_nro, encuesta_colesterol$promedio, lwd=2, col="#D61355")
legend("topleft", legend = c("Promedio", "Colesterol"), lwd=c(2, 2), col=c("#D61355", "#009EFF"))

# Recategorización Respuesta 3
encuesta_recat = as.data.frame(encuesta)
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre > 70] = "Alto"
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre > 65 & encuesta_recat$colesterol_sangre <= 70] = "Normal"
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre <= 65] = "Bajo"

barplot(table(encuesta_recat$colesterol_sangre),
        col=c("#C0DBEA","#BA90C6","#E8A0BF"),
        ylab = "Cantidad",
        xlab = "Categoria",
        main="Frequencia de categorias",
        ylim=c(0,25))


###############
# Actividad 2 #
###############

# Reconsidere el problema 2, Material 1 (pág. 6)
# “En una empresa distribuidora de placas de madera de gran tamaño de uso para la
#  industria de la construcción, se preparan lotes de 80 unidades para sus clientes. 
#  La empresa tiene un sistema láser que detecta defectos de cada placa y lleva 
#  un registro del número de defectos de todas las placas de cada lote. 

#  Uno de los clientes es estricto en relación al número total de defectos (poros, rayaduras, etc.)
#  presentes en la superficie de las placas y exige que el número promedio de defectos
#  por placa en cada lote sea menor a 1,2 unidades. Al momento de recibir el pedido
#  por parte de este cliente, en la distribuidora cuentan con un lote ya preparado 
#  y desean saber si pueden enviárselo o no” 

# a) ¿Son aptas las maderas para el envío? 
# b) Formalicen el planteo del problema (Definan población, variable, parámetro de interés, objetivo).
# c) Analicen los datos e informe sus conclusiones. Indique si estas son preliminares o definitivas.
# d) Completen el informe con al menos dos gráficos y el cálculo e interpretación de dos medidas de localización y dos de dispersión.
# e) ¿Qué otra información sería útil recabar de cada placa del lote? ¿Con qué objetivo? Comenten brevemente.

# Lectura del archivo
data2 = read.delim("./ejercicio2.csv", header=TRUE, sep=",")

# renombro para facilidad del acceso
colnames(data2)[1] = "nro_placa"
colnames(data2)[2] = "nro_defectos"


### Ejercicio a

promedio_defectos = mean(data2$nro_defectos)

if (promedio_defectos > 1.2) {
  print("El lote no cumple con la exigencia del cliente")
} else {
  print("El lote cumple con la exigencia del cliente")
}


### Ejercicio b

# La poblacion para este problema consiste en todas las placas de madera que distribuye dicha empresa.
# Las placas son de un tamaño especifico utilizadas en la industria de la construcción.

# La variable de estudio es la cantidad de defectos (provocados por rayuduras, poros, etc.) que posee cada placa del lote.
# Es una variable cuantitativa discreta de razon, ya que el '0' indica la ausencia de defectos.

# El parametro de interes en este caso es el promedio de defectos detectados en las placas en lotes de 80 placas.

# El objetivo del problema es determinar si un lote de 80 placas de madera es adecuado para ser enviado a un cliente 
# que exige que la cantidad promedio de defectos por placa en el lote sea menor a 1.2 unidades.


### Ejercicio c
grp = as.data.frame(data2 %>%
                      count(`nro_defectos`))

print(grp)

# Este dataframe indica que si bien muchas de las tablas tiene solo un error
# Muchas otras tienen dos o mas errores,lo que desbalancea el promedio general 
# tendiendo a un valor mayor a 1,2 unidades.

# Por lo que este lote de 80 placas especifico no cumple con el requerimiento del cliente. 
# Este analisis es particular para dicho lote. No significa que todos los lotes distribuidos
# posean dicha cantidad promedio de defectos. Por lo tanto es una analisis preliminar.


### Ejercicio d

## Calculo y grafico de medida de localizacion: La moda
mode <- as.numeric(grp[which.max(grp$n), "nro_defectos"])

colores <- ifelse(seq_along(grp$nro_defectos) == mode+1, "#B1AFFF" , "#AAE3E2")

barplot(height=grp$n,
        names.arg = grp$nro_defectos,
        col=colores,
        main='Ocurrencia de defectos',
        ylab='numero de ocurrencias',
        xlab='numero de defectos',
        ylim=c(0,30),
)
legend("topright", "Moda", fill = "#B1AFFF")

## Calculo y grafico de medidas de localizacion: Boxplot
boxplot(data2$nro_defectos,
        col = "#FFAACF",
        border = "#7286D3",
        ylab="Numero de Defectos")

# Notar la mediana es 2, y que a pesar de que 1 es la moda, solo el 25% de las tablas tiene hasta solo un defecto.

## Calculo y grafico de medida de dispersion: Desviacion Estandar
deviacion_standar = sd(data2$nro_defectos)

data2 %>%
  ggplot( aes(x=`nro_defectos`)) +
  geom_density(fill="#11a3a2",
               color="black",
               alpha=0.5) +
  geom_vline(xintercept=promedio_defectos, size=0.7, color="#FF9494") +
  geom_text(aes(x= promedio_defectos, label=paste0("Promedio:", mean(data2$nro_defectos)), y=0.6))+
  geom_vline(xintercept=promedio_defectos+deviacion_standar, size=0.5,linetype="dashed" , color="#1572A1")+
  geom_vline(xintercept=promedio_defectos-deviacion_standar , size=0.5, linetype="dashed", color="#1572A1")


## Calculo de medida de dispercion: Coeficiente de variacion

coeficiente_variacion <- deviacion_standar / promedio_defectos * 100
print(paste("coeficiente de variacion: ", coeficiente_variacion))

# Esto valor significa que el conjunto de datos tiene una variabilidad relativamente alta en comparación con su media.
# Ya que el valor de la desviación estándar es aproximadamente el 66.18% del valor de la media.
# Esto indica que hay una gran dispersión de los datos.


### Ejercicio e

# Seria de utilidad que en el dataset se recolectaran el tipo de defectos que posee cada tabla.
# Ya que de desta manera se podria estudiar cuales son los tipos de fallas mas usuales y con que frecuencia ocurren.
# Poder medir el tipo de defecto podria ayudar a que el fabricante pueda enforcarse en evitar los
# defectos mas usuales o mas preocupantes para mejorar la calidad de sus productos.


###############
# Actividad 3 #
###############

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
# disponible en ese momento en el deposito) y midieron el contenido de vitamina D.

# ¿Considera que los complementos vitamínicos cumplen con las especificaciones 
# en relación al contenido de vitamina D?  ¿Qué medidas (parámetros) serían de 
# interés en este caso?

# A) Plantee el problema, defina población, variable y parámetro/s de interés.
# B) Plantee un objetivo en términos de dicho/s parámetro/s.
# C) Analice exhaustivamente los datos (incluya dos gráficos). Responda al 
# objetivo planteado. Indique si sus conclusiones son preliminares o definitivas. 

# Importamos el dataset en una variable para poder trabajarlo dentro de R
df = read.csv2("ejercicio3.csv")

# Antes de comenzar a resolver las actividades vamos a considerar que las
# preguntas que se realizaron entre el enunciado que explica la situación del
# laboratorio y los ejercicios A, B y C se van a responder dentro de los
# ejercicios asignados. Decidimos esta medida para evitar redundancia de datos.

## Ejercicio A
# La población está compuesta por todas las pildoras contenedoras de Vitamina D
# que realice este laboratorio aunque la muestra solo consta de 150 pildoras.

# La variable es la cantidad de Vitamina D (en mg) que contiene cada pildora.
# Esta variable es de tipo cuantitativa continua ya que mide una canitdad numerica real.

# El parametro de interes es el porcentaje de pildoras que superan la cantidad
# recomendada de vitamina D.


## Ejercicio B
# El objetivo es observar si el porcentaje de pildoras que superen la cantidad
# de Vitamina D recomendada es suficiente como para asumir que la pildora es
# "confiable". Si bien el enunciado no nos brinda un porcentaje a superar nosotros
# vamos a asumir que a partir de 80% es un porcentaje aceptable para poder
# confiar en estas pildoras.


## Ejercicio C
# Asignamos un valor de mayor o menor en relacion al contenido de vitamina D.
df$resumen_contenido <- ifelse(df$Cant.Vitamina.D >= 20, "Mayor", "Menor")

# Creamos un nuevo DF en donde tenemos los valores de frecuencia y porcentaje
# Correspondientes a la relacion del contenido de vitamina D en cada pildora.
df_porc <- as.data.frame(table(df$resumen_contenido))
colnames(df_porc) <- c("Cantidad", "Frec")
df_porc$Porc <- round(df_porc$Frec/sum(df_porc$Frec) * 100, 2)

# Realizamos un gráfico de torta para ver los resultados del mismo.
torta <- pie(df_porc$Porc,
             df_porc$Cantidad,
             col = c("#aee637", "#ef3353"),
             main = "Relación del contenido de vitamina D sobre lo recomendado entre todas las pildoras")

# Ahora pasaremos a ver si es que el porcentaje de pildoras que superan el contenido recomendado por día
# es el suficiente como para confiar en las pildoras distribuidas por el laboratorio.
barplot_df <- barplot(df_porc$Porc,
                      ylab = "Porcentaje",
                      xlab = "Relación",
                      main = "Relación del contenido de vitamina D sobre lo recomendado entre todas las pildoras",
                      col = c("#aee637", "#ef3353"), ylim = c(0, 100))
axis(1, at = barplot_df, labels=df_porc$Cantidad, cex.axis=0.9)
text(barplot_df, df_porc$Porc + 3, labels = paste(df_porc$Porc, "%"))

# Trazamos una linea recta que determinara si el porcentaje de pildoras que cumplen con al menos el 
# contenido recomendado son lo suficiente para asumir la confiabilidad de las pildoras del laboratorio
abline(h = 80, lty = 2, col = "blue")
text(83, "Porcentaje de confiabilidad ", col = "blue", font = 2)

# Una vez realizado los gráficos podemos dar como finalizado este problema y podemos
# concluir de manera definitiva que las pildoras no son confiables, si bien la mayoría
# cumple con lo que se estipula no es suficiente para poder confiar en que estas pildoras
# puedan proveer el contenido de vitamina D que se necesita por día. 
# Este es un estudio preliminar, ya que solo estamos trabajando con una muestra, por lo tanto
# no podemos concluir que todas las pildoras de la poblacon tengan estas caracteristicas.





