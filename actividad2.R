# ACTIVIDAD 2 
# Reconsidere el problema 2, Material 1 (pág. 6)
#“En una empresa distribuidora de placas de madera de gran tamaño de uso para la
# industria de la construcción, se preparan lotes de 80 unidades para sus clientes. 
# La empresa tiene un sistema láser que detecta defectos de cada placa y lleva 
# un registro del número de defectos de todas las placas de cada lote. 

# Uno de los clientes es estricto en relación al número total de defectos (poros, rayaduras, etc.)
# presentes en la superficie de las placas y exige que el número promedio de defectos
# por placa en cada lote sea menor a 1,2 unidades. Al momento de recibir el pedido
# por parte de este cliente, en la distribuidora cuentan con un lote ya preparado 
# y desean saber si pueden enviárselo o no” 

# a) ¿Son aptas las maderas para el envío? 
# b) Formalicen el planteo del problema (Definan población, variable, parámetro de interés, objetivo).
# c) Analicen los datos e informe sus conclusiones. Indique si estas son preliminares o definitivas.
# d) Completen el informe con al menos dos gráficos y el cálculo e interpretación de dos medidas de localización y dos de dispersión.
# e) ¿Qué otra información sería útil recabar de cada placa del lote? ¿Con qué objetivo? Comenten brevemente.


########## Importacion del dataset ##########

# En la ventana Environment>import Dataset>From Excel selecciona la ruta al archivo de excel, especificando la hoja 2 
Trabajo_práctico1_datos = read.delim("./ejercicio2.csv", header=TRUE, sep=",")

install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)

# renombro para facilidad del acceso

data2 <- Trabajo_práctico1_datos

######### Ejercicio a ##########

promedio_defectos = mean(data2$`N° de defectos`)

if (promedio_defectos > 1.2) {
  print("El lote no cumple con la exigencia del cliente")
} else {
  print("El lote cumple con la exigencia del cliente")
}


######### Ejercicio b ##########

# La poblacion para este problema consiste en todas las placas de madera que distribuye dicha empresa.
# Las placas son de un tamaño especifico utilizadas en la industria de la construcción.

# La variable de estudio es la cantidad de defectos (provocados por rayuduras, poros, etc.) que posee cada placa del lote.
# Es una variable cuantitativa discreta de razon, ya que el '0' indica la ausencia de defectos.

# El parametro de interes en este caso es el promedio de defectos detectados en las placas en lotes de 80 placas.

# El objetivo del problema es determinar si un lote de 80 placas de madera es adecuado para ser enviado a un cliente 
# que exige que la cantidad promedio de defectos por placa en el lote sea menor a 1.2 unidades.


######### Ejercicio c ##########

grp = as.data.frame(data2 %>%
                    count(`N° de defectos`))

print(grp)

# Este dataframe indica que si bien muchas de las tablas tiene solo un error
# Muchas otras tienen dos o mas errores,lo que desbalancea el promedio general 
# tendiendo a un valor mayor a 1,2 unidades.

# Por lo que este lote de 80 placas especifico no cumple con el requerimiento del cliente. 
# Este analisis es particular para dicho lote. No significa que todos los lotes distribuidos
# posean dicha cantidad promedio de defectos. Por lo tanto es una analisis preliminar.


######### Ejercicio d ##########

## Calculo y grafico de medida de localizacion: La moda

mode <- as.numeric(grp[which.max(grp$n), "N° de defectos"])

colores <- ifelse(seq_along(grp$`N° de defectos`) == mode+1, "#B1AFFF" , "#AAE3E2")

barplot(height=grp$n,
        names.arg = grp$`N° de defectos`,
        col=colores,
        main='Ocurrencia de defectos',
        ylab='numero de ocurrencias',
        xlab='numero de defectos',
        ylim=c(0,30),
)
legend("topright", "Moda", fill = "#B1AFFF")


## Calculo y grafico de medidas de localizacion: Boxplot

boxplot(data2$`N° de defectos`,
        col = "#FFAACF",
        border = "#7286D3",
        ylab="Numero de Defectos")

# Notar la mediana es 2, y que a pesar de que 1 es la moda, solo el 25% de las tablas tiene hasta solo un defecto.


## Calculo y grafico de medida de dispersion: Desviacion Estandar

deviacion_standar = sd(data2$`N° de defectos`)

data2 %>%
  ggplot( aes(x=`N° de defectos`)) +
  geom_density(fill="#11a3a2",
               color="black",
               alpha=0.5) +
  geom_vline(xintercept=promedio_defectos, size=0.7, color="#FF9494") +
  geom_text(aes(x= promedio_defectos, label=paste0("Promedio:", mean(data2$`N° de defectos`)), y=0.6))+
  geom_vline(xintercept=promedio_defectos+deviacion_standar, size=0.5,linetype="dashed" , color="#1572A1")+
  geom_vline(xintercept=promedio_defectos-deviacion_standar , size=0.5, linetype="dashed", color="#1572A1")


## Calculo de medida de dispercion: Coeficiente de variacion

coeficiente_variacion <- deviacion_standar / promedio_defectos_defectos * 100
print(paste("coeficiente de variacion: ", coeficiente_variacion))

# Esto valor significa que el conjunto de datos tiene una variabilidad relativamente alta en comparación con su media.
# Ya que el valor de la desviación estándar es aproximadamente el 66.18% del valor de la media.
# Esto indica que hay una gran dispersión de los datos.


######### Ejercicio e ##########

# Seria de utilidad que en el dataset se recolectaran el tipo de defectos que posee cada tabla.
# Ya que de desta manera se podria estudiar cuales son los tipos de fallas mas usuales y con que frecuencia ocurren.
# Poder medir el tipo de defecto podria ayudar a que el fabricante pueda enforcarse en evitar los
# defectos mas usuales o mas preocupantes para mejorar la calidad de sus productos.


