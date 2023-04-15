###############
# Ejercicio 1 #
###############
#Población: Estudiantes de Probabilidad y Estadística de la Comisión 2
### Objetivos: ###
# 1) Porcentaje de la cantidad de formación académica alcanzada
# 2) Cuántos alumnos tienen Álgebra y Cálculo aprobadas?
# 3) Cuál es el promedio del colesterol en sangre de los alumnos que nacieron en Marzo?
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
###########################
# Instalación de paquetes #
###########################
install.packages("qcc")
install.packages("ggplot2")
library("qcc")
library("ggplot2")

#Cargar archivo csv en una tabla
encuesta = read.delim("./ejercicio1.csv", header = TRUE, sep=",")
#Renombrar columnas
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

#Reemplazar la coma por punto. Ejemplo: 4,5 => 4.5
encuesta$colesterol_sangre = scan(text = encuesta$colesterol_sangre, dec=",", sep=".")

#Respuesta 1:
encuesta_frame = as.data.frame(table(encuesta$form_academica))
encuesta_frame$porc = round(encuesta_frame$Freq/sum(encuesta_frame$Freq) * 100, 2)
encuesta_barplot = barplot(encuesta_frame$porc, ylab="Porcentaje", main = "Porcentaje de la formación académica de los alumnos", xlab="Tipo de formación", col = c("#98D8AA", "#10A19D", "#F7D060", "#FF6D60", "#D82148"), ylim = c(0, 50))
axis(1, at = encuesta_barplot, labels=encuesta_frame$Var1, cex.axis=0.9)
text(encuesta_barplot, encuesta_frame$porc + 1, labels = paste(encuesta_frame$porc, "%"))

#Respuesta 2:
#Aplicamos un criterio de inclusión en el cual las respuestas que sean distintas de "Si" o "No" 
#no se tienen en cuenta (Trabajamos con la muestra y no con la población)
encuesta_filtrada = subset(encuesta, encuesta$alg_calc_aprobadas == "Sí" | encuesta$alg_calc_aprobadas == "No")
encuesta_frame2 = as.data.frame(table(encuesta_filtrada$alg_calc_aprobadas))
encuesta_pie = pie(encuesta_frame2$Freq, labels = paste(c("Desaprobado: ", "Aprobado: "), encuesta_frame2$Freq), main = "Cantidad de personas que aprobaron algebra y calculo", col = c("#FF6D60", "#609966"))

#Respuesta 3
encuesta_colesterol = encuesta[, c("res_nro", "colesterol_sangre")]
encuesta_colesterol$promedio = mean(encuesta_colesterol$colesterol_sangre)
plot(encuesta_colesterol$res_nro, encuesta_colesterol$colesterol_sangre, type="l", lwd=2, col="#009EFF", main = "Colesterol en sangre", xlab = "Nro Respuesta", ylab="Colesterol en sangre")
lines(encuesta_colesterol$res_nro, encuesta_colesterol$promedio, lwd=2, col="#D61355")
legend("topleft", legend = c("Promedio", "Colesterol"), lwd=c(2, 2), col=c("#009EFF", "#D61355"))

#Recategorización Respuesta 3
encuesta_recat = as.data.frame(encuesta)
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre > 70] = "Alto"
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre > 65 & encuesta_recat$colesterol_sangre <= 70] = "Normal"
encuesta_recat$colesterol_sangre[encuesta_recat$colesterol_sangre <= 65] = "Bajo"
