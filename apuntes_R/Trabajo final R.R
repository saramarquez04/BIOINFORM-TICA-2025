# SaraMarquezHernandez_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
library(tidyverse)

datos = read.csv("datos_biomed.csv", header = TRUE)#así leemos los archivos en R, "header=true" sirve para que nos muestre los nombres de las columnas
datos

#con estos códigos nos aseguramos que cada uno de nuestros datos tengan el formato correcto 
datos$Glucosa = as.numeric(datos$Glucosa)
datos$Presion = as.numeric(datos$Presion)
datos$Colesterol = as.numeric(datos$Colesterol)
datos$Tratamiento = as.factor(datos$Tratamiento)

datos$Glucosa
datos$Presion
datos$Colesterol
datos$Tratamiento

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

#el comando head sirve para cuando tenemos una tabla muy grande y no la queremos cargar entera, ejecutamos esta comando y nos saca las 5-6 primeras filas para ver que se ha cargado bien el archivo
head(datos)

#el comando summary nos da valores como mediana, media, primer cuartil...
summary(datos)

#el comando dim nos proporciona las dimensiones de nuestra tabla (en este caso 100 filas y 5 columnas)
dim(datos)

#el comando str nos da un analisis de variables de las columnas que tenemos
str(datos)

#Tenemos 5 variables y 3 tratamientos:
#para calcular el número de variables ponemos el comando:
num_variables = ncol(datos)
num_variables
cat("Número de variables:", num_variables, "\n")


#para calcular el número de tratamiento ponemos el comando:
num_tratamientos = length(unique(datos$Tratamiento))
num_tratamientos
cat("Número de tratamientos:", num_tratamientos, "\n")

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

#para crear el boxplot he utilizado la libreria ggplot que es más moderna y estética, para ello cargamos la librería:

library (ggplot2)

#luego utilizamos este comando proporcionado en los apuntes:
# ggplot(nombre de nuestra tabla, en nuestro caso "datos" + "aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)" que es el contenido del boxplot y la leyenda )
#con theme_minimal finalizamos el grafico
#para cambiar el color de los boxplot uso el comando "scale_fill_manual"

ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_boxplot() +  
  theme_minimal() +
  labs(title = "Boxplots de Glucosa por Tratamiento") +
   scale_fill_manual(values = c ("Placebo" = "sandybrown", 
                               "FarmacoA" = "darkorange", 
                               "FarmacoB" = "orange"))


# 4. Realiza un violin plot (investiga qué es). (1 pt)

#un violin plot es una visualización que combina un diagrama de caja con un grafico de densidad de probabilidad, mostrando la distribucion de un conjunto de datos en forma de violin simetrico, permite ver las estadisticas resumidas, los cuartiles y la forma de distribucion de los datos (donde se concentran la mayoria de valores)
#con el comando "geom_violin" dibujamos la geometría del violín y con "trim = FALSE" le decimos a R que no corte las colas al manejar los extremos de distribución, es decir que muestre la densidad completa, ya que si trim = TRUE las colas del violin se cortarían en el mínimo y máximo de los datos
#con el comando "geom_boxplot(width = 0.1, fill = "white") +" le decimos a R que superponga el boxplot en el centro del violin plot y lo coloree de blanco
# con labs añadimos el título y con scale_fill_manual como he dicho antes añadimos los colores 

ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() + 
  labs(title = "Violin plot de Glucosa por Tratamiento")+
  scale_fill_manual(values = c ("Placebo" = "sandybrown", 
                               "FarmacoA" = "darkorange", 
                               "FarmacoB" = "orange"))



# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

#para realizar el grafico de dispersion usamos el siguiente comando:
#para que el gráfico de dispersión tenga los colores que he estado usando anteriormente uso la variable "mis_colores" para almacenar los colores que he elegido y con "c" los combino todos en un vector
mis_colores = c("sandybrown", "darkorange", "orange")

#definimos glucosa en el eje x y presión en el eje y y con "col"asignamos los colores elegidos basandose en los tratamientos
#con "pch=19" definimos el símbolo usado en los puntos

plot(datos$Glucosa, datos$Presion,
     col = mis_colores[as.factor(datos$Tratamiento)], pch = 19,
     xlab = "Glucosa", ylab = "Presión",
     main = "Dispersión Glucosa vs Presión")

#para añadir la leyenda en la parte inferior derecha usando el siguiente comando
legend("bottomright", legend = levels(datos$Tratamiento),
       col = mis_colores,
       pch = 19, title = "Tratamiento")


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

#un facet grid es una técnica de visualziación en ggplot 2 que permite dividir un gráfico en múltiples subgráficos más pequeños basandose en los niveles de una o en nuestro caso, dos, variables, en nuestro caso queremos mostrar la relación entre colesterol y presión pero separando esa relación en un panel indvidual (es decir, 3 paneles) para cada tipo de tratamiento
#para mostrar la relación colesterol vs presión que se indica en el enunciado utilizamos el comando " aes(x = Colesterol, y = Presion))" 
# con la función "geom_point" definimos el gráfico de dispersión (capa de puntos)y con "aes(color = Tratamiento" indicamos que el color de cada punto se elige en función al tratamiento

ggplot(datos, aes(x = Colesterol, y = Presion)) +
  geom_point(aes(color = Tratamiento)) +
  facet_grid(. ~ Tratamiento) +
  scale_color_manual(values = c ("Placebo" = "sandybrown", 
                               "FarmacoA" = "darkorange", 
                               "FarmacoB" = "orange"))+
  theme_minimal() +
  labs(title = "Facet Grid: Colesterol vs Presión por Tratamiento")

# 7. Realiza un histogramas para cada variable. (0.5 pts)

#para realizar un histograma usamos los siguientes comandos:
#he cambiado los colores porque aquí ya no utilizamos los diferentes tratamientos como referencia, sino que usamos las variables glucosa, presión y colesterol para cada histograma por lo que he querido cambiar los colores

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

hist(datos$Glucosa, main = "Histograma Glucosa", col = "lightslategrey")
hist(datos$Presion, main = "Histograma Presión", col = "blue")
hist(datos$Colesterol, main = "Histograma Colesterol", col = "dodgerblue")

par(mfrow = c(1,1))



# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)

#usando la función factor le estamos diciendo a R que el tratamiento es una variable cualitativa y con "str" nos aseguramos que los datos se hayan almacenado correctamente
#por eso en la consola nos sale "factor con 3 niveles:2,3,2,2,1... ya que R no ha asignado el texto placebo, farmaco A... para el analisis sino que ha asignado un número a cada tratamiento y los ha almacenado
factor_tratamiento = factor(datos$Tratamiento)
str(factor_tratamiento)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

#para obtener la media utilizamos este comando y en la consola nos sale una tabla con el valor promedio de glucosa para cada tratamiento:
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)

#para obtener la desviación estándar utilizamos este comando y en la consola obtenemos el valor de dispersión de la glucosa para cada grupo en tratamiento:
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)


# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

#uso el comando head() para confirmar que solo tengo los datos del tratamiento que quiero
#con el comando dim obtengo las dimensiones de cada tabla, es decir cuantos pacientes hay con cada tratamiento 

lista_tratamientos = split(datos, datos$Tratamiento)

#para obtener los datos de los pacientes con placebo (33 pacientes):
placebo = lista_tratamientos[["Placebo"]]
head(placebo)
dim(placebo)

#para obtener la lista de pacientes con el farmacoA (36 pacientes)
FarmacoA = lista_tratamientos[["FarmacoA"]]
head(FarmacoA)
dim(FarmacoA)

#para obtener la lista de pacientes con fármacoB (31 pacientes)
FarmacoB = lista_tratamientos[["FarmacoB"]]
head(FarmacoB)
dim(FarmacoB)



# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

#primero comparamos si los datos siguen una distribución normal:
#para averiguar si nuestros datos siguen una distribución normal usamos el saphiro-wilk test, este nos dará valores de p-values de 0,05 por encima o por debajo y en función de eso decidimos si es normal o no, si la distribución es normal se usan test paramétricos y si no es normal se usan test no paramétricos

library(dplyr)
normalidad_glucosa <- datos %>%
  group_by(Tratamiento) %>%
  summarise(
    Shapiro_W = shapiro.test(Glucosa)$statistic,
    p_valor = shapiro.test(Glucosa)$p.value
  )

# Mostrar el resultado de la prueba de normalidad
print(normalidad_glucosa)

#llegamos a la conclusión de que los 3 datos siguen una distribución normal ya que todos son mayores de 0,05
#como la distribución es normal para realizar la comparativa de medias realizamos un test paramétrico como ANOVA


# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

#realizamos el analisis ANOVA ya que la distribución obtenida en el punto 11 es normal
#con este comando le estamos diciendo al programa que mire la asociación entre glucosa y tramiento

anova_glucosa = aov(Glucosa ~ Tratamiento, data = datos)
summary(anova_glucosa)
#al ejecutar el comando nos sale una tabla en la consola. Debemos fijarnos en la columna Pr(>F) cuyo resultado en este caso es 0.1 (mayor que 0.05) esto quiere decir que no hay evidencia suficiente para concluir que los promedios de glucosa sean diferentes en los 3 grupos (no podemos rechazar la hipótesis nula)

# Deberiamos de ejecutar el comando Post-hoc Tukey en el caso de que nuestro resultado del analisis hubiese sido menor de 0.05 (resultado de anova significativo)
TukeyHSD(anova_glucosa)

