#############################################################################
#
# PRACTICA R
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
#
##############################################################################


# Instalar RCurl

#delante de cada linea de texto poner un comentario despues del hasghat porque si no da error al ejecutar
Tenemos datos de expresión reales de microarray y un listado de genes de ratón, dos condiciones wildtype y knock-out 
3 replicas knock-out y 3 replicas wild-type)
#para ejecutar ctrl +r

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

#hemos instalado un paquete que nos ha permitido obtener datos de internet y con el comando de read table leemos el contenido de los archivos sin almacenarlos en el ordenador

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data)
head(data)
tail(data)
#dimdata= nos dice que tenemos 12488 genes repartidos en 6 columnas
#headdata= 6 primeras filas de nuestra tabla (3 wt + 3 ko)
#taildata= últimas 6 filas de nuestra tabla

# Hacemos un primer histograma para explorar los datos
#colo (color) y main (título)
hist(data, col = "gray", main="GSE5583 - Histogram")

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve? 
#Si hacemos una transformación logarítmica la gráfica cambia de forma y sirve para que la imagen quede más bonita visualmente
#Con la transformación logarítmica, guardas los datos en una variable llamada data2
#data= lee los datos brutos y data 2 = lee los datos transformados

data2 = log2(data)
hist(data2, col = "gray", main="GSE5583 (log2) - Histogram")


# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado? En color azul hemos mostrado los wild-type y en naranja los knock-out. Con el parámetro "main" le asignamos un nombre a la gráfica
# ¿Qué es un boxplot? Un boxplot es un gráfico que representa visualmente la distribución de un conjunto de datos numéricos a través de los cuartiles. Permite identificar la mediana, la dispersión de los datos (rango intercuartílico) y los valores atípicos. Es útil para resumir datos, compararlos y detectar valores extremos de un vistazo. 
boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)

#Nos sale una gráfica menos bonita visualmente si quitamos el 2 del data
boxplot(data, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)

boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)


# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación 
# de los valores de expresión. ¿Es correcta la separación? Sí, porque un cluster tiene las 3 réplicas de knoc-out y el otro cluster tiene las 3 réplicas de wild-type
hc = hclust(as.dist(1-cor(data2)))
plot(hc, main="GSE5583 - Hierarchical Clustering")


#######################################
# Análisis de Expresión Diferencial 
#######################################

head(data)

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
#Al ejecutar esto nos da las columnas de la 1 a la 3 en wt y de la 4 a la 6 en kno y con class nos da el tipo de objeto que tenemos (tabla,lista...)
wt <- data[,1:3]
ko <- data[,4:6]
class(wt)

head(wt)
head(ko)

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean)
ko.mean = apply(ko, 1, mean)
head(wt.mean)
head(ko.mean)

# ¿Cuál es la media más alta?
#La media más alta es 37460.5
limit = max(wt.mean, ko.mean)
limit

# Ahora hacemos un scatter plot (gráfico de dispersión)
#En el eje de las x tenemos los wild type y en el eje de las y tenemos los knok-out. El x-lab es la etiqueta del eje de las x y el ylab la etiqueta del eje de las y. Main es el título
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO",
	main = "GSE5583 - Scatter", xlim = c(0, limit), ylim = c(0, limit))
# Añadir una línea diagonal con abline (nos indica una correlación perfecta entre datos)
abline(0, 1, col = "red")

# ¿Eres capaz de añadirle un grid?
grid()
#abline(a, b): línea de pendiente b y ordenada en el origen a
#abline(h=y): línea horizontal
#abline(v=x): línea vertical
abline(1, 2, col = "red")     # línea y = 2x + 1
abline(h = 2, col = "green")  # línea y = 2
abline(v = 3, col = "violet") # línea x = 3

# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean

# Hacemos un histograma de las diferencias de medias
#El resultado del histograma nos dice que en la diferencia de medias la mayoría se mueve no muy lejos del 0, esdecir, hay pocos genes que tengan expresión diferencial y se muevan lejos del 0
hist(diff.mean, col = "gray")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué? Porque para hacer los analisis necesitamos los datos brutos (sin transformar)
# ¿Cuántas valores tiene cada muestra?
pvalue = NULL 
tstat = NULL 
for(i in 1 : nrow(data)) { #Para cada gen
	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i
	
	# Hacemos el test
	t = t.test(x, y)
	
	# Añadimos el p-value a la lista
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue)

# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10? Que la gráfica sale mejor en comparación a si no lo ponemos
hist(pvalue,col="gray")
hist(-log10(pvalue), col = "gray")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
#abline(v = -diff.mean_cutoff, col = "red", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)
#Ejecutar hasta aqui cuando volvamos a intentar el volcano plot con el p-value
#En el eje de las x tenemos la diferencia de medias

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
#426 genes cumplen los dos criterios, es decir, son diferencialmente expresados de 12488 genes
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)
#Ejecutamos hasta aqui en el siguiente y nos deberia salir una tabla

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")
#Ejecutamos hasta aqui para generar el volcano plot, y nos debería salir una gráfica

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap? Deberian salir los wt juntos y los ko juntos, nos permite clasificar según genes de expresión. En rojo oscuro salen los genes con sobreexpresión 
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)

heatmap(filtered)

#Ejecutamos hasta aqui para generar el primer heatmap. En rojo oscuro salen los genes que tienen sobreexpresión (si estan sobreexpresados en wild type están reprimidos en knockout y al revés)

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
