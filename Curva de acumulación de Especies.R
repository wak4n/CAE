#############################################################
###                                                       ###
###   CURVA DE ACUMULACIÓN DE ESPECIES                    ###
###   Script elaborado por @wak4n                         ###
###   Dudas o comentarios: wak4n@riseup.net               ###
###                                                       ###
#############################################################


#Para realizar este ejercicio necesitamos 3 archivos:

# 1.- accumulation.csv
# 2.- curva_estimates.txt
# 3.- curva_e_res.csv

# Todos ellos guardados en el directorio donde vamos a trabajar para que al llamarlos
# no existan errores


#######################
###     MÉTODO 1    ###
#######################


#Se instala la librería VEGAN la cual contiene la funcion SPECACCUM

install.packages("vegan")
library(vegan)

#Se establece el directorio de trabajo y se enlistan los archivos
#Esto varía según tu ruta y directorio de trabajo.


setwd("D:/02-BIBLIOTECA/Ciencia/Estadística/Scripts R/CAE")
list.files()

#Se lee el archivo que contiene la base de datos en formato .CSV
#en este ejemplo nombramos al archivo "accumulation" y si lo visualizamos
#podemos observar que el nombre de las especies se encuentra en las columnas
#mientras que las unidades de muestreo en las filas.
#Nota: en este ejercicio tenemos datos de abundancia por lo que los números
#representan el número de individuos de cada especie encontrados en cada unidad de muestreo
#que para este caso son meses, pero podrían ser parcelas, sitios, etc.


curve <- read.csv("accumulation.csv")

#Posteriormente convertimos la tabla en un objeto llamado "curve1" y le decimos a la  función
#specaccum que nos arroje dos desviaciones, es decir las barras (ci=2) y haga 50 permutaciones.

curve1 = specaccum(curve, ci = 2, permutations = 50)

#Ahora graficaremos el resultado del paso anterior y colocamos en el eje X la palabra "Months", 
#en el eje y= la palabra #Species number" y el título del gráfico "Species Accumulation Curve".

plot(curve1, xlab="Months", ylab = "Species number", main = "Species Accumulation Curve")

###########

# Opcional: este paso solo se puede realizar después de realizar el MÉTODO 2#
#Consiste en mostrar los puntos observados en color negro.

points(muestras, riqueza, pch=19, bg="black")




####################################################################################
###                                                                              ###
###   MÉTODO 2 UTILIZANDO ESTIMATES PARA CALCULAR EL ESTIMADO DE RIQUEZA CHAO1   ###
###                                                                              ###
####################################################################################



# Para realizar este ejercicio necesitamos analizar nuestra información en ESTIMATES
# http://viceroy.eeb.uconn.edu/estimates/EstimateSPages/EstimateSRegistration.htm

# para ingresar los datos a ESTIMATES construimos un archivo que nombramos:
# curva_estimates.txt , si lo abrimos con  Excel la celda A1 tiene el título curve
# la celda A2 contiene el núméro de especies, la celda B2 contiene las unidades de muestreo
# en nuestro caso son viajes a campo; y finalmente los datos que comienzan en A3 son el número
# de individuos de una especie (columnas) que se encontraron en cada viaje a campo (filas)


#  CURVE
#   40  11
#   0   0   0   0   0   0   0   0   0 ...... 40
#   0   3   0   0   0   0   0   0   0 ......
#   0   2   0   2   0   0   0   0   0 ......
#   .
#   .
#   .
#   11

# De este modo nuestro archivo estará en formato 2 y procedemos al menú Diversity/diversity settings
# y pedimos Computar. Copiamos los resultados en un excel y los guardamos como: 
# "curva_e_res.csv" 



list.files()

curva <- read.csv("curva_e_res.csv")

muestras=unlist(curva[1])
riqueza=unlist(curva[3])
error=unlist(curva[6])


estimador=unlist(curva[20])
error.estimado=qt(1 - 0.025,df=10)*curva[23]

y.minimo=min(riqueza)
y.maximo=max(riqueza)
y.bajo=y.minimo*0.80
y.alto=estimador+error.estimado
limites.y=c(y.bajo, max(y.alto))

plot(riqueza~muestras, type="l", ylim= c(0,70),
    las=1, xlab="Months",
    ylab="Species number", main = "Species Accumulation Curve", xlim=c(0,12))

points(muestras, riqueza, pch=19, bg="black")

guerreros=unlist(riqueza + error.estimado)
diablos=unlist(riqueza - error.estimado)
arrows(muestras, guerreros, muestras, diablos, angle=90,
       code=3,length = 0.05, col = "black")

# Construir curva

points(muestras, unlist(estimador), pch=19, bg="red", col="red")
guerreros=unlist(estimador + error.estimado)
diablos=unlist(estimador - error.estimado)
arrows(muestras, guerreros, muestras, diablos, angle=90,
       code=3, length=0.05, col="red")

legend(x=10, y=15, legend=c("Observed", "Estimated"),
       pch=19, col=c("black", "red"), bty="n")




##############################################################################
#                                                                            #
#  MÉTODO 2 UTILIZANDO ESTIMATES PARA CALCULAR EL ESTIMADO DE RIQUEZA CHAO2  #
#                                                                            #
##############################################################################



list.files()

curva2 <- read.csv("curva_e_res.csv")

muestras2=unlist(curva2[1])
riqueza2=unlist(curva2[3])
error2=unlist(curva2[6])


estimador2=unlist(curva2[24])
error.estimado2=qt(1 - 0.025,df=15)*curva2[27]

y.minimo=min(riqueza2)
y.maximo=max(riqueza2)
y.bajo=y.minimo*0.80
y.alto=estimador2+error.estimado2
limites.y=c(y.bajo, max(y.alto))

plot(riqueza2~muestras2, type="l", ylim= c(0,70),
     las=1, xlab="Months",
     ylab="Species number", main = "Species Accumulation Curve", xlim=c(0,18))

points(muestras2, riqueza2, pch=19, bg="black")

guerreros2=unlist(riqueza2 + error.estimado2)
diablos2=unlist(riqueza2 - error.estimado2)
arrows(muestras2, guerreros2, muestras2, diablos2, angle=90,
       code=3,length = 0.05, col = "black")

# Construir curva



points(muestras2, unlist(estimador2), pch=19, bg="red", col="339999")
guerreros2=unlist(estimador2 + error.estimado2)
diablos2=unlist(estimador2 - error.estimado2)
arrows(muestras2, guerreros2, muestras2, diablos2, angle=90,
       code=3, length=0.05, col="339999")

legend(x=15, y=15, legend=c("Observed", "Estimated"),
       pch=19, col=c("black", "339999"), bty="n")



#################
###           ###
###   FIN     ###
###           ###
#################