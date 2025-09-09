# 2.6 Ejercicio 1, version R
datos <- read.csv("Rendimiento_de_gasolina.csv")
datos

# ******************************************************************************
# A) Ajustar un modelo de regresión lineal múltiple que relacione el rendimiento
# de la gasolina y, en millas por galón, la cilindrada del motor (x1) y la 
# cantidad de gargantas del carburador (x6).
# ******************************************************************************
library(tibble) 
data_intro <- tibble(x1=datos$x1, x6=datos$x6, y=datos$y)
m1 <- lm(y ~ x1 + x6, data = data_intro)
print(m1)
#
#Call:
#  lm(formula = y ~ x1 + x6, data = data_intro)
#
#Coefficients:
#  (Intercept)           x1           x6  
#32.88455     -0.05315      0.95922  

# RESPUESTA:
# y = 32.88 - 0.05 * x1 + 0.95 * x6

# ******************************************************************************
# B) Formar la tabla de análisis de varianza y probar la significancia de la 
# regresión.
# ******************************************************************************

print(summary(m1))
# ...
# ...
# ...
#Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
#F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

# RESPUESTA:
# Como p-value es cercano a 0, la hipotesis de que los coeficientes son nulos, 
# es altamente improbable. El decir, el modelo obtenido hace pronostico cercanos
# a los reales. 

# ******************************************************************************
# C) Calcular R2 y R2adj para este modelo. Compararlas con las R2 y R2adj 
# Ajustado para el modelo de regresión lineal simple, que relaciona las millas 
# con la cilindrada (x1).
# ******************************************************************************

print(summary(m1))
# ...
#Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
#F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

m2 <- lm(y ~ x1, data = data_intro)
print(m2)
#
#Call:
#  lm(formula = y ~ x1, data = data_intro)
#
#Coefficients:
#  (Intercept)           x1  
#33.72268     -0.04736

print(summary(m2))
# ...
#Multiple R-squared:  0.7723,	Adjusted R-squared:  0.7647 
#F-statistic: 101.7 on 1 and 30 DF,  p-value: 3.743e-11

# RESPUESTA:
# El modelo m1 con x1 y x6 tiene un valor R2_adj=0.77 mientras que el m2 
# que tiene solamente a x1, tiene R2_adj=0.76. Añadir x6 mejora ligeramente 
# las predicciones. 

# ******************************************************************************
# D) Determinar un intervalo de confianza para β1.
# ******************************************************************************

conf_int_all <- confint(m1, level = 0.95)
print(conf_int_all)
#
#2.5 %      97.5 %
#  (Intercept) 29.74428901 36.02481266
#x1          -0.06569892 -0.04059641
#x6          -0.41164739  2.33009349

# RESPUESTA:
# B1 estará en el rango [-0.065, -0.040]

# ******************************************************************************
# E) Determinar un intervalo de confianza de 95% para el rendimiento promedio 
# de la gasolina, cuando x1=225pulg3 y x6=2 gargantas.
# ******************************************************************************

nuevos_datos <- data.frame(x1 = 225, x6 = 2)
# Intervalo de CONFIANZA para el RENDIMIENTO PROMEDIO (Media)
ic_media <- predict(m1, newdata = nuevos_datos, interval = "confidence", level = 0.95)
print("Intervalo de Confianza del 95% para el RENDIMIENTO PROMEDIO:")
print(ic_media)
#
#fit      lwr      upr
#1 22.84477 21.57653 24.11302

# RESPUESTA
# El rendimiento promedio estará entre [21.57,24.11]

# ******************************************************************************
# F) Determinar un intervalo de predicción de 95% para una nueva observación 
# de rendimiento de gasolina, cuando x1=225pulg3 y x6=2 gargantas.
# ******************************************************************************

# Intervalo de PREDICCIÓN para una NUEVA OBSERVACIÓN
ip_prediccion <- predict(m1, newdata = nuevos_datos, interval = "prediction", level = 0.95)
print("Intervalo de Predicción del 95% para una NUEVA OBSERVACIÓN:")
print(ip_prediccion)
#fit      lwr      upr
#1 22.84477 16.55371 29.13583

# RESPUESTA
# Estará entre [16.55,29.13]. Es mas grande que en el caso anterior, porque 
# se incluye el error aleatorio de un caso específico, no de la media (como
# en el caso anterior). 

# ******************************************************************************
# G) Considerar el modelo de regresión lineal simple, que relaciona las 
# millas con la cilindrada. Construir un intervalo de confianza de 95% para el 
# rendimiento promedio de la gasolina y un intervalo de predicción para el 
# rendimiento, cuando x1=225pulg3. Comparar las longitudes de estos intervalos 
# con los intervalos obtenidos en los dos incisos anteriores. 
# ¿Tiene ventajas agregar x6 al modelo?
# ******************************************************************************

nuevos_datos2 <- data.frame(x1 = 225)
# Intervalo de CONFIANZA para el RENDIMIENTO PROMEDIO (Media)
ic_media <- predict(m2, newdata = nuevos_datos, interval = "confidence", level = 0.95)
print("Intervalo de Confianza del 95% para el RENDIMIENTO PROMEDIO:")
print(ic_media)
#fit      lwr      upr
#1 23.06677 21.81939 24.31415

# RESPUESTA
# El rendimiento promedio de m1 era [21.57,24.11], mientras que ahora para
# m2 es [21.81,24.31]. No veo mucha ventaja de agregar x6 al modelo.  

# ******************************************************************************
# H) Trazar una gráfica de probabilidad normal de los residuales. ¿Parece haber 
# algún problema con la hipótesis de normalidad?
# ******************************************************************************

plot(m1, which=1)
# La grafica se guardo en ej_2_grafica_Residuals_vs_Fitted_en_R.png. Muestra 
# los puntos distribuidos en forma de U (la linea roja), lo cual denota que
# la relación no es exactamente lineal. Las predicciones se alejan de las reales
# para valores menores de 15 y mayores que 25. 

# ******************************************************************************
# I) Trazar e interpretar una gráfica de los residuales en función de la 
# respuesta predicha.
# ******************************************************************************

plot(m1, which = 2)
# La grafica se guardo en ej_2_grafica_Q-Q_Residuales_en R.png. Muestra los puntos
# cercanos a linea de 45 grados en el centro. A la izquierda y a la derecha se
# alejan un poco de la línea de 45 grados.

# ******************************************************************************
# J) Trazar las gráficas de los residuales en función de cada una de las 
# variables regresoras. ¿Implican esas gráficas que se especificó en forma 
# correcta el regresor?
# ******************************************************************************

plot(data_intro$x1, residuals(m1), 
     main = "Residuales vs x1",
     xlab = "Variable x1", ylab = "Residuales",
     pch = 19, col = "blue")
# La grafica se guarda en ej_2_grafica_Residuales_vs_x1_en_R.png

plot(data_intro$x6, residuals(m1), 
     main = "Residuales vs x6",
     xlab = "Variable x6", ylab = "Residuales",
     pch = 19, col = "blue")
# La grafica se guarda en ej_2_grafica_Residuales_vs_x6_en_R.png

# RESPUESTA
# Mientras que la grafica de x1 parece "normal", la grafica de x6 muestra que
# hay poco variabilidad para x6=1 garganta, mientras que para x6=2 es muy grande
# y para x6=4 es grande. Me parece que faltan regresores para mejorar las 
# predicciones, quizás x4 (Relación de compresión), x10 (peso), x11 (tipo
# transmisión).
