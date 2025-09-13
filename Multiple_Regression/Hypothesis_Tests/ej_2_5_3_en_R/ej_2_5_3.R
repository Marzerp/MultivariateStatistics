# 2.5.3 Ejercicio 1: Realizar las pruebas de hipótesis sobre la significancia 
# de la regresión y sobre los coeficientes. Encontrar los intervalos de confianza
# respectivos del 95%. Para una tienda con presupuestos: youtube=150, 
# facebook=30, newspaper=20 (en miles de USD): 
# (a) Calcula el intervalo de confianza del 95% para la media de ventas E(sales|X0). 
# (b) Calcula el intervalo de predicción del 95% para una nueva observación de ventas. 
# (c) Comenta la diferencia entre ambos intervalos. 
# Subir respuesta y explicación de sus resultados a github.

library(datarium)
data("marketing")
str(marketing)
marketing

modelo1<-lm(sales~facebook+youtube+newspaper,data=marketing)
print(modelo1)
#Call:
#  lm(formula = sales ~ facebook + youtube + newspaper, data = marketing)
#
#Coefficients:
#  (Intercept)     facebook      youtube    newspaper  
#3.526667     0.188530     0.045765    -0.001037  

# RESPUESTA:
# Se presentan los valores calculados de los parámetros
summary(modelo1)
#Call:
#  lm(formula = sales ~ facebook + youtube + newspaper, data = marketing)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-10.5932  -1.0690   0.2902   1.4272   3.3951
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.526667   0.374290   9.422   <2e-16 ***
#facebook     0.188530   0.008611  21.893   <2e-16 ***
#  youtube      0.045765   0.001395  32.809   <2e-16 ***
#  newspaper   -0.001037   0.005871  -0.177     0.86    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.023 on 196 degrees of freedom
#Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956 
#F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16

# RESPUESTA:
# El valor p-value: < 2.2e-16 (último renglón) indica que la hipotesis de que 
# los coeficientes son nulos, es altamente improbable porque p-valor es casi 0.
# Es decir, el modelo obtenido hace pronósticos cercanos a los reales. 
# Los valores de "Pr(>|t|)" para los parámetros facebook y youtube son "<2e-16",
# lo cual indica que también la probabilidad de que sean nulos es casi 0, por
# cual SON PARÁMETROS ADECUADOS PARA LA REGRESIÓN. 
# Por otro lado, en "newspaper" se tiene una probabilidad
# de 0.86, lo cual descalifica a este parámetro para uso en la regresión.

conf_int_all <- confint(modelo1, level = 0.95)
print(conf_int_all)
#2.5 %     97.5 %
#(Intercept)  2.78851474 4.26481975
#facebook     0.17154745 0.20551259
#youtube      0.04301371 0.04851558
#newspaper   -0.01261595 0.01054097
#
# RESPUESTA:
# Se muestran los límites inferior y superior de los parámetros.
# para un nivel de confianza del 95%.

nuevos_datos <- data.frame(youtube=150, facebook=30, newspaper=20)
ic_media <- predict(modelo1, newdata=nuevos_datos,interval="confidence", level=0.95)
print(ic_media)
#fit      lwr      upr
#1 16.02651 15.66985 16.38318

# RESPUESTA a)
# Se estima una media de sales = 16.026, con un intervalo del confianza del 95%
# de [15.669, 16.383]. Se utilizó interval="confidence" para hacer este cálculo.

ic_media <- predict(modelo1, newdata=nuevos_datos,interval="predict", level=0.95)
print(ic_media)
#fit      lwr      upr
#1 16.02651 12.02172 20.03131

# RESPUESTA b)
# El intervalo de confianza del 95% para obtener una medición se hizo más amplio, 
# de  [12.026, 20.031]. Se utilzó interval="predict" para hacer este cálculo.

# RESPUESTA c)
# En a) se calculó un intervalo para la MEDIA de sales, dado un valor de los
# parámetros. En b) se calculó el intervalo para una observación de sales.
# Debido a que la media es el promedio de observaciones individuales, su 
# varianza es mucho menor que la de una observación. Esto explica porque el 
# intervalo de la media es más pequeño que el intervalo de una observación. 