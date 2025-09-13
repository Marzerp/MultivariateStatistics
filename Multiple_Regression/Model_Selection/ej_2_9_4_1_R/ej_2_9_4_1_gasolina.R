# 2.9.4.1 Ejercicios
# Ejercicio 1: Para los datos de rendimiento de gasolina. 
# Aplica los tres métodos vistos de selección de modelos. 
# Compara los resultados y especifica cual sería el mejor modelo con cuales estadísticas.

datos_brutos <- read.csv("Rendimiento_de_gasolina.csv")
datos_brutos
# Eliminar filas con valores faltantes ANTES de modelar
datos <- na.omit(datos_brutos)  # Elimina filas con NA
datos

# Modelo nulo (solo intercepto)
modelo_nulo <- lm(y ~ 1, data = datos)

# Modelo completo (todas las variables)
modelo_completo <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = datos)

# Selección hacia adelante con step()
cat("*** MODELO FORWARD ***")
seleccion_stepF <- step(modelo_nulo, 
                       scope = list(lower = modelo_nulo, upper = modelo_completo),
                       direction = "forward",
#                       k = qchisq(1 - 0.05, 1),  # Esto hace que step use p-valores ≈ alpha
                       trace = 1)  # trace=1 muestra los pasos

cat("\nModelo seleccionado por step(direction=forward):\n")
print(summary(seleccion_stepF))

#Start:  AIC=111.1
#y ~ 1
#Step:  AIC=70.21
#y ~ x1
#Step:  AIC=70.09
#y ~ x1 + x4
#
#Modelo seleccionado por step(direction=forward):
#  
#  Call:
#  lm(formula = y ~ x1 + x4, data = datos)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.5011 -2.1243 -0.3884  1.9964  6.9582 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.179421  18.787955   0.382    0.705    
#x1          -0.044479   0.005225  -8.513 3.98e-09 ***
#  x4           3.077228   2.190294   1.405    0.171    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.067 on 27 degrees of freedom
#Multiple R-squared:  0.777,	Adjusted R-squared:  0.7605 
#F-statistic: 47.03 on 2 and 27 DF,  p-value: 1.594e-09

# *************************************************************************
# Método Forward
# El método inicia con el modelo que contiene solo el intercepto y después
# introduce x1 y x4. Inicia con AIC=111.1 y termina AIC=70.09.
# *************************************************************************

cat("*** MODELO BACKWARD ***")
seleccion_stepB <- step(modelo_completo, 
                       scope = list(lower = modelo_nulo, upper = modelo_completo),
                       direction = "backward",  
#                       k = qchisq(1 - 0.05, 1),  # Mantiene el criterio de p-valores ≈ alpha
                       trace = 1)  # trace=1 muestra los pasos
cat("\nModelo seleccionado por step(direction=backward):\n")
print(summary(seleccion_stepB))

#Start:  AIC=78.96
#y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11
#Step:  AIC=77.04
#y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
#Step:  AIC=75.12
#y ~ x1 + x2 + x3 + x4 + x5 + x7 + x8 + x9 + x10
#Step:  AIC=73.67
#y ~ x1 + x2 + x3 + x5 + x7 + x8 + x9 + x10
#Step:  AIC=73.31
#y ~ x1 + x3 + x5 + x7 + x8 + x9 + x10
#Step:  AIC=72.82
#y ~ x1 + x3 + x5 + x8 + x9 + x10
#Step:  AIC=71.49
#y ~ x1 + x5 + x8 + x9 + x10
#Step:  AIC=69.6
#y ~ x5 + x8 + x9 + x10
#Step:  AIC=68.29
#y ~ x5 + x8 + x10
#
#Modelo seleccionado por step(direction=backward):
#  
#  Call:
#  lm(formula = y ~ x5 + x8 + x10, data = datos)
#
#Residuals:
# Min      1Q  Median      3Q     Max 
#-4.6101 -1.9868 -0.6613  2.0369  5.8811 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.590404  11.771925   0.390   0.6998    
#x5           2.597240   1.264562   2.054   0.0502 .  
#x8           0.217814   0.087817   2.480   0.0199 *  
#  x10         -0.009485   0.001994  -4.757 6.38e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2.934 on 26 degrees of freedom
#Multiple R-squared:  0.8035,	Adjusted R-squared:  0.7808 
#F-statistic: 35.44 on 3 and 26 DF,  p-value: 2.462e-09

# *************************************************************************
# Método Backward
# El método inicia con el modelo completo y después elimina variables hasta
# quedar sólo con x5, x8 y x10. Inicia con AIC=78.96 y termina AIC=68.29
# *************************************************************************

# Stepwise selection con criterio AIC estándar
cat("*** MODELO STEPWISE ***")
seleccion_stepwise <- step(modelo_nulo,
                           scope = list(lower = modelo_nulo, upper = modelo_completo),
                           direction = "both",
                           trace = 1)
cat("\nModelo seleccionado por step(direction=both):\n")
print(summary(seleccion_stepwise))

#Start:  AIC=111.1
#y ~ 1
#Step:  AIC=70.21
#y ~ x1
#Step:  AIC=70.09
#y ~ x1 + x4
#Modelo seleccionado por step(direction=both):
#  
#  Call:
#  lm(formula = y ~ x1 + x4, data = datos)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.5011 -2.1243 -0.3884  1.9964  6.9582 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.179421  18.787955   0.382    0.705    
#x1          -0.044479   0.005225  -8.513 3.98e-09 ***
#  x4           3.077228   2.190294   1.405    0.171    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.067 on 27 degrees of freedom
#Multiple R-squared:  0.777,	Adjusted R-squared:  0.7605 
#F-statistic: 47.03 on 2 and 27 DF,  p-value: 1.594e-09

# *************************************************************************
# Método Stepwise
# El método inicia con el modelo que contiene solo el intercepto y después
# introduce x1 y x4. Obtuvo el mismo resultado que Forward. 
# Inicia con AIC=111.1 y termina AIC=70.09.
# *************************************************************************


# *************************************************************************
# COMPARACIÓN DE MÉTODOS:
# El mejor modelo lo obtuvo el método Backward:  y ~ x5 + x8 + x10
# con AIC=68.29, R2=0.8035 y R2_adj=0.7808.
# Los otros métodos obtuvieron y ~ x1 + x4, AIC=70.09, R2=0.777 y R2_adj=0.7605.
# *************************************************************************