# 2.6 Ejercicio 1, version R
#setwd("~/Documents/E_MULTIVARIADA/Tareas/ej_2_6/ej_1/R/")
datos <- read.csv("Liga_nacional_de_futbol.csv")
n <- nrow(datos)

# **********************************************************************
# A) Ajustar un modelo de regresión lineal múltiple que relacione la 
# cantidad de juegos ganados con las yardas por aire del equipo (x2), 
# el porcentaje de jugadas por tierra (x7) y las yardas por tierra del 
#contrario (x8)

# Usando matrices

#col de 1 para intercepto
idv <- rep(1, n)

#creamos matriz x
X <- matrix(c(idv, datos$x2, datos$x7, datos$x8), nrow = n, ncol= 4)

#creamos matriz y
y <-  matrix(datos$y, nrow = n, ncol= 1)

#el estimador de \hat{\beta} = (X´X)^{-1}X'y
beta <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta) 
#            [,1]
#[1,] -1.808372059
#[2,]  0.003598070
#[3,]  0.193960210
#[4,] -0.004815494

#INCISO A usando matrices: y = -1.8083 + 0.0035*x2 + 0.1939*x7 - 0.0048*x8

# Usando la biblioteca lm 

library(tibble) 
data_intro <- tibble(x2=datos$x2, x7=datos$x7, x8=datos$x8, y=datos$y)
m1 <- lm(y ~ x2 + x7 + x8, data = data_intro)
print(m1)
#Coefficients:
#(Intercept)           x2           x7           x8  
#-1.808372     0.003598     0.193960    -0.004815  
#INCISO A usando lm: y = -1.8083 + 0.0035*x2 + 0.1939*x7 - 0.0048*x8

# **********************************************************************
# B) Formar la tabla de análisis de varianza y probar la significancia de la 
# regresión.

SCT <- t(y) %*% y -  sum(y)**2 / nrow(datos)
SCE <- t(beta) %*% t(X) %*% y - sum(y)**2 / nrow(datos)
SSE <- SCT - SCE
print(paste("Analisis de varianza"))
print(paste("SSE = ", SSE))

# Para probar la hipotesis H0 (coeficientes 0), se calcula F0 y p-valor

n <- nrow(y); p <- ncol(X) - 1
F0 <- (SCE / p) / (SSE / (n - p - 1))

m1 <- lm(y ~ x2 + x7 + x8, data = data_intro)
print(paste("n = ", n))
GLT<- n-1
print(paste("GLT = ", GLT))
GLRes<- df.residual(m1)
print(paste("GLRes = ", GLRes))
GLR<- GLT-GLRes
print(paste("GLR = ", GLR))
alpha <- 0.05
F_crit <- qf(1 - alpha, GLR, GLRes)
pv <- 1 - pf(F0, GLR, GLRes)
print(paste("F0 = ", F0, "F_crit =", F_crit, "p_valor = ", pv))
# "F0 =  29.4368703185818 F_crit = 3.00878657044736 p-valor =  3.27345828221581e-08"
#
# La hipotesis de que los coeficientes son nulos, es altamente improbable porque
# p_valor es casi 0. Esto tambien se refleja en que F0 es mucho mayor que F_crit.
# El decir, el modelo obtenido hace pronostico cercanos a los reales. 

# **********************************************************************
# C) Calcular el estadístico t para probar las hipótesis H0:β2=0, H0:β7=0 y H0:β8=0. 
# ¿Qué conclusiones se pueden sacar acerca del papel de las variables x2, x7 y x8 en el modelo?

# Obtener los errores estándar
SSE <- sum(residuals(m1)^2)  # otra forma de calcular SSE
sigma2 <- SSE / (n - p - 1)
XtX_inv <- solve(t(X) %*% X)
vcov_matrix <- sigma2 * XtX_inv
std_errors <- sqrt(diag(vcov_matrix))
# Formato de calculo usando funciones de biblioteca
# std_errors <- sqrt(diag(vcov(m1)))

# Obtener los coeficientes 
coeficientes <- coef(m1)
print(paste("coeficientes: ", coeficientes))
print(std_errors)

# Calcular estadísticos t
t_values <- coeficientes / std_errors

# Grados de libertad residuales
n <- nrow(model.frame(m1))
p <- length(coeficientes) - 1  # número de variables predictoras
df_residual <- n - p - 1

# Calcular p-values (prueba bilateral)
p_values <- 2 * pt(-abs(t_values), df = df_residual)
print(p_values)
# > print(p_values)
#(Intercept)           x2           x7           x8 
# 8.208990e-01 2.655723e-05 3.781516e-02 9.377699e-04 

# Comparar con summary del modelo para verificar
print("Comparación con summary(m1):")
print(summary(m1)$coefficients)
#print(summary(m1)$coefficients)
#Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept) -1.808372059 7.9008594002 -0.228883 8.208990e-01
#x2           0.003598070 0.0006949986  5.177090 2.655723e-05
#x7           0.193960210 0.0882334488  2.198262 3.781516e-02
#x8          -0.004815494 0.0012769683 -3.771036 9.377699e-04
# En el ultimo renglon, el valor p-value coincide con el calculado antes.
# Las variables mas relevantes son x2 y x8. La menos relevante es x7, con p-value=0.03
# (se acerca al 5%).
# Los p_values coinciden con los calculados paso a paso.


# **********************************************************************
# D) Calcular R2 y R2adj para este modelo.

# Datos básicos
n <- nrow(data_intro)         
p <- length(coef(m1)) - 1 

# Cálculo de sumas de cuadrados
SST <- sum((data_intro$y - mean(data_intro$y))^2)
SSE <- sum(residuals(m1)^2)
SSR <- SST - SSE

R2 <- 1 - (SSE / SST)

# R² ajustado
R2_adj <- 1 - ((SSE / (n - p - 1)) / (SST / (n - 1)))

print(paste("R2 = ", R2))
# "R2 =  0.786306923310954"
print(paste("R2_adj = ", R2_adj))
# "R2_adj =  0.759595288724823"

# Verificar con summary
print(paste("R2 de summary:", summary(m1)$r.squared))
#"R2 de summary: 0.786306923310954"

print(paste("R2_adj de summary:", summary(m1)$adj.r.squared))
# "R2_adj de summary: 0.759595288724823"
#
# El valor de R2_adj de 0.75 (cercano a 1) indica que el modelo obtenido explica bien los datos.

# **********************************************************************
# E) Trazar una gráfica de probabilidad normal de los residuales. 
# ¿Parece haber algún problema con la hipótesis de normalidad?

plot(m1, which=1)
# La grafica se guardo en ej_1_grafica_Residuals_vs_Fitted_en_R.png. Muestra los puntos
# distribuidos cerca de la horizontal en 0.

# Prueba de normalidad
shapiro.test(residuals(m1))
#Shapiro-Wilk normality test
#
#data:  residuals(m1)
#W = 0.96508, p-value = 0.4566
#
# Debido a que p-value > 0.05 la hipotesis de normalidad de los residuos es aceptable.

# **********************************************************************
# F) Trazar e interpretar una gráfica de los residuales en función de la respuesta predicha.

plot(m1, which = 2)
# La grafica se guardo en ej_1_grafica_Q-Q_Residuales_en R.png. Muestra los puntos
# cercanos a linea de 45 grados, indicando que no hay problema con la hipotesis de normalidad

# **********************************************************************
# G) Trazar las gráficas de los residuales en función de cada una de las variables regresoras. 
# ¿Implican esas gráficas que se especificó en forma correcta el regresor?

plot(data_intro$x2, residuals(m1), 
     main = "Residuales vs x2",
     xlab = "Variable x2", ylab = "Residuales",
     pch = 19, col = "blue")
# La grafica se guarda en ej_1_grafica_Residuales_vs_x2_en_R.png

plot(data_intro$x7, residuals(m1),
     main = "Residuales vs x7", 
     xlab = "Variable x7", ylab = "Residuales",
     pch = 19, col = "blue")
# La grafica se guarda en ej_1_grafica_Residuales_vs_x7_en_R.png

plot(data_intro$x8, residuals(m1),
     main = "Residuales vs x8",
     xlab = "Variable x8", ylab = "Residuales", 
     pch = 19, col = "blue")
# La grafica se guarda en ej_1_grafica_Residuales_vs_x8_en_R.png

# La seleccion de parametros de x2, x7 y x8 es adecuada. Se cubre un rango razonable 
# de los parametros y la distribucion se acerca a la linea horizontal, con cierta simetria
# superior inferior. 

# **********************************************************************
# H) Calcular un intervalo de confianza de 95% para β7

# Obtener coeficiente y error estándar de x7
coef_x7 <- coef(m1)["x7"]
se_x7 <- sqrt(diag(vcov(m1)))["x7"]

# Grados de libertad residuales
df_res <- df.residual(m1)

# Valor crítico t (95% de confianza, bilateral)
t_critico <- qt(0.975, df_res)

# Límites del intervalo
limite_inferior <- coef_x7 - t_critico * se_x7
limite_superior <- coef_x7 + t_critico * se_x7

print(paste("Intervalo para x7: [", limite_inferior, ",", limite_superior, "]"))
#"Intervalo para x7: [ 0.0118553215676871 , 0.376065097598759 ]

# otra forma utilizando una funcion de biblioteca para todos los coeficientes
conf_int_all <- confint(m1, level = 0.95)
print(conf_int_all)
#2.5 %       97.5 %
#  (Intercept) -18.114944410 14.498200293
#x2            0.002163664  0.005032477
#x7            0.011855322  0.376065098
#x8           -0.007451027 -0.002179961

# **********************************************************************
# H) Calcular un intervalo de confianza de 95% para la cantidad media de juegos 
# ganados por un equipo cuando x2=2300, x7=56 y x8=2100

# Version paso a paso

# Valores específicos
x2_val <- 2300
x7_val <- 56  
x8_val <- 2100

# 1. Vector de características (incluye intercepto)
x_vector <- c(1, x2_val, x7_val, x8_val)  # 1 para el intercepto

# 2. Coeficientes del modelo
beta_hat <- coef(m1)

# 3. Estimación puntual
y_hat <- sum(x_vector * beta_hat)

# 4. Error estándar de la estimación media
X <- model.matrix(m1)  # Matriz de diseño original
X_new <- matrix(x_vector, nrow = 1)  # Nueva observación

# Matriz de varianza-covarianza
sigma2 <- sum(residuals(m1)^2) / df.residual(m1)
var_beta <- sigma2 * solve(t(X) %*% X)

# Error estándar para la media
se_mean <- sqrt(X_new %*% var_beta %*% t(X_new))

# 5. Valor crítico t
t_critical <- qt(0.975, df.residual(m1))

# 6. Intervalo de confianza
lower_bound <- y_hat - t_critical * se_mean
upper_bound <- y_hat + t_critical * se_mean

print(paste("Intervalo: [", lower_bound, ",", upper_bound,"]"))
#"Intervalo: [ 6.43620277665726 , 7.99664488933467 ]"

# Usando una funcion de biblioteca
# Crear nuevo data frame con los valores específicos
nuevos_datos <- data.frame(x2 = 2300, x7 = 56, x8 = 2100)

# Intervalo de confianza para la respuesta MEDIA
ic_media <- predict(m1, newdata=nuevos_datos,interval="confidence", level=0.95)

cat("Intervalo de confianza 95% para la respuesta media:\n")
print(ic_media)
#   fit      lwr      upr
# 1 7.216424 6.436203 7.996645
# 
# Se obtienen los mismos resultados en ambos casos

# **********************************************************************
# I) Ajustar un modelo a esos datos, usando solo x7 y x8 como regresores y probar 
# la significancia de la regresión

m2 <- lm(y ~ x7 + x8, data = data_intro)
print(m2)
#
#Call:
#  lm(formula = y ~ x7 + x8, data = data_intro)
#
#Coefficients:
#  (Intercept)           x7           x8  
#17.944319     0.048371    -0.006537  

# **********************************************************************
# J) Calcular R2 y R2adj. Compararlos con los resultados del modelo anterior.

print(paste("R2 de summary:", summary(m2)$r.squared))
#"R2 de summary: 0.547662835055794"

print(paste("R2_adj de summary:", summary(m2)$adj.r.squared))
"R2_adj de summary: 0.511475861860258"

# Comparados con el modelo 1 (usando x2, x7 y x8), R2_adj bajo de 0.75 a 0.51. 
# Este modelo tiene un desempeno pobre
# "R2 de summary: 0.786306923310954"      # para modelo m1
# "R2_adj de summary: 0.759595288724823"  # para modelo m1
#

# **********************************************************************
# K) Calcular un intervalo de confianza de 95% para β7.

# Es mas simple, calcular para todos los parametros
conf_int_all <- confint(m2, level = 0.95)
print(conf_int_all)
# 2.5 %       97.5 %
# (Intercept) -2.36784828 38.256485319
#x7          -0.19716429  0.293906022
#x8          -0.01015637 -0.002916818
  
# **********************************************************************
# K) También un intervalo de confianza de 95% para la cantidad media de juegos 
#  ganados por un equipo cuando x7=56 y x8=2100

nuevos_datos <- data.frame(x7 = 56, x8 = 2100)
ic_media <- predict(m2, newdata=nuevos_datos,interval="confidence", level=0.95)
print(ic_media)
#       fit      lwr      upr
# 1 6.926243 5.828643 8.023842

# Resultados anteriores del modelo m1
#   fit      lwr      upr
# 1 7.216424 6.436203 7.996645

# Se observa que el rango de variacion es mayor:[5.82, 8.02] en comparacion
# con el rango del modelo m1 (usando x2, x7 y x8): [6.43,7.99].

# **********************************************************************
# L) ¿Qué conclusiones se pueden sacar de este problema, acerca de las consecuencias
# de omitir un regresor importante de un modelo?

# La confiabilidad de las predicciones del modelo se degradan. El modelo no logra
# explicar apropiadamente la variabilidad de los datos. 