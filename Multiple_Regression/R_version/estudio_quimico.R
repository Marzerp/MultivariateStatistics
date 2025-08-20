# Ejercicio 2. Usando datasets::trees 
# Los resultados están comentados con "#"

datos = datasets::trees
datos

#col de 1 para intercepto
idv = rep(1, nrow(datos))

#creamos matriz x
X = matrix(c(idv, datos$Girth, datos$Height), nrow = 31, ncol= 3)

#creamos matriz y
y =  matrix(datos$Volume, nrow = 31, ncol= 1)

#el estimador de \hat{\beta} = (X´X)^{-1}X'y
beta = solve(t(X) %*% X)%*% t(X)%*% y
beta 

# \hat{y} = beta[1] + beta[2]X1 + beta[3]X2
M1 <- lm(y ~ datos$Girth + datos$Height, data=datos)
M1
#> M1
#
#Call:
#  lm(formula = y ~ datos$Girth + datos$Height, data = datos)
#
#Coefficients:
#  (Intercept)   data$Girth  data$Height  
#-57.9877       4.7082       0.3393  


# Estimación de sigma cuadrada

SSE <- t(y) %*% y -t(beta) %*% t(X) %*% y
SSE

varest <- SSE / (nrow(y)-nrow(beta))
varest

summary(M1)
sum(residuals(M1)^2)/df.residual(M1)

par(mfrow=c(1,1))
plot(M1, which=1)
# La gráfica muestra en el eje x los valores predichos mientras que en el eje y
# se encuentran los residuos
# la gráfica se encuentra en el archivo grafica_estudio_arboles_residuales.png  

# Cargar librerías
library(plotly)

# Generar las predicciones
predicciones <- predict(M1, newdata = datos)

# Crear la gráfica 3D
p <- plot_ly() %>%
  # Datos reales (en azul)
  add_markers(x = datos$Girth, y = datos$Height, z = datos$Volume, 
              marker = list(color = 'blue', size = 5), 
              name = 'Mediciones reales') %>%
  # Datos de predicción (en rojo)
  add_markers(x = datos$Girth, y = datos$Height, z = predicciones, 
              marker = list(color = 'red', size = 5), 
              name = 'Predicciones') %>%
  layout(title = 'Gráfico 3D: Mediciones reales vs Predicciones',
         scene = list(
           xaxis = list(title = 'Girth'),
           yaxis = list(title = 'Height'),
           zaxis = list(title = 'Volume')
         ))

# Mostrar la gráfica
p
# grafica en archivo grafica_estudio_arboles.png
# Las predicciones se ajustan bien a los datos reales