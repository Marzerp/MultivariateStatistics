# 2.6 Ejercicio 5, version R
datos <- read.csv("Liga_nacional_de_futbol.csv")
n <- nrow(datos)

# **********************************************************************
# A) Usar el algoritmo de selección hacia adelante para seleccionar un 
# modelo de regresión.
# **********************************************************************

# Cargar los datos
datos <- read.csv("Liga_nacional_de_futbol.csv")

# Ver la estructura de los datos
str(datos)

# Función para selección hacia adelante
seleccion_adelante <- function(datos) {
  # Variables disponibles (todas las x)
  variables_disponibles <- names(datos)[-1]  # Excluir la variable respuesta y
  
  # Modelo inicial (solo intercepto)
  modelo_actual <- lm(y ~ 1, data = datos)
  variables_seleccionadas <- character(0)
  
  cat("Iniciando selección hacia adelante...\n")
  cat("=====================================\n")
  
  while(length(variables_disponibles) > 0) {
    mejor_pvalor <- Inf
    mejor_variable <- NULL
    mejor_modelo <- NULL
    
    # Probar cada variable disponible
    for(variable in variables_disponibles) {
      # Crear fórmula con las variables ya seleccionadas + la nueva variable
      if(length(variables_seleccionadas) == 0) {
        formula_temp <- as.formula(paste("y ~", variable))
      } else {
        formula_temp <- as.formula(paste("y ~", 
                                         paste(variables_seleccionadas, collapse = " + "), 
                                         "+", variable))
      }
      
      # Ajustar modelo temporal
      modelo_temp <- lm(formula_temp, data = datos)
      
      # Obtener el p-valor de la nueva variable
      resumen <- summary(modelo_temp)
      pvalor_nueva_variable <- resumen$coefficients[variable, "Pr(>|t|)"]
      
      # Verificar si es la mejor variable
      if(pvalor_nueva_variable < mejor_pvalor) {
        mejor_pvalor <- pvalor_nueva_variable
        mejor_variable <- variable
        mejor_modelo <- modelo_temp
      }
    }
    
    # Verificar criterio de parada (p-valor > 0.05)
    if(mejor_pvalor > 0.05) {
      cat("\nCriterio de parada alcanzado. Ninguna variable restante tiene p-valor < 0.05\n")
      break
    }
    
    # Agregar la mejor variable al modelo
    variables_seleccionadas <- c(variables_seleccionadas, mejor_variable)
    variables_disponibles <- setdiff(variables_disponibles, mejor_variable)
    modelo_actual <- mejor_modelo
    
    # Mostrar progreso
    cat(sprintf("Paso %d: Se agregó '%s' (p-valor = %.4f)\n", 
                length(variables_seleccionadas), 
                mejor_variable, 
                mejor_pvalor))
    cat(sprintf("   R² ajustado: %.4f, AIC: %.2f\n", 
                summary(modelo_actual)$adj.r.squared,
                AIC(modelo_actual)))
  }
  
  cat("\n=====================================\n")
  cat("Selección finalizada\n")
  cat("Variables seleccionadas:", paste(variables_seleccionadas, collapse = ", "), "\n")
  cat("Fórmula final: y ~", paste(variables_seleccionadas, collapse = " + "), "\n\n")
  
  # Mostrar resumen del modelo final
  cat("Resumen del modelo final:\n")
  print(summary(modelo_actual))
  
  return(list(modelo = modelo_actual, 
              variables = variables_seleccionadas,
              formula = formula(modelo_actual)))
}

# Ejecutar la selección hacia adelante
resultado <- seleccion_adelante(datos)

# Alternativa usando la función step() de R (más eficiente)
cat("\nUsando la función step() de R:\n")
cat("==============================\n")

# Modelo nulo (solo intercepto)
modelo_nulo <- lm(y ~ 1, data = datos)

# Modelo completo (todas las variables)
modelo_completo <- lm(y ~ ., data = datos)

# Selección hacia adelante con step()
seleccion_step <- step(modelo_nulo, 
                       scope = list(lower = modelo_nulo, upper = modelo_completo),
                       direction = "forward",
                       k = qchisq(1 - 0.05, 1),  # Esto hace que step use p-valores ≈ alpha
                       trace = 1)  # trace=1 muestra los pasos

cat("\nModelo seleccionado por step():\n")
summary(seleccion_step)
#
#Iniciando selección hacia adelante...
#=====================================
#Paso 1: Se agregó 'x8' (p-valor = 0.0000)
#R² ajustado: 0.5272, AIC: 132.25
#Paso 2: Se agregó 'x2' (p-valor = 0.0002)
#R² ajustado: 0.7227, AIC: 118.20
#Paso 3: Se agregó 'x7' (p-valor = 0.0378)
#R² ajustado: 0.7596, AIC: 115.06

#Criterio de parada alcanzado. Ninguna variable restante tiene p-valor < 0.05

# **********************************************************************
# B) Usar el algoritmo de selección hacia atrás para seleccionar un 
# modelo de regresión.
# **********************************************************************
# Cargar los datos
datos <- read.csv("Liga_nacional_de_futbol.csv")

# Ver la estructura de los datos
cat("Estructura de los datos:\n")
str(datos)
cat("\n")

# Función para selección hacia atrás basada en p-valores
seleccion_atras_pvalor <- function(datos, alpha = 0.05) {
  # Modelo completo con todas las variables
  modelo_completo <- lm(y ~ ., data = datos)
  variables_actuales <- names(coef(modelo_completo))[-1]  # Excluir intercepto
  
  cat("Iniciando selección hacia atrás...\n")
  cat("==================================\n")
  cat("Modelo inicial con todas las variables:\n")
  print(summary(modelo_completo)$r.squared)
  cat("\n")
  
  paso <- 1
  modelo_actual <- modelo_completo
  
  while(length(variables_actuales) > 0) {
    # Obtener p-valores del modelo actual
    resumen_actual <- summary(modelo_actual)
    pvalores <- resumen_actual$coefficients[-1, 4]  # Excluir intercepto
    names(pvalores) <- variables_actuales
    
    # Encontrar la variable con el p-valor más alto
    peor_pvalor <- max(pvalores)
    peor_variable <- names(which.max(pvalores))
    
    # Verificar criterio de parada
    if(peor_pvalor <= alpha) {
      cat("Criterio de parada alcanzado. Todas las variables tienen p-valor <=", alpha, "\n")
      break
    }
    
    # Eliminar la variable con p-valor más alto
    variables_actuales <- setdiff(variables_actuales, peor_variable)
    
    # Crear nueva fórmula
    if(length(variables_actuales) == 0) {
      formula_nueva <- as.formula("y ~ 1")
    } else {
      formula_nueva <- as.formula(paste("y ~", paste(variables_actuales, collapse = " + ")))
    }
    
    # Ajustar nuevo modelo
    modelo_nuevo <- lm(formula_nueva, data = datos)
    
    cat(sprintf("Paso %d: Se eliminó '%s' (p-valor = %.4f)\n", 
                paso, peor_variable, peor_pvalor))
    cat(sprintf("   R² ajustado: %.4f, AIC: %.2f\n", 
                summary(modelo_nuevo)$adj.r.squared,
                AIC(modelo_nuevo)))
    
    modelo_actual <- modelo_nuevo
    paso <- paso + 1
  }
  
  cat("\n==================================\n")
  cat("Selección finalizada\n")
  cat("Variables finales:", paste(variables_actuales, collapse = ", "), "\n")
  cat("Fórmula final:", paste(deparse(formula(modelo_actual)), collapse = ""), "\n\n")
  
  # Mostrar resumen del modelo final
  cat("Resumen del modelo final:\n")
  print(summary(modelo_actual))
  
  return(list(modelo = modelo_actual, 
              variables = variables_actuales,
              formula = formula(modelo_actual)))
}

# Ejecutar los diferentes métodos
cat("MÉTODO MANUAL (p-valor < 0.05):\n")
cat("===============================\n")
resultado_manual <- seleccion_atras_pvalor(datos)
#
#MÉTODO MANUAL (p-valor < 0.05):
#  ===============================
#  Iniciando selección hacia atrás...
#==================================
#  Modelo inicial con todas las variables:
#  [1] 0.8155969

#Paso 1: Se eliminó 'x5' (p-valor = 0.9997)
#R² ajustado: 0.7380, AIC: 120.94
#Paso 2: Se eliminó 'x1' (p-valor = 0.6811)
#R² ajustado: 0.7488, AIC: 119.19
#Paso 3: Se eliminó 'x6' (p-valor = 0.6390)
#R² ajustado: 0.7580, AIC: 117.51
#Paso 4: Se eliminó 'x3' (p-valor = 0.4514)
#R² ajustado: 0.7625, AIC: 116.28
#Paso 5: Se eliminó 'x4' (p-valor = 0.4446)
#R² ajustado: 0.7666, AIC: 115.04
#Paso 6: Se eliminó 'x9' (p-valor = 0.2024)
#R² ajustado: 0.7596, AIC: 115.06
#Criterio de parada alcanzado. Todas las variables tienen p-valor <= 0.05 
#
#==================================
#  Selección finalizada
#Variables finales: x2, x7, x8 
#Fórmula final: y ~ x2 + x7 + x8 
#
#Resumen del modelo final:
#  
#  Call:
#  lm(formula = formula_nueva, data = datos)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.0370 -0.7129 -0.2043  1.1101  3.7049 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.808372   7.900859  -0.229 0.820899    
#x2           0.003598   0.000695   5.177 2.66e-05 ***
#  x7           0.193960   0.088233   2.198 0.037815 *  
#  x8          -0.004816   0.001277  -3.771 0.000938 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.706 on 24 degrees of freedom
#Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
#F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08

# **********************************************************************
# C) Usar el algoritmo de regresión por pasos para seleccionar un 
# modelo de regresión.
# **********************************************************************

# Cargar los datos
datos <- read.csv("Liga_nacional_de_futbol.csv")

# Ver la estructura de los datos
cat("Estructura de los datos:\n")
str(datos)
cat("\n")

# Función para regresión por pasos manual
regresion_por_pasos_manual <- function(datos, alpha_entrada = 0.05, alpha_salida = 0.10) {
  # Variables disponibles
  variables_disponibles <- names(datos)[-1]  # Excluir y
  variables_modelo <- character(0)
  
  cat("Iniciando regresión por pasos manual...\n")
  cat("=======================================\n")
  cat("Criterios: Entrada (p <", alpha_entrada, "), Salida (p >", alpha_salida, ")\n\n")
  
  paso <- 1
  cambios <- TRUE
  modelo_actual <- lm(y ~ 1, data = datos)  # Modelo inicial con solo intercepto
  
  while(cambios) {
    cambios <- FALSE
    
    # PASO 1: SELECCIÓN HACIA ADELANTE
    if(length(variables_disponibles) > 0) {
      cat(sprintf("--- Paso %d: Fase de entrada ---\n", paso))
      
      mejor_p_entrada <- Inf
      mejor_var_entrada <- NULL
      mejor_modelo_entrada <- NULL
      
      # Probar cada variable disponible para entrada
      for(variable in variables_disponibles) {
        # Crear fórmula temporal
        if(length(variables_modelo) == 0) {
          formula_temp <- as.formula(paste("y ~", variable))
        } else {
          formula_temp <- as.formula(paste("y ~", paste(variables_modelo, collapse = " + "), "+", variable))
        }
        
        # Ajustar modelo temporal
        modelo_temp <- lm(formula_temp, data = datos)
        resumen_temp <- summary(modelo_temp)
        
        # Obtener p-valor de la nueva variable
        pvalor_nuevo <- resumen_temp$coefficients[variable, "Pr(>|t|)"]
        
        # Verificar si es la mejor candidata para entrada
        if(pvalor_nuevo < mejor_p_entrada) {
          mejor_p_entrada <- pvalor_nuevo
          mejor_var_entrada <- variable
          mejor_modelo_entrada <- modelo_temp
        }
      }
      
      # Verificar criterio de entrada
      if(mejor_p_entrada < alpha_entrada) {
        # Agregar variable al modelo
        variables_modelo <- c(variables_modelo, mejor_var_entrada)
        variables_disponibles <- setdiff(variables_disponibles, mejor_var_entrada)
        modelo_actual <- mejor_modelo_entrada
        
        cat(sprintf("   ENTRADA: Se agregó '%s' (p = %.4f)\n", mejor_var_entrada, mejor_p_entrada))
        cat(sprintf("   R² ajustado: %.4f, AIC: %.2f\n", 
                    summary(modelo_actual)$adj.r.squared,
                    AIC(modelo_actual)))
        cat(sprintf("   Variables en modelo: %s\n", paste(variables_modelo, collapse = ", ")))
        
        cambios <- TRUE
        paso <- paso + 1
        next  # Ir al siguiente paso después de una entrada
      }
    }
    
    # PASO 2: ELIMINACIÓN HACIA ATRÁS
    if(length(variables_modelo) > 0) {
      cat(sprintf("--- Paso %d: Fase de salida ---\n", paso))
      
      # Obtener p-valores del modelo actual
      resumen_actual <- summary(modelo_actual)
      pvalores_actuales <- resumen_actual$coefficients[variables_modelo, "Pr(>|t|)"]
      
      # Encontrar variable con mayor p-valor
      peor_p_salida <- max(pvalores_actuales)
      peor_var_salida <- names(which.max(pvalores_actuales))
      
      # Verificar criterio de salida
      if(peor_p_salida > alpha_salida) {
        # Eliminar variable del modelo
        variables_modelo <- setdiff(variables_modelo, peor_var_salida)
        variables_disponibles <- c(variables_disponibles, peor_var_salida)
        
        # Ajustar nuevo modelo
        if(length(variables_modelo) == 0) {
          modelo_actual <- lm(y ~ 1, data = datos)
        } else {
          formula_nueva <- as.formula(paste("y ~", paste(variables_modelo, collapse = " + ")))
          modelo_actual <- lm(formula_nueva, data = datos)
        }
        
        cat(sprintf("   SALIDA: Se eliminó '%s' (p = %.4f)\n", peor_var_salida, peor_p_salida))
        cat(sprintf("   R² ajustado: %.4f, AIC: %.2f\n", 
                    summary(modelo_actual)$adj.r.squared,
                    AIC(modelo_actual)))
        cat(sprintf("   Variables en modelo: %s\n", paste(variables_modelo, collapse = ", ")))
        
        cambios <- TRUE
        paso <- paso + 1
      } else {
        cat("   Ninguna variable cumple criterio de salida (p >", alpha_salida, ")\n")
      }
    }
    
    # Si no hubo cambios en ninguna fase, terminar
    if(!cambios && length(variables_disponibles) > 0) {
      cat("No se pueden agregar más variables (p >", alpha_entrada, ") y no se pueden eliminar (p <", alpha_salida, ")\n")
      break
    }
  }
  
  cat("\n=======================================\n")
  cat("Regresión por pasos finalizada\n")
  cat("Variables finales:", paste(variables_modelo, collapse = ", "), "\n")
  cat("Fórmula final: y ~", ifelse(length(variables_modelo) > 0, 
                                   paste(variables_modelo, collapse = " + "), 
                                   "1"), "\n\n")
  
  # Mostrar resumen del modelo final
  cat("Resumen del modelo final:\n")
  print(summary(modelo_actual))
  
  return(list(modelo = modelo_actual, 
              variables = variables_modelo,
              formula = formula(modelo_actual)))
}

# Ejecutar los métodos
cat("MÉTODO MANUAL DE REGRESIÓN POR PASOS:\n")
cat("=====================================\n")
resultado_manual <- regresion_por_pasos_manual(datos)

cat("\nRESULTADOS:\n")
cat("==========================\n")
cat("Manual - Variables:", paste(resultado_manual$variables, collapse = ", "), "\n")
#MÉTODO MANUAL DE REGRESIÓN POR PASOS:
#=====================================
#  Iniciando regresión por pasos manual...
#=======================================
#  Criterios: Entrada (p < 0.05 ), Salida (p > 0.1 )
#
#--- Paso 1: Fase de entrada ---
#  ENTRADA: Se agregó 'x8' (p = 0.0000)
#R² ajustado: 0.5272, AIC: 132.25
#Variables en modelo: x8
#--- Paso 2: Fase de entrada ---
#  ENTRADA: Se agregó 'x2' (p = 0.0002)
#R² ajustado: 0.7227, AIC: 118.20
#Variables en modelo: x8, x2
#--- Paso 3: Fase de entrada ---
#  ENTRADA: Se agregó 'x7' (p = 0.0378)
#R² ajustado: 0.7596, AIC: 115.06
#Variables en modelo: x8, x2, x7
#--- Paso 4: Fase de entrada ---
#  --- Paso 4: Fase de salida ---
#  Ninguna variable cumple criterio de salida (p > 0.1 )
#No se pueden agregar más variables (p > 0.05 ) y no se pueden eliminar (p < 0.1 )
#
#=======================================
#  Regresión por pasos finalizada
#Variables finales: x8, x2, x7 
#Fórmula final: y ~ x8 + x2 + x7 
#
#Resumen del modelo final:
#  
#  Call:
#  lm(formula = formula_temp, data = datos)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.0370 -0.7129 -0.2043  1.1101  3.7049 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.808372   7.900859  -0.229 0.820899    
#x8          -0.004816   0.001277  -3.771 0.000938 ***
#  x2           0.003598   0.000695   5.177 2.66e-05 ***
#  x7           0.193960   0.088233   2.198 0.037815 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.706 on 24 degrees of freedom
#Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
#F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08
#
#RESULTADOS:
#  ==========================
#  Manual - Variables: x8, x2, x7 

# ****************************************************************************
# D) Comenta los modelos finales en cada uno de los casos anteriores. ¿Cuál tiene 
# más sentido? ¿Cuál modelo usarían?
# ****************************************************************************
#
#
# En cada caso se llega al mismo modelo, con las variables x8, x2 y x7. Yo utilizaría
# el algoritmo de selección por pasos, por eficiente y por superar algunas de las
# limitaciones del forward. 