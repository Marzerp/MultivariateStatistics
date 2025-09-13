# 2.8.1 Ejercicios
# Ejercicio 1(R): Para el datasets datasets::trees, realice las pruebas de 
# hipótesis para determinar si: 
# 1) El modelo solo con la variable Girth es mejor que el modelo completo. 
# 2) El modelo sin Girth y Height es mejor que el completo. 
# Usar la tabla de ANOVA para calcular el estadístico F0 y encontrar el p-valor 
# asociado usando pf(F_0, df1, df2, lower.tail = FALSE). 
# Justifique su respuesta y suba su código en R a github.

library(datarium)
data("trees")
str(trees)

modelo_Completo <- lm(Volume ~ Girth + Height, data = trees)
print(paste("Modelo Completo"))
print(summary(modelo_Completo))
print(anova(modelo_Completo))

#[1] "Modelo Completo"
#
#Call:
#  lm(formula = Volume ~ Girth + Height, data = trees)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.4065 -2.6493 -0.2876  2.2003  8.4847 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -57.9877     8.6382  -6.713 2.75e-07 ***
#  Girth         4.7082     0.2643  17.816  < 2e-16 ***
#  Height        0.3393     0.1302   2.607   0.0145 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.882 on 28 degrees of freedom
#Multiple R-squared:  0.948,	Adjusted R-squared:  0.9442 
#F-statistic:   255 on 2 and 28 DF,  p-value: < 2.2e-16
#
#Analysis of Variance Table
#
#Response: Volume
#Df Sum Sq Mean Sq  F value  Pr(>F)    
#  Girth      1 7581.8  7581.8 503.1503 < 2e-16 ***
#  Height     1  102.4   102.4   6.7943 0.01449 *  
#  Residuals 28  421.9    15.1                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

modelo_Girth <- lm(Volume ~ Girth, data = trees)
print(paste("Modelo solo con Girth"))
print(summary(modelo_Girth))
print(anova(modelo_Girth))

#[1] "Modelo solo con Girth"
#
#Call:
#  lm(formula = Volume ~ Girth, data = trees)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-8.065 -3.107  0.152  3.495  9.587 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -36.9435     3.3651  -10.98 7.62e-12 ***
#  Girth         5.0659     0.2474   20.48  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.252 on 29 degrees of freedom
#Multiple R-squared:  0.9353,	Adjusted R-squared:  0.9331 
#F-statistic: 419.4 on 1 and 29 DF,  p-value: < 2.2e-16
#
#Analysis of Variance Table
#
#Response: Volume
#Df Sum Sq Mean Sq F value    Pr(>F)    
#Girth      1 7581.8  7581.8  419.36 < 2.2e-16 ***
#  Residuals 29  524.3    18.1                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

n = nrow(trees)
SSE_C <- sum(residuals(modelo_Completo)^2)  # da 421.9214
SSE_R <- sum(residuals(modelo_Girth)^2)     # da 524.3025
SCE_C = SSE_R - SSE_C
F0 <- (SCE_C/GLR) / (SSE_C/GLRes)
alpha <- 0.05
GLT<- n-1                             # da 30
GLRes<- df.residual(modelo_Completo)  # da 28
GLR <- 1  # Solo agregamos 1 predictor (Height)
F_crit <- qf(1 - alpha, GLR, GLRes)
pv <- 1 - pf(F0, GLR, GLRes)
print(paste("F0 = ", F0, "F_crit =", F_crit, "p_valor = ", pv))
#[1] "F0 =  6.79433017950626 F_crit = 4.19597181855776 p_valor =  0.0144909745250643"
#
# ANOVA compara los dos modelos
resultado_anova <- anova(modelo_Girth, modelo_Completo)
print(resultado_anova)
#Analysis of Variance Table
#
#Model 1: Volume ~ Girth
#Model 2: Volume ~ Girth + Height
#Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
#1     29 524.30                              
#2     28 421.92  1    102.38 6.7943 0.01449 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# RESPUESTA 1)
# ¿El modelo solo con la variable Girth es mejor que el modelo completo?
# RESPUESTA:  NO
# Tenemos un p_valor=0.014, lo cual indica que la reducción de SSE_C (421)
# contra SSE_R (524) es estadísticamente significativa. 
# p-valor = 0.014 < α = 0.05, por lo tanto se rechaza la hipótesis nula.
# Existe evidencia estadística significativa de que agregar "Height" al modelo 
# que ya tiene "Girth" mejora significativamente la predicción del Volumen
# Esto también coincide con el valor 
# "Adjusted R-squared:  0.9442" del modelo completo, contra el valor de 
# "Adjusted R-squared:  0.9331" del modelo con solo Girth. 

modelo_Intercepto <- lm(Volume ~ 1, data = trees)
print(paste("Modelo solo con Intercepto"))
print(summary(modelo_Intercepto))
print(anova(modelo_Intercepto))

#[1] "Modelo solo con Intercepto"
#
#Call:
#  lm(formula = Volume ~ 1, data = trees)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-19.971 -10.771  -5.971   7.129  46.829 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   30.171      2.952   10.22 2.75e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 16.44 on 30 degrees of freedom
#
#Analysis of Variance Table
#
#Response: Volume
#Df Sum Sq Mean Sq F value Pr(>F)
#Residuals 30 8106.1   270.2               

n = nrow(trees)
SSE_C <- sum(residuals(modelo_Completo)^2)  # da 421.9214
SSE_R <- sum(residuals(modelo_Intercepto)^2)# da 8106.084
SCE_C = SSE_R - SSE_C
alpha <- 0.05
GLT <- n - 1                           # 30 (total)
GLRes_Completo <- df.residual(modelo_Completo)  # n - p - 1 = 31 - 2 - 1 = 28
GLR <- 2  # Número de parámetros adicionales en el modelo completo (Girth + Height)
F0 <- (SCE_C/GLR) / (SSE_C/GLRes_Completo)
F_crit <- qf(1 - alpha, GLR, GLRes_Completo)
pv <- 1 - pf(F0, GLR, GLRes_Completo)
print(paste("F0 =", F0, "F_crit =", F_crit, "p_valor =", pv))

#[1] "F0 = 254.972337410669 F_crit = 3.34038555823776 p_valor = 0"


# ANOVA compara los dos modelos
resultado_anova <- anova(modelo_Intercepto, modelo_Completo)
print(resultado_anova)
#Analysis of Variance Table
#
#Model 1: Volume ~ 1
#Model 2: Volume ~ Girth + Height
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1     30 8106.1                                  
#2     28  421.9  2    7684.2 254.97 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# RESPUESTA 2)
#  ¿El modelo sin Girth y Height es mejor que el completo?
# RESPUESTA:  NO
# Tenemos un p_valor=0, lo cual indica que la reducción de SSE_C (421)
# contra SSE_R (8106) es estadísticamente significativa. En la comparación
# con anova, aparece como "< 2.2e-16". Se Rechaza contundentemente la hipótesis 
# nula. Es decir, existe evidencia estadística muy fuerte de que al menos uno 
# de los predictores (Girth o Height) está relacionado significativamente con 
# el Volumen.