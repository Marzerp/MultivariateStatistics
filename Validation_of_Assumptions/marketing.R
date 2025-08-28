library(datarium)
data("marketing")

modelo1<-lm(sales~youtube+facebook+newspaper,data=marketing)
summary(modelo1)

# Normalidad con Shapiro

# H_0 = la distribucion es normal
# H_1 = no son normales

residuales = modelo1$residuals
qqnorm(residuales)
qqline(residuales)
plot(modelo1)

shapiro.test(residuales)

# W = 0.91767, p-value = 3.939e-09
# Como p-value es mucho menor a 0.05 no son normales 


#----------------------------------------

# Quitando la variable 'newspaper'

modelo2<-lm(sales~youtube+facebook,data=marketing)
summary(modelo2)

# Normalidad con Shapiro

# H_0 = la distribucion es normal
# H_1 = no son normales

residuales = modelo2$residuals
qqnorm(residuales)
qqline(residuales)
plot(modelo2)

shapiro.test(residuales)

#W = 0.91804, p-value = 4.19e-09
# Como p-value es mucho menor a 0.05 no son normales 
