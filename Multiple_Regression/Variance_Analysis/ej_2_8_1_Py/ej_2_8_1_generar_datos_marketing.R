library(datarium)
data("marketing")
str(marketing)

# Guardar como archivo CSV
write.csv(marketing, "marketing_dataset.csv", row.names = FALSE)

# Verificar que se guardó correctamente
print("Archivo guardado:")
print(list.files(pattern = "\\.csv$"))
print("Primeras filas del dataset:")
print(head(marketing))

modelo3<-lm(sales~facebook+youtube+newspaper,data=marketing)
modeloF<-lm(sales~facebook,data=marketing)
modeloY<-lm(sales~youtube,data=marketing)
modeloN<-lm(sales~newspaper,data=marketing)

r2_facebook <- summary(modeloF)$r.squared
r2_youtube <- summary(modeloY)$r.squared
r2_newspaper <- summary(modeloN)$r.squared
r2_completo <- summary(modelo3)$r.squared

cat("R² Facebook:", r2_facebook, "\n")
cat("R² YouTube:", r2_youtube, "\n")
cat("R² Newspaper:", r2_newspaper, "\n")
cat("R² completo:", r2_completo, "\n")

modeloFY<-lm(sales~facebook+youtube,data=marketing)
resultado_anova <- anova(modeloY, modeloFY,modelo3)
print(resultado_anova)
