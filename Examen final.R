library(readxl)
Problema1 <- read_excel("GitHub/estadistica-inferencia/DOE-UCuenca/Datos examen final.xlsx")
attach(Problema1)
Problema1$Clona <-  factor(Problema1$Clona)
# Anova
modelo.problema1 <- aov(Azucar_porce~ Clona, data = Problema1)
summary(modelo.problema1)
plot(modelo.problema1)

Problema2 <- read_excel("GitHub/estadistica-inferencia/DOE-UCuenca/Datos examen final.xlsx", 
                        sheet = "Hoja2")
attach(Problema2)

# Anova
modelo.problema2 <- lm(Sabor ~ Cuajo + Sal)
summary(modelo.problema2)
plot(modelo.problema2)
#LSD gráfica
library(agricolae)

## Warning: package &#39;agricolae&#39; was built under R version 4.3.2
plot(LSD.test(modelo.problema1, "Clona", alpha = 0.05))
