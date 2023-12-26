# El data set empleado es el state.x77
# Para facilitar su interpretación se renombra y se modifica
library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)

step(object = modelo, direction = "both", trace = 1)
# Regresión con una variable cualitativa
datos <- data.frame(peso = c(800, 950, 1050, 350, 750, 600, 1075, 250, 700,
                             650, 975, 350, 950, 425, 725),
                    volumen = c(885, 1016, 1125, 239, 701, 641, 1228, 412, 953,
                                929, 1492, 419, 1010, 595, 1034),
                    tipo_tapas = c("duras", "duras", "duras", "duras", "duras", 
                                   "duras", "duras", "blandas", "blandas",
                                   "blandas", "blandas", "blandas", "blandas",
                                   "blandas", "blandas"))
head(datos, 4)
datos$tipo_tapas <- as.factor(datos$tipo_tapas)
pairs(x = datos)
cor.test(datos$peso, datos$volumen, method = "pearson")
library(ggplot2)
ggplot(data = datos, mapping=aes(x = tipo_tapas, y = peso, color=tipo_tapas)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
modelo <- lm(peso ~ volumen + tipo_tapas, data = datos)
summary(modelo)

 # Se coloca la prueba de DW test 

library(car)
dwt(modelo,alternative = "two.sided")
