# Librería necesaria
library(dplyr)

# Datos correctos del experimento con 4 niveles en cada factor
posicion <- rep(1:4, each=4)
carro <- rep(1:4, times=4)
llanta <- c("C", "B", "A", "D",
            "D", "C", "B", "A",
            "A", "D", "C", "B",
            "B", "A", "D", "C")
desgaste <- c(12, 14, 17, 13,
              11, 12, 14, 14,
              13, 11, 10, 13,
              8, 13, 9, 9)

# Crear el DataFrame
datos <- data.frame(posicion, carro, llanta, desgaste)

# Asegurarse de que los factores están bien definidos
datos$llanta <- as.factor(datos$llanta)
datos$carro <- as.factor(datos$carro)
datos$posicion <- as.factor(datos$posicion)

# Realizar el ANOVA para cuadro latino con 3 factores
anova_model <- aov(desgaste ~ llanta + carro + posicion, data = datos)

# Mostrar los resultados del ANOVA
summary(anova_model)
model.tables( anova_model, type = "means" )
library(daewr)
mod6 <- aov( AUC ~ Subject + Period + Treat, data = bioeqv)
summary(mod6)
