# Cargar datos
library(readxl)
Datos_2k <- read_excel("GitHub/estadistica-inferencia/DOE-UCuenca/dATOS-2k EJEMPLO VELOCIDAD PROFU.xlsx")
# Convertir en factor
Datos_2k$Profundidad <-  as.factor(Datos_2k$Profundidad)
Datos_2k$Velocidad <- as.factor(Datos_2k$Velocidad)

# Modelo ANOVA


