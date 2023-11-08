# Tomado de: https://rpubs.com/ctellez_gdl/76102
# Se reconoce los derechos de autor.
# Los datos se han modificado

#### Universidad de Cuenca, Campus Balzay ####
####=======================================####

# Lectura de datos
library(readxl)
Diseno_2k <- read_excel("GitHub/estadistica-inferencia/DOE-UCuenca/dATOS-2k EJEMPLO VELOCIDAD PROFU.xlsx",
                        sheet = "2k sin replicas")
#("GitHub/estadistica-inferencia/DOE-UCuenca/dATOS-2k EJEMPLO VELOCIDAD PROFU.xlsx", 
  #sheet = "2k sin replicas")
attach(Diseno_2k)
names(Diseno_2k)
library(FrF2)
Tabla <- FrF2(nruns = 4, 
              nfactors = 2, 
              factor.names = list(Profundidad=c(-1,1), 
                                  Velocidad=c(-1,1)), 
                                   
              replications = 3, randomize = F)
Tabla

# Se agrega la respuesta
Tabla <- add.response(design = Tabla, response = Acabado)
Tabla

## Análisis de la tabla ANOVA
Profundidad <- factor(Profundidad)
velocidad <- factor(Velocidad)
#Velocidad <- factor(Velocidad)

Modelo <- lm(Acabado~(Profundidad + velocidad)^2)
ANOVA <- aov(Modelo)
summary(ANOVA)
# otra opición 
Modelo_1 <-  aov(Acbado~ Profundidad*velocidad)
summary(Modelo_1)

## Solamente son significativos los efectos principales

## Gráficas de efectos principales
MEPlot(Tabla, lwd = 2)
abline(h=0, col="red")

## Lo mejor es todos a nivel bajo, pero la velocidad impacta en
## la productividad por lo que hay que considerar

## Gráficas de Interacciones
IAPlot(Tabla, lwd = 2)

## Se observa que es posible aumentar la velocidad

# Gráfica de interacción triple
cubePlot(obj = Llenado, 
         eff1 = Carbonatacion, 
         eff2 = Presion, 
         eff3 = Velocidad, 
         main = " Gráfica de interacción triple")


# Define los niveles de tus factores
factor1_levels <- c("A", "B", "C", "D")
factor2_levels <- c("X", "Y", "Z")

# Genera todas las combinaciones de niveles
combinaciones <- expand.grid(Factor1 = factor1_levels, Factor2 = factor2_levels)

# Repite las combinaciones tres veces
replicaciones <- rep(1:3, each = nrow(combinaciones))

# Agrega las réplicas a las combinaciones
combinaciones$Replica <- replicaciones

# Muestra el arreglo factorial con réplicas
print(combinaciones)
