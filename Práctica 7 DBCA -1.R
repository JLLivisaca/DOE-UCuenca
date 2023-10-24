# Práctica DBCA
library(readxl)

Operador <- read_excel("Operador.xlsx")

# Convertir los valores del operador a factores
Operador <- within(Operador, {
  Operador <- factor(Operador, labels=c('Ope_1','Ope_2','Ope_3','Ope_4'))
})

Metodo <- as.factor(Operador$Metodo)

# Modelo DBCA
AnovaModel.1 <- (aov(Tiempo ~ Metodo+Operador, data=Operador))
summary(AnovaModel.1)
TukeyHSD(AnovaModel.1)
# Comparación de diferencia de medias con otros métodos
library(asbio)
#(pairw.anova(x=Line, y=Fecundity, conf.level = 0.95, method="tukey")) # COmparación con Tukey
(pairw.anova(x=Operador$Operador, y=Operador$Tiempo, conf.level = 0.95, method="lsd")) # COmparación con LSD
(pairw.anova(x=Operador$Operador, y=Operador$Tiempo, conf.level = 0.99, method="scheffe")) # COmparación con Sheffe


old.oma <- par(oma=c(0,5,0,0))
.Pairs <- glht(AnovaModel.1, linfct = mcp(Metodo = "Tukey"))
confint(.Pairs) # confidence intervals
mtext('Metodo')
plot(confint(.Pairs))
.Pairs <- glht(AnovaModel.1, linfct = mcp(Operador = "Tukey"))
confint(.Pairs) # confidence intervals
plot(confint(.Pairs))
par(old.oma)
remove(.Pairs)
