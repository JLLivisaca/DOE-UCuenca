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
Anova(AnovaModel.1)
TukeyHSD(AnovaModel.1)
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
