# Diseño factorial 2^3

# Cargar los datos
library(readxl)
datos.2k<- read_excel("GitHub/estadistica-inferencia/Estadistica-inferencia/sept 23 - feb 24/Repaso2K.xlsx")
attach(datos.2k)
names(datos.2k)
library(FrF2)
Tabla <-  FrF2(nruns = 8, 
               nfactors =3,
               factor.names = list(A= c(-1,1),
                                   B= c(-1,1),
                                   C= c(-1,1)),
                                   
               replications = 1, randomize = F)
Tabla <-  add.response(design = Tabla, response= datos.2k$`(Y)`)
# Hacer factores 
A= factor(A)
B= factor(B)
C= factor(C)

# modelo factorial

modelo.2k3 <-  lm(datos.2k$`(Y)` ~ (A+B+C)^3) # el símbolo de ^ no significa potencia, si no que la fórmula de lm va a tomarle un nivel 3