#### Problema 1 ####
recorrido <-  factor(rep(c("A", "B"),each=5))
ruta <-  data.frame(recorrido)
ruta$tiempo_ruta <- NA
ruta$tiempo_ruta[ruta$recorrido=="A"] <- c(18,24,30,21,32)
ruta$tiempo_ruta[ruta$recorrido=="B"] <- c(18,24,30,21,32)
t.test(Tiempo_ruta~Recorrido, alternative='two.sided', conf.level=.95, 
       var.equal=FALSE, data=ruta)

attach(ruta)
library(readxl)
Rutas <- read_excel("D:/Diseño de experimentos/Alumnos/Sep 23 - Feb 24/Rutas.xlsx")
attach(Rutas)
# Prueba T
t.test(Tiempo_ruta~Recorrido, alternative='two.sided', conf.level=.95, 
       var.equal=FALSE, data=Rutas)

######################
#### RESPUESTAS ####
######################
# Literal a) No existen diferencias en las rutas
# Literal b) 
  boxplot(Tiempo_ruta ~ Recorrido)
  #La mejor Ruta es la A

# Literal c) Se pueden escoger los datos usando un anova, colocando a los
  # choferes como bloques.
 

#### Problema 2 ####

library(readxl)
Teach <- read_excel("D:/Diseño de experimentos/Alumnos/Sep 23 - Feb 24/Teach.xlsx")
attach(Teach)
names(Teach)
Teach$Metodo <- as.factor(Teach$Metodo)
Teach$Calificación <- as.factor(Teach$Calificación)
# Modelo ANOVA
fit <- aov(Nota~ Calificación + Metodo, data=Teach)
summary(fit)
fit <- aov(Nota~ (Metodo + Calificación))
summary(fit)
######################
#### RESPUESTAS ####
######################
# Literal a) El modelo que se aplica es el de bloques, el bloque es el método
# Literal b) No hay diferencia en el método. Cuál es el mejor método 3, viendo gráficas, peor 
# peor método es el 1.
# Literal c) Si hay diferencia en las escalas colocadas.
# Literal d) 
  # CONTRASTAR NORMALIDAD
  shapiro.test(fit$residuals) #es el test de normalidad aplicado a los residuos
  # Se puede notar que p-value = 0.5944; los residuos si tienen normalidad

  # CONTRASTAR HOMOCEDASTICIDAD
  #Harrison-McCabe 
  library(lmtest)
  plot(fit$residuals~Teach$Nota);abline(h=0)
  hmctest(fit) #Test de homocedasticidad Harrison McCabe: 
  # Se puede el  p-value = 0.241 que si se tiene homocedasticidad

# Literal e)
  
# Numero de replicas
library(daewr)
rmin <-10 #smallest number of replicates considered
rmax <-50 # largest number of replicates considered
alpha <- rep(0.01, rmax - rmin +1)
sigma <-sqrt(5.13^2)
nlev <- 3 #the number of levels of the factor (tratamiento)
nreps <- rmin:rmax
Delta <- 4.5 # Variación significativa entre la diferencia de medias detectable
power <- Fpower1(alpha,nlev,nreps,Delta,sigma)
power


#### Problema 3 ####
library(readxl)
Caucho <- read_excel("D:/Diseño de experimentos/Alumnos/Sep 23 - Feb 24/Caucho.xlsx")
Caucho$Tiempo_Cura <-  as.factor(Caucho$Tiempo_Cura)
Caucho$Acelerante <-  as.factor(Caucho$Acelerante)
attach(Caucho)
factorial <- (aov(Resistencia ~ Acelerante*Tiempo_Cura, data=Caucho))
summary(factorial)

######################
#### RESPUESTAS ####
######################
# Literal a) El modelo es factorial de 3 X 3 con dos replicas
  # Y = miu + alfa + beta + (alfa * beta) + errores
# Literal b) 
   # Se colocan hipótesis.
# Literal c) No hay alguno que aumente la resistencia 


