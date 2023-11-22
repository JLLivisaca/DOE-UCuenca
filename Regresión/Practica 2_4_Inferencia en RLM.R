# INFERENCIAS EN RLM 

# INTERVALOS DE CONFIANZA
#cargamos los datos
library(faraway)
data(savings)
attach(savings)
head(savings)

#hacemos el modelo de regresion lineal multiple
modelo<-lm(sr~pop15+pop75+dpi+ddpi)
summary(modelo)

#Estimaciones de los beta
modelo$coefficients

betahat<-coef(modelo) #vector de parametros betas

# matriz de diseno: X
X<-model.matrix(modelo) #para expresar el modelo de forma matricial
n = nrow(X)
p = ncol(X)

# estimación de sigma
sigma2 = deviance(modelo)/(n-p)
sigma2

# matriz de covarianza de los betas
cov_beta = sigma2 * solve(t(X)%*%X)
cov_beta

# errores tipicos de los betas
ET = sqrt(diag(cov_beta))

# IC para los betas (al 95% - 0.975 prob acumulada cola derecha)
t = qt(0.975, n-p)
betainf = betahat - t*ET
betasup = betahat + t*ET
betainf; betasup

# intervalos de confianza para betas (objeto "lm")
confint(modelo) #vemos que hay variables que se anulan 

# Representacion de los intervalos de confianza (beta1 y beta2)
library(ellipse)
plot(ellipse(modelo, c(2,3)), type ="l", 
     xlim = c(-1,0), main = "region de confianza") # IC conjuntos
points(betahat[2], betahat[3], pch=16, col=4) # Betas estimados
points(0,0, pch=16, col=2) # se verifica si ambos se anulan 
# Punto (0,0) fuera del IC. No se anulan de forma conjunta



# TEST F - CONTRASTE DE DESCOMPOSICION DE VARIABILIDAD

#hacemos un nuevo modelo sin "dpi"
modelo2<-lm(sr~pop15+pop75+ddpi)
summary(modelo2)
#vemos que cambia todos los valores por tanto, hay que 
# analizar cuando queremos eliminar varios Xi del modelo

#PODEMOS ELIMINAR DOS VARIABLES A LA VEZ? 
# DEPENDE DE TEST F 
modelo_sim <- lm(sr~pop15+ddpi) #eliminados pop75 y dpi

#se hace la prueba anova, verificar los g.l
anova(modelo_sim,modelo) #primero va el modelo simplificado,

#veamos las RSS
deviance(modelo)
deviance(modelo_sim)
#como Valor P es mayor al 5%, entonces se acepta Ho 
# (se puede eliminar las variables)