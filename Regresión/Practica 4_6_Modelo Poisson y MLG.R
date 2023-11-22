#REGRESION DE POISSON Y MODELOS LINEALES GENERALIZADOS
#MODELOS DE POISSON

#Ejemplo 1. Datos de atropellos
#--- Lectura de los datos
datos11=read.table("RoadKills.txt",header=TRUE)
attach(datos11)

#--- Diagrama de dispersi?n
plot(TOT.N~D.PARK,xlab="Distancia al parque",ylab="Atropellos")
#la variable dependiente es el total de atropellos (DEBE SER UN NUMERO ENTERO POSITIVO)
#la variable x es la distancia al parque, vemos que en este modelo no se cumple
#la homocedasticidad

#--- Modelo lineal
mod_lineal=lm(TOT.N~D.PARK)
summary(mod_lineal)
windows()
plot(TOT.N~D.PARK,main="Ajuste lineal")
abline(mod_lineal)
#vemos que el modelo lineal no se ajusta bien


#--- Modelo de Poisson
mod_poisson=glm(TOT.N~D.PARK,family=poisson(link=log))
#observe que ahora la familia ahora es poisson, funcion link =log (parecido al modelo binomial)
summary(mod_poisson)
exp(coef(mod_poisson))
#igualmente los coeficientes se deben calcular con exponenciales

exp(1000*coef(mod_poisson))
windows()
plot(TOT.N~D.PARK,main="Ajuste del modelo de Poisson")
beta=coef(mod_poisson)
curve(exp(beta[1]+beta[2]*x),add=TRUE,lwd=2,from=0,to=25000)
abline(v=0,lty=2)
abline(h=0,lty=2)
#vemos que el modelo de poisson se ajusta mejor a los datos.


#--- Intervalos de confianza para los coeficientes basados en Profile Likelihood
int_beta=confint(mod_poisson)
int_expbeta=exp(1000*int_beta)
int_beta
int_expbeta


#--- Contraste de modelos
mod_poisson2=glm(TOT.N~D.PARK+OPEN.L,family=poisson(link=log))
summary(mod_poisson2)
anova(mod_poisson,mod_poisson2,test="Chi")

#--- Diagnosis del modelo de Poisson
windows()
par(mfrow=c(2,2))
plot(mod_poisson)
par(mfrow=c(1,1))
lp=predict(mod_poisson,type="link")
mu=predict(mod_poisson,type="response")

#aqui se calculan todos los diferentes residuos para este modelo
res=residuals(mod_poisson,type="response")
pearson=residuals(mod_poisson,type="pearson")
dev=residuals(mod_poisson,type="deviance")

#aqui hacemos los gr?ficos de residuos
windows()
par(mfrow=c(2,2))
plot(mu,res,main="Response residuals")
plot(mu,pearson,main="Pearson residuals")
plot(mu,dev,main="Deviance residuals")
plot(lp,dev,main="Dev. residuals vs. linear predictors")
par(mfrow=c(1,1))
#se observa que existe una sobredispersion de residuos (Rango va de -5 a 5 en los residuos
#estandizados). Por tanto, el modelo no es Poisson (esto porque se asumi? que media=varianza)

windows()
par(mfrow=c(2,2))
plot(mod_poisson)
par(mfrow=c(1,1))

#probamos otros modelos para corregir la sobredesviacion
#--- Modelo Binomial Negativa
library(MASS)
mod_NB2=glm.nb(TOT.N~D.PARK+OPEN.L,link=log)
#aqui se parametriza el modelo binomal negativo
summary(mod_NB2)
windows()
par(mfrow=c(2,2))
plot(mod_NB2)
par(mfrow=c(1,1))

#se puede comprobar la sobredispersion entre dos modelos con el siguiente paquete
# Contraste de la sobre-dispersion
library(lmtest)
lrtest(mod_poisson,mod_poisson2) # Coincide con el anova entre modelos glm
lrtest(mod_poisson2,mod_NB2)
#se observa que entre ambos modelos, preferimos el modelo binomial negativo

#--- Modelo Quasi-Poisson
mod_quasipoisson2=glm(TOT.N~D.PARK+OPEN.L,family=quasipoisson(link=log))
summary(mod_quasipoisson2)
windows()
par(mfrow=c(2,2))
plot(mod_quasipoisson2)
par(mfrow=c(1,1))