#sesion 6 - MRLM

#cargamos los datos
library(faraway)
data(savings) # ES UNA BASE DE MUESTRa, indica los % de población y la renta per-capita
attach(savings)
head(savings)

#hacemos el modelo de regresion lineal multiple Y(sr)=X1(pop15)+X2(pop75)+X3(dpi)+X4(ddpi)
#Y=Bo + B1X1+B2X2+B3X3+B4X4
modelo<-lm(sr~pop15+pop75+dpi+ddpi)
summary(modelo)
AIC(modelo)
modelo1<-lm(sr~pop15+dpi+ddpi)
summary(modelo1)
AIC(modelo1)
BIC(modelo, modelo1)
#Estimaciones de los beta
names(modelo)
modelo$coefficients # es el vector beta
class(modelo)#esta es la clase del objeto "modelo" . todas las propiedades de "lm" son aplicables

betahat<-coef(modelo) #vector de parametros betas
betahat
#matriz de diseno: X
X<-model.matrix(modelo) #para expresar el modelo de forma matricial
head(X) # esta es la matriz de diséño la X que tiene 50 x 5 de tamaño
n = nrow(X) # tamano muestral
p = ncol(X) # numero de coeficientes

#Calculo de ajustes por matrices, son las proyecciones
yhat<-X%*%betahat #calculado por matrices es el producto matricial
yfit<-modelo$fitted.values
head(yhat)
head(yfit)#son los mismos que el yhat, que es el calculo por matrices y esto es por el modelo lm

#calculo de estimadores por matrices
y <- sr
betas <- solve(t(X)%*%X)%*%t(X)%*%y
betas
modelo$coefficients

# Matriz Hat y proyecciones
H<-X%*%solve(t(X)%*%X)%*%t(X) # matriz de proyección HAT
ajustes<-H%*%y # son las proyecciones de Y
head(ajustes)
head(modelo$fitted.values)

# Matriz generadora de residuos M
M = diag(n) - H #la matriz M, la matriz identidad es diag(n)
E = M%*%y
head(E)
head(modelo$residuals)

# Suma cuadrada residual a partir de M
RSS = t(y)%*%M%*%y
RSS
deviance(modelo) # saca los RSS la fórmula deviance

# estimacion de varianza del error
sigma2 = RSS/(n-p)
sigma2
sqrt(sigma2)

