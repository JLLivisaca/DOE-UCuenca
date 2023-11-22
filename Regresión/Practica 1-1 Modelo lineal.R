# Simulacion de RLS

# Modelo teórico Y = 1 + 0.5 X + error
beta0 <-1
beta1 <-0.5
n <-100 #la muestra es de diseño fijo de tamano 100
x <- seq(0,1,length=n) 
mx <- beta0 + beta1*x
plot(mx ~ x, type = "l") 

# generamos errores aleatorios normales con media 0 y desviacion 0.1
err <- rnorm(n, sd=0.1)
hist(err)

# combinamos el error al modelo teórico
y <- mx + err

# Graficos
plot(y ~ x, type = "p") 
points(x, mx, col =2, type = "l")

# calculamos la covarianza entre X y Y
cov(x,y) #positiva

# Estimamos la regresión lineal simple por MCO
beta1_est <- cov(x,y) / var(x)
beta0_est <- mean(y) - beta1_est * mean(x)
beta0_est
beta1_est

Proy_y <- beta0_est + beta1_est * x
head(Proy_y)

plot(y ~ x, type = "p") 
points(x, mx, col =2, type = "l")
points(x, Proy_y, col = 4, type = "p", cex = 0.5)

# Calculo de residuos y estimacion de sigma cuadrada
resid <- y - Proy_y

mean(resid)
hist(resid)

sum_MCO <- sum(resid^2)
sum_MCO

sigma2 <- sum(resid^2)/(n-2)
sigma2
sqrt(sigma2) # se acerca a la desviación planteada
# Ajustemos otra recta cualquiera: m(x): 0.99 + 0.51 X
y2 <- 0.99 + 0.51*x

plot(y ~ x, type = "p") 
points(x, y2, col =2, type = "l")
points(x, Proy_y, col = 4, type = "p", cex = 0.5)

resid2 <- y - y2

mean(resid2)  
sum(resid2)^2
sum_MCO

# Estimacion MCO usando funcion "lm"
model <- lm(y ~ x)
model$coefficients

plot(y ~ x, type = "p") 
points(x, mx, col = 2, type = "l")
abline(model, col = 4)

head(model$fitted.values) # proyecciones, puntos por donde pasa la recta
head(model$residuals) # residuos
