# Regresión 
x <- rnorm(100,0,1)
y <- rnorm(100,0,1)
plot(y ~ x) # diagrama de dispersión
data <- data.frame(x,y)
attach(data)
# Modelo de regresión 
mod <-  lm(y ~ x)
summary(mod)
anova(mod)
