#SESION 9: PREDICCION EN MRLM

#cargamos los datos
library(faraway)
data(savings)
attach(savings)
modelo<-lm(sr~pop15+pop75+dpi+ddpi)
summary(modelo)

#PREDICCION DE SR: 
predict(modelo) # estos son los ajustes
modelo$fitted.values # coinciden con los anteriores

#Prediccion puntual con: pop15=30, pop75=2, dpi=1000, ddpi=5
predict(modelo,data.frame(pop15=30,pop75=2,dpi=1000,ddpi=5))

#prediccion de varios puntos
predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),
                          dpi=c(1000,500),ddpi=c(5,4)))

#intervalos de prediccion para la media 
predict(modelo,interval="confidence") # para la media

# intervalo de predicción para la media en nuevos puntos
predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),
                          dpi=c(1000,500),ddpi=c(5,4)),
                          interval="confidence")

      
#intervalo de prediccion puntual
predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),
                          dpi=c(1000,500),ddpi=c(5,4)),
                          interval="prediction")
  