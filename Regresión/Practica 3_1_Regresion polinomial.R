# REGRESION POLINOMIAL

library(readr)
datos1 <- read_table2("C:/Users/Juan Llivisaca/OneDrive/Escritorio/Maestria Estadística/3 Modelos Lineales/Bases de datos/acaros.txt")
attach(datos1)
datos1=as.data.frame(datos1)
#Diagrama de dispersion
plot(datos1)

#Polinomio de grado cero
modelo0<-lm(datos1$acaros~1)
abline(modelo0,lwd=2,lty=2)

#Polinomio de grado uno
modelo1<-lm(datos1$acaros~humedad)
summary(modelo1)
abline(modelo1,lwd=2,col="blue")
#EL AJUSTE NO ES BUENO, EL BETA CERO SE ANULA

#Polinomio de grado dos
humedad2=humedad*humedad # es la variable al cuadrado
modelo2<-lm(datos1$acaros~humedad+humedad2)
modelo2<-lm(datos1$acaros~humedad+I(humedad^2)) # es la mejor forma esta 
# el colocar el valor de I y poner al cuadrado I(humedad^2)
summary(modelo2)
beta=coef(modelo2)
curve(beta[1]+beta[2]*x+beta[3]*x^2,add=TRUE,lwd=2,col="red")
#LOS BETAS SON SIGNIFICATIVOS, EL AJUSTE ES MEJOR
#OJO- LA FUNCION CURVE ES PARA GRAFICAR FUNCIONES GENERICAS

#PREDICCION PARA UNA HUMEDAD DEL 80%
#hay que fijarse en las escalas de las variables (% o escala de 1)
Ypred0<-beta[1]+beta[2]*80+beta[3]*(80)^2 # cuando uso la variable ficticia de la linea 21
Ypred0
predict(modelo2,data.frame(humedad=80)) # OJO:solo funciona porque usamos el comando I en la regresion

#Polinomio de grado tres
modelo3<-lm(datos1$acaros~humedad+I(humedad^2)+I(humedad^3))
summary(modelo3)
beta=coef(modelo3)
curve(beta[1]+beta[2]*x+beta[3]*x^2+beta[4]*x^3,add=TRUE,lwd=2,lty=2,col="green")
#CUANDO SE SOBREESTIMAN LOS PARAMETROS SE PIERDEN SIGNIFICACION DE LOS BETAS.
#OBSERVE QUE EL VALOR DE F ES SIGNIFICATIVO (el pe valor del modelo es menor a alfa), LO QUE INDICA QUE GLOBALMENTE ESTA BIEN
#PERO INDIVIDUALMENTE LOS BETAS SON NULOS
# esto es un sobreajuste

# pimer paso fijar el k=3
summary(modelo3) #sobreajuste lo elimino
summary(modelo2)# funciona mejor
#podría buscar algún modelo que se ajuste mejor, bajo el k
summary(modelo1) 
# como se ve en el modelo uno, se elimina un betha, y reduce la 
#bondad de ajuste

#conclusión se queda con el modelo 2, porque me conviene por ajuste y significancia
