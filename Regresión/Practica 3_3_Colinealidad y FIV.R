# COLINEALIDAD y FACTOR DE INFLACION DE VARIANZA

#--- Lectura de los datos
bridge <- read_table2("C:/Users/Juan Llivisaca/OneDrive/Escritorio/Maestria Estadística/3 Modelos Lineales/Bases de datos/bridge.txt")


attach(bridge)

#--- Modelo de regresion multiple, pero con logaritmo porque son positivas
m73=lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(m73)
# los valores negativos me da una posible evidencia que existe colinealidad
# esos betas son betas parciales, al eliminar la variable
#--- Diagrama de dispersi?n sobre los valores ajustes (con recta)
plot(log(Time)~fitted(m73),xlab="Valores ajustados")
abline(lm(log(Time)~fitted(m73)))
# este grafico muestra un buen ajuste en los valores, está bastante bien
#--- Residuos estandarizados frente a cada variable explicativa
#--- y frente a los valores ajustados
par(mfrow=c(2,3))
res_est=rstandard(m73)
plot(res_est~log(DArea),ylab="Residuos estandarizados")
plot(res_est~log(CCost),ylab="Residuos estandarizados")
plot(res_est~log(Dwgs),ylab="Residuos estandarizados")
plot(res_est~log(Length),ylab="Residuos estandarizados")
plot(res_est~log(Spans),ylab="Residuos estandarizados")
plot(res_est~fitted(m73),xlab="Valores ajustados",ylab="Residuos estandarizados")
par(mfrow=c(1,1))
# no se ve patrones todos estan en la banda 2 y -2, por qué se anularon los betas?

#HAY QUE CONSIDERAR QUE HAY GRAFICOS DONDE SE PRESENTAN DATOS DE FORMA VERTICAL, ES PORQUE
#LA VARIABLE ES DE CONTEO (1 ARCO, 2 ARCOS, ETC.. Y ESTAN EXPRESADAS POR SU LOGARITMO)
#ESTO NO INFLUYE EN SU ANALISIS

# CRITERIOS GLOBALES (FIV)

#--- Matriz de correlaciones entre las variables explicativas
x=cbind(log(DArea),log(CCost),log(Dwgs),log(Length),log(Spans))
colnames(x)=c("logDArea","logCCost","logDwgs","logLength","logSpans")
round(cor(x),3)

#--- Factores de inflacion de la varianza
VIF=c()
VIF["logDArea"]=1 / ( 1 - cor( log(DArea),
                               fitted(lm(log(DArea)~log(CCost)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
VIF["logCCost"]=1 / ( 1 - cor( log(CCost),
                               fitted(lm(log(CCost)~log(DArea)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
VIF["logDwgs"]=1 / ( 1 - cor( log(Dwgs),
                              fitted(lm(log(Dwgs)~log(DArea)+log(CCost)+log(Length)+log(Spans)) ) )^2 )
VIF["logLength"]=1 / ( 1 - cor( log(Length),
                                fitted(lm(log(Length)~log(DArea)+log(CCost)+log(Dwgs)+log(Spans)) ) )^2 )
VIF["logSpans"]=1 / ( 1 - cor( log(Spans),
                               fitted(lm(log(Spans)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)) ) )^2 )
VIF

library(car) # Con la funci?n vif del paquete car
vif(m73)

modelo_sin_colinealidad=lm(log(Time)~ log(Dwgs)+ log(Spans))
summary(modelo_sin_colinealidad)
