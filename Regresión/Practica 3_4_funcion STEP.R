# Función STEP
?step

#--- Lectura de los datos
bridge=read.table("bridge.txt",header=TRUE)
attach(bridge)

#--- Selecci?n de variables mediante un algoritmo stepwise (por pasos)
m73=lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))

# metodo backward
step(m73, direction = "backward")

# metodo forward
step(m73, direction = "forward")

# metodo both
step(m73, direction = "both")

m73r=step(m73)#se guarda el modelo con la funcion STEP

