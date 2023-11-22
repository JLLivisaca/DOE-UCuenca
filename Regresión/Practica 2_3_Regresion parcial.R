#REGRESION PARTICIONADA Y PARCIAL

# Ejemplo Greene 1999
datos<-read.table("Greene.txt",header=TRUE)
head(datos)
attach(datos)

#conversion de los datos teniendo como base al IPC
datos_real<-datos
names(datos_real) <- c("YEAR","PNB","IN","IPC","TI")
datos_real$YEAR=1:15
datos_real$PNB=(datos$PNB/datos$IPC)*100
datos_real$IN=(datos$IN/datos$IPC)*100
datos_real$IPC[1]=(datos$IPC[1]/79.06-1)*100 
datos_real$IPC[2:15]=(datos$IPC[2:15]/datos$IPC[1:14]-1)*100
attach(datos_real)

#hacemos el modelo
#las variables son: IN=inversion anual, 
modelo<-lm(IN ~ YEAR+PNB+IPC+TI)
summary(modelo)
# El beta de YEAR es NEGATIVO... que significa?? 
# FENOMENO DE CONFUSION

#Hagamos la regresion solo sobre YEAR
m1<-lm(IN~YEAR)
summary(m1)
#aqui vemos que el coeficiente respecto a YEAR es positivo????
#CONCLUSION: en ceteris paribus (con el resto de variables), 
# el aumento de YEAR no aumentara la inversion.(por eso es negativa)

#veamos que sucede graficamente en la Regresion Simple
plot(IN~YEAR); abline(m1)

#coeficiente por regresion particionada
Yast<-lm(IN ~PNB+IPC+TI)$residuals  #este es Y* (ver notas)
Xast<-lm(YEAR ~PNB+IPC+TI)$residuals #este es X* (ver notas)
plot(Yast~Xast) #vemos que la relacion es negativa
mast<-lm(Yast~Xast)
summary(mast)
plot(Yast~Xast);abline(mast) #GRAFICO DE LA VARIABLE ANADIDA

#ANALISIS DE LOS COEFICIENTES DE CORRELACION
#coeficiente de correlacion lineal simple

summary(m1)
sqrt(0.5609) #es la correlacion entre YEAR e Inversion (es positiva) 
# Este es la verdadera relacion entre YEAR e Inversion... 
# (la de la MRLG es en ceteris paribus)

# Coeficiente de correlacion linel multiple
names(modelo)
cor(IN,modelo$fitted.value)^2 
#es la correlacion entre la variable IN y los ajustes
# del modelo de regresion multiple . 
# SIRVE COMO MEDIDA DE BONDAD DE AJUSTE
summary(modelo) #comparamos y vemos que se obtiene el mismo.

# Coeficientes de correlacion parcial
cor(Yast,Xast) #Se obtiene de las regresiones particionadas. 
# Sirve para dar el signo obtenido en los betas del MRLG
