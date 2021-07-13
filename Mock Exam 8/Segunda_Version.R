#Para correr el código más facilmente, establezca su directorio de trabajo como
#la carpeta Mock_P8. Lo puede hacer así:
#Session->Set Working directory->Choose directory y elija 'abrir' en dicha carpeta

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-PREPARATIVOS.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#comenzamos cargando las librerías
paqueterias_a_usar<-c("readr","hms","moments","ggplot2",'MASS')
lapply(paqueterias_a_usar, require, character.only = TRUE)
#Descargamos los datos de BBVA.MC, cuyos precios están dados en euros, de los 
#últimos 5 años.
dirty_data<-read.csv('BBVA.MC.csv')
#vamos a hacer una variable con el nombre del activo que usaremos en las gráficos
activo='BBVA.MC'
#Nos quedamos solo con date y close, que son las fechas y el precio de cierre
datos<-dirty_data[c('Date','Close')]
#Quitamos los na y null
nulls<-which((dirty_data$Close)=='null')
#En nuestro caso, hay un valor null en la entrada 563, removemos
datos<-datos[-c(nulls),]
datos<-na.omit(datos)
#reseteamos índices
row.names(datos)<-NULL
#Esta etiqueta servirá para los gráficos
etiqueta <- c("BBVA.MC","Precio en Euros","Porcentaje","Rendimientos logarítmicos")
#Pasamos a tipo fecha la columna de Date. Viendo nuestros datos vemos que 
#están en formato YYYY-mm-dd
datos$Date<-as.Date(datos$Date,format='%Y-%m-%d')
#Pasamos los datos de cierre a numéricos
datos$Close<-as.numeric(datos$Close)
#Visualizamos los datos históricos. Estos son los precios aún
par(cex=0.7,mfrow=c(1,1)) 
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date,y=datos$Close,col="red",type="l",ylab=etiqueta[2],xlab= "",
     lwd = 1, font = 1, font.lab = 1,main = etiqueta[1])

#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS DIARIOS.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Calculamos los rendimientos logarítmicos DIARIOS. Se va a reciclar este nombre
#así que lo guardamos en la siguiente variable
Rdatos<-diff(log(datos$Close),lag = 1)*100
R_Diarios<-Rdatos
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la segunda fecha disponible ya que, para la primera fecha no tenemos
#manera de calcular su rendimiento lagarítmico al ser nuestro punto de inicio
#df será el nombre genérico para reciclar el código, pero se guardará en otra
#variable
df<-data.frame(x=datos$Date[2:nrow(datos)],y=Rdatos)
df_Diario<-df
#Graficamos los rendimientos logarítmicos
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date[2:nrow(datos)],y=Rdatos,col="blue",type="l",
     ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,
     main = paste(etiqueta[4],'Diarios',sep=' '),cex.main=1)
#Creamo un vector con los nombres de las medidas
medidas<-c('Media','Desviación estándar', 'Skewness', 'Kurtosis')
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Diarios<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Diarios[1,1]<-round(mean(Rdatos),5)
#Desviación estándar
Rendimientos_Diarios[1,2]<-round(sd(Rdatos),5)
#Skewness
Rendimientos_Diarios[1,3]<-round(skewness(Rdatos),5)
#Kurtosis
Rendimientos_Diarios[1,4]<-round(kurtosis(Rdatos)+3,5)
#Visualizamos
Rendimientos_Diarios<-as.data.frame(Rendimientos_Diarios)
#Le damos el nombre
colnames(Rendimientos_Diarios)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Diarios
par(mfrow=c(1,1)) 
Ker<-density(Rdatos)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(Rdatos, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlim =c(-3,3),ylim = c(0,0.9),xlab ="Rendimientos logarítmicos Diarios",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(Rdatos),sd=sd(Rdatos)), from=-10, to=10, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos diarios
par(mfrow=c(1,1))
qqnorm(Rdatos,ylim = c(-7,7),main=activo)
qqline(Rdatos,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos diarios y 
#comparamos con el de los rendimientos logarítmicos absolutos diario
par(mfrow=c(2,1))
acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Diarios")
acf(abs(Rdatos), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Diarios")


#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS SEMANALES.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos semanales. Guardamos al igual que en diarios
Rdatos<-diff(log(datos$Close),lag = 5)*100
R_semanales<-Rdatos
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 5 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el sexto día podemos comparar.
#Guardamos de la misma manera que en diarios
df<-data.frame(x=datos$Date[6:nrow(datos)],y=Rdatos)
df_semanales<-df
#Graficamos los rendimientos logarítmicos
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date[6:nrow(datos)],y=Rdatos,col="blue",type="l",
     ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,
     main = paste(etiqueta[4],'Semanales',sep=' '),cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Semanales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Semanales[1,1]<-round(mean(Rdatos),5)
#Desviación estándar
Rendimientos_Semanales[1,2]<-round(sd(Rdatos),5)
#Skewness
Rendimientos_Semanales[1,3]<-round(skewness(Rdatos),5)
#Kurtosis
Rendimientos_Semanales[1,4]<-round(kurtosis(Rdatos)+3,5)
#Visualizamos
Rendimientos_Semanales<-as.data.frame(Rendimientos_Semanales)
#Le damos el nombre
colnames(Rendimientos_Semanales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Semanales
par(mfrow=c(1,1)) 
Ker<-density(Rdatos)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(Rdatos, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlim =c(-3,3),ylim = c(0,0.9),xlab ="Rendimientos logarítmicos Semanales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(Rdatos),sd=sd(Rdatos)), from=-10, to=10, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos semanales
par(mfrow=c(1,1))
qqnorm(Rdatos,ylim = c(-7,7),main=activo)
qqline(Rdatos,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos semananales y 
#comparamos con el de los rendimientos logarítmicos absolutos semanales
par(mfrow=c(2,1))
acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Semanales")
acf(abs(Rdatos), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Semanales")





#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS MENSUALES-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos mensuales. Guardamos al igual que en diarios
Rdatos<-diff(log(datos$Close),lag = 21)*100
R_mensaules<-Rdatos
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 21 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el 22vo día podemos comparar.
#Guardamos de la misma manera que en diarios
df<-data.frame(x=datos$Date[22:nrow(datos)],y=Rdatos)
df_mensuales<-df
#Graficamos los rendimientos logarítmicos
plot(x=datos$Date[22:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",
     lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos mensuales",
     cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Mensuales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Mensuales[1,1]<-round(mean(Rdatos),5)
#Desviación estándar
Rendimientos_Mensuales[1,2]<-round(sd(Rdatos),5)
#Skewness
Rendimientos_Mensuales[1,3]<-round(skewness(Rdatos),5)
#Kurtosis
Rendimientos_Mensuales[1,4]<-round(kurtosis(Rdatos)+3,5)
#Visualizamos
Rendimientos_Mensuales<-as.data.frame(Rendimientos_Mensuales)
#Le damos el nombre
colnames(Rendimientos_Mensuales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Mensuales
par(mfrow=c(1,1)) 
Ker<-density(Rdatos)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(Rdatos, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlim =c(-3,3),ylim = c(0,0.9),xlab ="Rendimientos logarítmicos Mensuales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(Rdatos),sd=sd(Rdatos)), from=-10, to=10, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos mensuales
par(mfrow=c(1,1))
qqnorm(Rdatos,ylim = c(-7,7),main=activo)
qqline(Rdatos,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos mensuales y 
#comparamos con el de los rendimientos logarítmicos absolutos mensuales
par(mfrow=c(2,1))
acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Mensuales")
acf(abs(Rdatos), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Mensuales")






#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS ANUALES.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos mensuales. Guardamos al igual que en diarios
Rdatos<-diff(log(datos$Close),lag = 360)*100
R_anules<-Rdatos
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 360 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el 361º día podemos comparar.
#Guardamos de la misma manera que en diarios
df<-data.frame(x=datos$Date[361:nrow(datos)],y=Rdatos)
df_mensuales<-df
#Graficamos los rendimientos logarítmicos
plot(x=datos$Date[361:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],
     xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos anuales",
     cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Mensuales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Mensuales[1,1]<-round(mean(Rdatos),5)
#Desviación estándar
Rendimientos_Mensuales[1,2]<-round(sd(Rdatos),5)
#Skewness
Rendimientos_Mensuales[1,3]<-round(skewness(Rdatos),5)
#Kurtosis
Rendimientos_Mensuales[1,4]<-round(kurtosis(Rdatos)+3,5)
#Visualizamos
Rendimientos_Mensuales<-as.data.frame(Rendimientos_Mensuales)
#Le damos el nombre
colnames(Rendimientos_Mensuales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Mensuales
par(mfrow=c(1,1)) 
Ker<-density(Rdatos)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(Rdatos, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlim =c(-3,3),ylim = c(0,0.9),xlab ="Rendimientos logarítmicos Anuales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(Rdatos),sd=sd(Rdatos)), from=-10, to=10, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos anuales
par(mfrow=c(1,1))
qqnorm(Rdatos,ylim = c(-7,7),main=activo)
qqline(Rdatos,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos anuales y 
#comparamos con el de los rendimientos logarítmicos absolutos anuales
par(mfrow=c(2,1))
acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Anuales")
acf(abs(Rdatos), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Anuales")







#.-.-.-.-.-.-.-.-.-.-.-.-.-.-Medidas de Riesgo.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-PARAMÉTRICO T-STUDENT 95%.-.-.-.-.-.-.-.-.-.-.-.-.-
