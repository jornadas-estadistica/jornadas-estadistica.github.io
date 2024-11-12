
#Título: Predicción de Desembarcos de Pulpo (Octopus vulgaris) con Inteligencia Artificial: Una Demo Interactiva con R
#Autor: Dr. Víctor Sanz-Fernández
#Contacto: victor.sanz@pucv.cl
#III Jornadas de Estadística USACH


#Modelos NNAR (Neural Network Autorregresive Model, o Red Neuronal Autorregresiva) y proyecciones

#Es una red de feed-forward con capa oculta única. Los valores rezagados de las capturas se usan como inputs en la red.

#La notación que se emplea es NNAR(p,k), donde p indica el número de retardos (los inputs), y k el número de nodos 
#de la capa oculta única.

#En el caso de series estacionales, se tendría NNAR(p, P, k)s, donde como inputs estarían los retardos. P hace referencia
#al retardo estacional. p al retardo no estacional. s a la estacionalidad. 

#Se utiliza la función nnetar() del paquete forecast. Usa redes neuronales feed-forward con una capa oculta
#y los retardos de la serie univariante como inputs de la misma.

#Se realizan 20 repeticiones como número de redes a ajustar, con los diferentes pesos iniciales aleatorios.

#La función de activación empleada es la logística.

#El número de nodos de la capa oculta viene determinado por el parámetro size=(p+P+1)/2=k

#El número de parámetros h(k+1)+h+1. h número de nodos en la capa oculta. k es el número de nodos de inputs.

#Carga de paquetes
library(tseries)
library(forecast)
library(seasonal)
library(ggplot2)
library(Kendall)
library(modifiedmk) 
library(hydroGOF)
library(lmtest)
library(uroot)
library(cowplot)

#Modelos NNAR(p, P, k)s

#Serie Pulpo por Áreas Mensual (Toneladas)
Serie_Pulpo_Mensual <- read.csv("CAPTURAS TONELADAS PULPO 2000-2022 Oct Sept.csv", header = T)

Serie_Pulpo_Mensual$fMonth<- as.factor((rep(1:12,22)))
Serie_Pulpo_Mensual$fYear<- as.factor(Serie_Pulpo_Mensual$Year1)

#Creación series temporales
Atlantico <- ts(Serie_Pulpo_Mensual$Atlantico, start=1, frequency=12)
Mediterraneo <- ts(Serie_Pulpo_Mensual$Mediterraneo, start=1, frequency=12)
Serie_Pulpo_Mensual$Time<-time(Atlantico)


#Representación de las series temporales junto con su tendencia lineal

#Atlántico

tendencia_lineal_Atlantico  <- lm(Atlantico ~ I(Time), data =Serie_Pulpo_Mensual)
tendencia_lineal_Atlantico$coefficients
summary(tendencia_lineal_Atlantico)
confint(tendencia_lineal_Atlantico)

#Representación visual del modelo lineal

MyTL <- data.frame(Time = seq(from = 1,
                              to = 22.916667,
                              length = 200))

TL <- predict(tendencia_lineal_Atlantico, newdata = MyTL, se = TRUE, type = "response")

MyTL$Pred <- TL$fit                    #Valor predicho
MyTL$seup <- TL$fit + 1.96 * TL$se.fit #Valor superior del intervalo de confianza al 95%
MyTL$selo <- TL$fit - 1.96 * TL$se.fit #Valor inferior del intervalo de confianza al 95%

p1<-ggplot()+
  geom_line(data = Serie_Pulpo_Mensual, 
            aes(y = Atlantico, x = Time),
            linetype="solid", 
            size = 0.8)+
  theme_classic()+
  ggtitle("Atlántico")+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1.8),
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="",y="Desembarcos pulpo (toneladas)")+
  scale_y_continuous(breaks=seq(0, 2000, 500),limits =c(0, 2000))+
  scale_x_continuous(breaks=c(1,5,10,15,20),
                     labels=c("00-01","04-05","09-10","14-15","19-20"))+
  theme(text = element_text(size=15))+
  theme(legend.position="")+
  geom_line(data = MyTL, 
            aes(x = Time, y = (Pred)), 
            colour = "blue",
            size =1)+
  geom_ribbon(data = MyTL, 
              aes(x = Time, 
                  ymax = (seup), 
                  ymin = (selo)),alpha = 0.5,fill = "grey70",
              size =1)+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.5), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.5), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.8))+
  theme(axis.line.y=element_line(color="black", size=0.8))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.5),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.5),family="serif"))

#Mediterráneo

tendencia_lineal_Mediterraneo  <- lm(Mediterraneo ~ I(Time), data =Serie_Pulpo_Mensual)
tendencia_lineal_Mediterraneo$coefficients
summary(tendencia_lineal_Mediterraneo)
confint(tendencia_lineal_Mediterraneo)

#Representación visual del modelo lineal

MyTL <- data.frame(Time = seq(from = 1,
                              to = 22.916667,
                              length = 200))

TL <- predict(tendencia_lineal_Mediterraneo, newdata = MyTL, se = TRUE, type = "response")

MyTL$Pred <- TL$fit                    #Valor predicho
MyTL$seup <- TL$fit + 1.96 * TL$se.fit #Valor superior del intervalo de confianza al 95%
MyTL$selo <- TL$fit - 1.96 * TL$se.fit #Valor inferior del intervalo de confianza al 95%

p2<-ggplot()+
  geom_line(data = Serie_Pulpo_Mensual, 
            aes(y = Mediterraneo, x = Time),
            linetype="solid", 
            size = 0.8)+
  theme_classic()+
  ggtitle("Mediterráneo")+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1.5),
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="",y="")+
  scale_y_continuous(breaks=seq(0, 300, 50),limits =c(0, 300))+
  scale_x_continuous(breaks=c(1,5,10,15,20),
                     labels=c("00-01","04-05","09-10","14-15","19-20"))+
  theme(text = element_text(size=15))+
  theme(legend.position="")+
  geom_line(data = MyTL, 
            aes(x = Time, y = (Pred)), 
            colour = "blue")+
  geom_ribbon(data = MyTL, 
              aes(x = Time, 
                  ymax = (seup), 
                  ymin = (selo)),alpha = 0.5,fill = "grey70")+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.5), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.5), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.8))+
  theme(axis.line.y=element_line(color="black", size=0.8))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.5),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.5),family="serif"))


h<-plot_grid(p1,p2, ncol=2,nrow=1, labels = c("a)","b)"), align = 'v',
             label_size = 22,label_fontfamily = "serif", label_fontface = "bold", hjust = -0.5, vjust = 1.5 )

h

#Utilizamos el último (2022) para medir la capacidad de predicción del modelo

#Atlántico

#Set de entrenamiento (2000 a 1)
train <- window(Atlantico, end = c(21,12))

#Set de validación de la predicción (2022)
test<-as.data.frame(window(Atlantico, start = c(22,1)))
colnames(test)[1]<-"Datos"

#NNAR(4,2,6)
set.seed(2015)

fit <- nnetar(train, p=4, P=2, size = 6, repeats = 20, lambda=NULL, scale.inputs = TRUE,decay=0, maxit=150)

fit

accuracy(fit)

ggof(as.vector(fitted.values(fit)),as.vector(train),gof.leg = TRUE, digits=2, legend = c("Ajustado", "Observado"),
     gofs=c("MAE", "RMSE", "NSE", "r", "R2", "KGE","cp"), xlab = "Index", ylab=c("Desembarcos (toneladas)"), main = c(""))

checkresiduals(fit, lag = 9)

#Proyecciones
Forecast_fit <- as.vector(forecast(object=fit, h=12))
Forecast_fit<-as.data.frame(t(as.vector(Forecast_fit[["mean"]])))
Forecast_fit<-as.data.frame(t(Forecast_fit))
colnames(Forecast_fit)[1]<-"Prediccion"

#Gráfico de dispersión
dispersion<-as.data.frame(cbind(Forecast_fit$Prediccion,test$Datos))
colnames(dispersion)[1]<-"Prediccion"
colnames(dispersion)[2]<-"Test"

p1<-ggplot(data = dispersion, aes(y = Prediccion, x = Test))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  ggtitle("NNAR(4,2,6) Atlántico")+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1.5), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Desembarcos observados (toneladas)",y="Desembarcos estimados (toneladas)")+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))

#Gráfico errores
ggof(as.vector(dispersion$Prediccion),as.vector(dispersion$Test),gof.leg = TRUE, digits=2, legend = c("Ajustado", "Observado"),
     gofs=c("MAE", "RMSE", "NSE", "r", "R2", "KGE","cp"), xlab = "Index", ylab=c("Desembarcos (toneladas)"), main = c(""))

Test <- ts(dispersion$Test, start=1, frequency = 12)
Prediccion <-ts(dispersion$Prediccion, start=1, frequency = 12)

union<-ts.union(Test,Prediccion)

p2<-autoplot(union)+
  scale_color_manual(values=c("black", "firebrick2"),breaks=c("Test","Prediccion"),labels=c("Observado","Estimado"))+
  geom_line(size = 0.8)+
  theme_classic()+
  ggtitle("NNAR(4,2,6) Atlántico")+
  theme(legend.position="top",legend.title = element_blank(), 
        legend.text= element_text(vjust=0.8, colour="black",size=rel(1.2), family="serif"))+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Meses",y="Desembarcos (toneladas)")+
  scale_x_continuous(breaks=c(1.000000,1.083333,1.166667,1.250000,1.333333,1.416667,
                              1.500000,1.583333,1.666667, 1.750000,1.833333,1.916667),
                     labels=c("Oct","Nov","Dic","Ene","Feb","Mar","Abr","May",
                              "Jun","Jul","Ago","Sep"))+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))

h<-plot_grid(p2,p1, ncol=2,nrow=1, labels = c("a)","b)"), align = 'v',
             label_size = 30,label_fontfamily = "serif", label_fontface = "bold", hjust = -0.5, vjust = 1.5 )
h

#Proyecciones Atlántico
a<-autoplot(forecast(fit,h=36))+
  geom_line(size = 0.8)+
  theme_classic()+
  ggtitle("NNAR(4,2,6) Atlántico")+
  theme(legend.position="top",legend.title = element_blank(), 
        legend.text= element_text(vjust=0.8, colour="black",size=rel(1.2), family="serif"))+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Años",y="Desembarcos (toneladas)")+
  scale_x_continuous(breaks=c(1,5,10,15,20,25),
                     labels=c("00-01","04-05","09-10","14-15","19-20","24-25"))+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))



#Mediterráneo

#Utilizamos el último (2022) para medir la capacidad de predicción del modelo

#Set de entrenamiento (2000 a 1)
train <- window(Mediterraneo, end = c(21,12))

#Set de validación de la predicción (2022)
test<-as.data.frame(window(Mediterraneo, start = c(22,1)))
colnames(test)[1]<-"Datos"

#NNAR(3,2,4)
set.seed(2015)

fit <- nnetar(train, p=3, P=2, size = 4, repeats = 20, lambda=NULL, scale.inputs = TRUE,decay=0.1, maxit=150)

fit

accuracy(fit)

ggof(as.vector(fitted.values(fit)),as.vector(train),gof.leg = TRUE, digits=2, legend = c("Ajustado", "Observado"),
     gofs=c("MAE", "RMSE", "NSE", "r", "R2", "KGE","cp"), xlab = "Index", ylab=c("Desembarcos (toneladas)"), main = c(""))

checkresiduals(fit, lag = 9)

#Proyecciones
Forecast_fit <- as.vector(forecast(object=fit, h=12))
Forecast_fit<-as.data.frame(t(as.vector(Forecast_fit[["mean"]])))
Forecast_fit<-as.data.frame(t(Forecast_fit))
colnames(Forecast_fit)[1]<-"Prediccion"

#Gráfico de dispersión
dispersion<-as.data.frame(cbind(Forecast_fit$Prediccion,test$Datos))
colnames(dispersion)[1]<-"Prediccion"
colnames(dispersion)[2]<-"Test"

p1<-ggplot(data = dispersion, aes(y = Prediccion, x = Test))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  ggtitle("NNAR(3,2,4)")+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1.5), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Desembarcos observados (toneladas)",y="Desembarcos estimados (toneladas)")+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))

#Gráfico errores
ggof(as.vector(dispersion$Prediccion),as.vector(dispersion$Test),gof.leg = TRUE, digits=2, legend = c("Ajustado", "Observado"),
     gofs=c("MAE", "RMSE", "NSE", "r", "R2", "KGE","cp"), xlab = "Index", ylab=c("Desembarcos (toneladas)"), main = c(""))

Test <- ts(dispersion$Test, start=1, frequency = 12)
Prediccion <-ts(dispersion$Prediccion, start=1, frequency = 12)

union<-ts.union(Test,Prediccion)

p2<-autoplot(union)+
  scale_color_manual(values=c("black", "firebrick2"),breaks=c("Test","Prediccion"),labels=c("Observado","Estimado"))+
  geom_line(size = 0.8)+
  theme_classic()+
  ggtitle("NNAR(3,2,4) Mediterráneo")+
  theme(legend.position="top",legend.title = element_blank(), 
        legend.text= element_text(vjust=0.8, colour="black",size=rel(1.2), family="serif"))+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Meses",y="Desembarcos (toneladas)")+
  scale_x_continuous(breaks=c(1.000000,1.083333,1.166667,1.250000,1.333333,1.416667,
                              1.500000,1.583333,1.666667, 1.750000,1.833333,1.916667),
                     labels=c("Oct","Nov","Dic","Ene","Feb","Mar","Abr","May",
                              "Jun","Jul","Ago","Sep"))+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))

h<-plot_grid(p2,p1, ncol=2,nrow=1, labels = c("a)","b)"), align = 'v',
             label_size = 30,label_fontfamily = "serif", label_fontface = "bold", hjust = -0.5, vjust = 1.5 )
h

#Proyecciones Mediterráneo
b<-autoplot(forecast(fit,h=36))+
  geom_line(size = 0.8)+
  theme_classic()+
  ggtitle("NNAR(3,2,4) Mediterráneo")+
  theme(legend.position="top",legend.title = element_blank(), 
        legend.text= element_text(vjust=0.8, colour="black",size=rel(1.2), family="serif"))+
  theme (plot.title = element_text(family="serif",
                                   size=rel(1), 
                                   vjust=0.5, 
                                   hjust=0.5,
                                   face="bold", 
                                   color="black", 
                                   lineheight=1.5))+
  labs(x="Años",y="Desembarcos (toneladas)")+
  scale_x_continuous(breaks=c(1,5,10,15,20,25),
                     labels=c("00-01","04-05","09-10","14-15","19-20","24-25"))+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text( vjust=-0.5, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.text.y = element_text( vjust=0.4, colour="black", 
                                    size=rel(1.2), family="serif"))+
  theme(axis.line.x=element_line(color="black", size=0.5))+
  theme(axis.line.y=element_line(color="black", size=0.5))+
  theme(axis.title.x = element_text( vjust=-0.5, colour="black", 
                                     size=rel(1.2),family = "serif"))+
  theme(axis.title.y = element_text( vjust=1.5, colour="black", 
                                     size=rel(1.2),family="serif"))


h<-plot_grid(a,b, ncol=2,nrow=1, labels = c("a)","b)"), align = 'v',
             label_size = 30,label_fontfamily = "serif", label_fontface = "bold", hjust = -0.5, vjust = 1.5 )
h