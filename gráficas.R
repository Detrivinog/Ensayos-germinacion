setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Génetica y viveros")

datos=read.csv2("sustratos.csv")
datos
require(ggplot2)
require(plyr)
write.csv2(datos3,"resumen_datos.csv")
ggplot(datos, aes(x=sustrato, y=Pt)) + geom_bar()

qplot(Pt, data = datos, geom = "bar")
ggplot(datos, aes(x=sustrato,y=Pt))+
  geom_bar(position='dodge',stat='identity',size=1)


datos3<- ddply(datos, c("Porosidad", "sustrato"), summarise,por= mean(Porcentaje),
               desv   = sd(Porcentaje))

datos4=ddply(datos,c("sustrato"), summarise, densidad=mean(Da), desv=sd(Da) )

ggplot(datos4,aes(sustrato, densidad))+
  geom_bar(position='dodge',stat='identity',size=1, width = .4)+
  geom_errorbar(aes(ymin = densidad-desv, ymax = densidad + desv), 
                width=0.2, size= .8,position=position_dodge(0.9)) + 
  theme_bw()+ ylab("Densidad (g/ml)")+xlab("Sustrato")


ggplot(datos3,aes(sustrato, por, fill=Porosidad))+
  geom_bar(position='dodge',stat='identity',size=1)+
  geom_errorbar(aes(ymin = por-desv, ymax = por + desv), 
                width=0.4, size= 1,position=position_dodge(0.9)) + 
  theme_bw()+ ylab("Porcentaje %")+xlab("Sustrato")+scale_fill_grey(start=0.3, end= .9)


