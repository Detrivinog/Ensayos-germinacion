setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Génetica y viveros")


require(readxl)
datos=read_xlsx("Datos_germinacion.xlsx", sheet = "GermT")


mod=lm(Semillas~Trat, datos)
summary(mod)
anova(mod)
a1=aov(Semillas~Trat, datos)

require(agricolae)
prueba=HSD.test(a1, "Trat")
prueba
bar.group(prueba$groups,ylim=c(0,90),border="black", xlab="Tratamiento", ylab="Germinación")

#Viabilidad
datos=read_xlsx("Datos_germinacion.xlsx", sheet = "Germinación")
require(lattice)
with(datos, xyplot(Semillas~Dia|Trat))

xyplot(Semillas~Dia|Trat, data=datos, scales = list(relation = "free"),
  ylim = list(c(0, 70), c(0, 10)), 
  panel = function(x, y) {
    panel.xyplot(x, y, grid = TRUE,type = c("p", "smooth"),col.line = "black")
 })

datosE=subset(datos, Trat=="Escarificación")
datosT=subset(datos, Trat=="Testigo")

modn=nls(Semillas~a-b*exp(-c*Dia),start=list(a=1,b=75,c=0.5),data=datosE)
summary(modn)
co=coefficients(modn)

with(datosE, plot(Dia, Semillas, xlim = c(0,22), ylim = c(0,80)))
curve(co[1]-co[2]*exp(-co[3]*x),xlim = c(0,22), ylim = c(0,80), add=T, col="blue")

str(predict(modn))
str(datosE)


modt=lm(log(Semillas+0.1)~log(Dia), datosT)
summary(modt)

modn2=nls(Semillas~a*(Dia^b),start=list(a=2,b=2),data=datosT)
summary(modn2)
co2=coefficients(modn2)
with(datosT, plot(Dia, Semillas, xlim = c(0,26),ylim = c(0,5)))
curve(co2[1]*(x^co2[2]),xlim = c(0,26),ylim = c(0,5), add=T, col="red")

datos$pred=c(predict(modn),predict(modn2))

xyplot(Semillas~Dia|Trat, data=datos, scales = list(relation = "free"),
       ylim = list(c(0, 70), c(0, 10)), 
       panel = function(x, y) {
         panel.xyplot(x, y, grid = TRUE,type = c("p", "smooth"),col.line = "red")
       })



xyplot(Semillas~Dia|Trat, data=datos, scales = list(relation = "free"),
       ylim = list(c(0, 70), c(0, 10)),xlim = list(c(0, 21), c(0, 25.3)),
       xlab = "Tiempo (días)",ylab = "Semillas germinadas (%)",
       panel =function(x, y){
         panel.xyplot(x,y)
         panel.curve(co[1]-co[2]*exp(-co[3]*x),0,20, col.line="black", type="l", 
                     fun=(datos$Trat=="Escarificación"))
         panel.curve(co2[1]*(x^co2[2]),0,25, add=T,col.line="red")
         })


par(mfrow=c(1,2))
with(datosE, plot(Dia, Semillas, xlim = c(0,22), ylim = c(0,80)))
curve(co[1]-co[2]*exp(-co[3]*x),xlim = c(0,22), ylim = c(0,80), add=T, col="blue")
with(datosT, plot(Dia, Semillas, xlim = c(0,26),ylim = c(0,5)))
curve(co2[1]*(x^co2[2]),xlim = c(0,26),ylim = c(0,5), add=T, col="red")


modg=glm(Semillas~Dia+Trat,datos, family="poisson")
summary(modg)


xyplot(written ~ course | gender, data = datos,
       panel = function(x, y) {
         panel.xyplot(x, y, grid = TRUE,
                      type = c("p", "smooth"),
                      col.line = "black")
       })





datosE=subset(datos, Trat=="Escarificación")
mod1=lm(Semillas~Dia, datosE)
summary(mod1)

mod2=lm(log(I(Semillas+0.1))~log(Dia),datosE)
summary(mod2)
anova(mod2)

b0=exp(coefficients(mod2)[1])

b0=exp(coefficients(mod2)[1]+0.5*anova(mod2)$'Mean Sq'[2])

with(datosE, plot(Dia, Semillas, xlim = c(0,22), ylim = c(0,80)))
with(datosE,scatter.smooth(Dia,Semillas))

curve(b0*(x^coefficients(mod2)[2]),add = T,xlim = c(0,22), ylim = c(0,80) )
scatter.smooth()

datosT=subset(datos, Trat=="Testigo")




datos=readWorksheet(wb, sheet = "cosecha", header = TRUE)
with(datos, plot(Bt~Diametro))

mod1=lm(log(Bt)~log(I((Diametro^2)*Long)),datos)
summary(mod1)
str(anova(mod1))
exp(coefficients(mod3)[1]+ 0.5*anova(mod3)$'Mean Sq'[3])

mod2=lm(Bt~Diametro, datos)
summary(mod2)

mod3=lm(log(Bt)~log(Diametro)+log(Long),datos)
summary(mod3)

