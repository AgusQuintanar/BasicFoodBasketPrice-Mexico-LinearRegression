# Regresion lineal de los precios de la canasta alimentaria urbana en mexico de 1992 a 2020
#Autor: Agustin Quitanar


################################################## OBTENER DATOS DEL CSV ####################################################
# SOURCE CSV: https://www.coneval.org.mx/Medicion/MP/Paginas/Lineas-de-bienestar-y-canasta-basica.aspx
getwd()
setwd("/Users/agusquintanar/Desktop/DESKTOP/TEC/FIFTH SEMESTER/ADVANCED DB/BasicFoodBasketPrice-Mexico-LinearRegression")

datos.canasta.alimentaria <- read.csv("precio_canasta_alimentaria_mexico.csv", stringsAsFactors = F)
str(datos.canasta.alimentaria)

datos.canasta.alimentaria.mtx <- matrix(datos.canasta.alimentaria)
datos.canasta.alimentaria.mtx

datos.canasta.alimentaria.mtx.nbr <- as.numeric(as.character(unlist(datos.canasta.alimentaria.mtx[2])))


################################################## OBTENER REGRESION LINEAL ##################################################

tiempo<- seq(as.Date("1992-1-1"), as.Date("2020-9-1"), by = "months")
length(tiempo)

formula.pcb <- datos.canasta.alimentaria.mtx.nbr ~  tiempo #formula precio canasta basica

lm.canasta.alimentaria <- lm(formula=formula.pcb) #modelo lineal (regresion lineal)
summary(lm.canasta.alimentaria)


########################################## PREDICCION PRECIO CANASTA EN 2021 MENSUAL #########################################

seq.2021 <- seq(as.Date("2021-1-1"), as.Date("2021-12-1"), by = "months")
df.prediccion.pcb.del.2021 <- data.frame(tiempo=seq.2021)
df.prediccion.pcb.del.2021

prediccion.pcb.2021 <- predict(lm.canasta.alimentaria, df.prediccion.pcb.del.2021)
prediccion.pcb.2021.df <- data.frame(tiempo=seq.2021, precio=prediccion.pcb.2021) # para estetica
prediccion.pcb.2021.df

######################################  GRAFICAR PREDICCION PRECIO CANASTA EN 2021 MENSUAL ####################################
plot(tiempo,
     datos.canasta.alimentaria.mtx.nbr, 
     main="Precio de la Canasta Básica Urbana en México (Predicción 2021)",
     sub="(Enero 1992 - Septiembre 2020)",
     xlab="Tiempo",
     ylab="Precio",
     col="blue",
     xlim=c(as.Date("1992-1-1"), as.Date("2022-1-1")),
     ylim=c(0, 1700)
     )
legend("topleft",
       c("Precio de la Canasta Básica Mensual (1992 - 2021)",
         "Regresión Lineal", 
         "Predicción del precio de la Canasta Básica Mensual en 2021"),
       fill=c("blue","red","green")
)
abline(lm(datos.canasta.alimentaria.mtx.nbr  ~  tiempo), col="red")
points(seq.2021, prediccion.pcb.2021, col="green")


########################################## PREDICCION PRECIO CANASTA EN EN 20 ANIOS ###########################################

seq.sig.20.anios <- seq(as.Date("2021-1-1"), as.Date("2040-1-1"), by = "years")
df.prediccion.sig.20.anios <- data.frame(tiempo=seq.sig.20.anios)
df.prediccion.sig.20.anios

prediccion.pcb.sig.20.anios <- predict(lm.canasta.alimentaria, df.prediccion.sig.20.anios)
prediccion.pcb.sig.20.anios.df <- data.frame(tiempo=seq.sig.20.anios, precio=prediccion.pcb.sig.20.anios) # para estetica
prediccion.pcb.sig.20.anios.df

######################################  GRAFICAR PREDICCION PRECIO CANASTA EN 20 ANIOS #######################################

plot(tiempo,
     datos.canasta.alimentaria.mtx.nbr, 
     main="Precio de la Canasta Básica Urbana en México (Predicción 2020-2040)",
     sub="(Enero 1992 - Septiembre 2020)",
     xlab="Tiempo",
     ylab="Precio",
     col="blue",
     xlim=c(as.Date("1992-1-1"), as.Date("2045-1-1")),
     ylim=c(0, 3000)
)
legend("topleft",
       c("Precio de la Canasta Básica Mensual (1992 - 2021)",
         "Regresión Lineal", 
         "Predicción del precio de la Canasta Básica Anual 2020 - 2040"),
       fill=c("blue","red","green")
)
abline(lm(datos.canasta.alimentaria.mtx.nbr  ~  tiempo), col="red")
points(seq.sig.20.anios, prediccion.pcb.sig.20.anios, col="green")

#############################################################################################################################
