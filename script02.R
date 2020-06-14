#### configuracion ####
rm(list =ls())
setwd("C:/Users/AZCH/Desktop/Clase5_R/")
getwd()
dir()


#### Funciones de autocorrelacion ####
# ¿Que es la autocorrelacion?
# Palabras sencillas : la autocorrelacion es un tipo de dependencia
# serial
# Autocorrelacion: es cuando una serie de tiempo esta relacionada linealmente
# con una version retrasada de si misma

# ¿Porque es importante la autocorrelacion?
# Lectura !!!!!!!

# ACF: describe que "tan bien" se relaciona el valor presete de la serie 
# con sus valores pasados

library(quantmod)
MicrosoftMensual <- getSymbols(Symbols = c("MSFT"),
                               from = "1986-03-01",
                               to = "2020-06-01", 
                               auto.assign = FALSE,
                               periodicity ="monthly")

AperturaMSFT <- ts(MicrosoftMensual$MSFT.Open,
                   start = c(1983,3),
                   frequency = 12)


# Funcion de autocorrelacion simple para la serie de tiempo original
acf(AperturaMSFT)

class(acf(AperturaMSFT))
help(acf)


# Como podemos deducir del grafico anterior, la autocorrelacion continua 
# disminuyendo a medida que aumenta el retraso, lo que confirma que no existe
# una asociacion lineal entre las observaciones separadas por retrasos mas
# grandes 

# Funcion de autocorrelacion parcial 
pacf(AperturaMSFT)


# Funcion de autocorrelacion simple para la serie transformada 
y <- diff(log(AperturaMSFT))
View(y)
acf(y,main="ACF de la serie transformada",
    sub ="Eliminacion de la tendencia")

pacf(y)


AcfY <- acf(y, plot = FALSE)

AcfY$acf


# Prueba de Ljung-Box (1978)
# Esta prueba permite probar en forma conjunta que todos los coeficientes
# de autocorrelacion son simulataneamente iguales a cero (esto es que con
# independientes)
# H0 : las autocorrelaciones son independientes (hipotesis nula)
# H1 : Las autocorrelaciones no son independientes (Hip. Alternativa)
# Esta prueba tambien es conocida como la prueba Q de Ljung-Box
# Otras opciones : Breusch-Godfrey y Durbin-watson

Box.test(x = AperturaMSFT,lag = 12, type = "Ljung-Box")




















































































