#### configuracion ####
rm(list =ls())
setwd("C:/Users/AZCH/Desktop/Clase5_R/")
getwd()
dir()


#### Ejemplo 1 ####

library(ggplot2)
library(tseries) # instalar
library(forecast) # instalar

# datos
help(gas)
data(gas)
class(gas)
start(gas)
end(gas)
frequency(gas)

# primeros 6 elementos
head(gas)

# ultimos 6 elementos
tail(gas)


# Grafico de la data
plot(gas,
     xlab = "AÑO",
     ylab = "Produccion de Gas [Mensual]",
     main = "Australia [1956-1995]")


# En forecast :
help("seasonplot")
help("tsdisplay")
seasonplot(gas)
tsdisplay(gas)

# Descomposicion de la serie de tiempo
GasDescomp <- decompose(gas)
help("decompose")
class(GasDescomp)

plot(GasDescomp)

#### Test de estacionariedad ####
# Test de Dicky-Fuller Aumentado (ADF)
# Hipotesis para el test ADF
# Hipotesis Nula : H0 : "La serie de tiempo no es estacionaria"
# Hipotesis Alternativa : Ha : "La serie de tiempo es estacionaria"
help("adf.test")
gas.adf <- adf.test(gas)

gas.adf$p.value # mayor que un umbral 0.05
# Que significa que el p-value es mucho mayor qu el valor signficativo
# estandar (0.05) , No podemos rechazar la hipotesis NULA (H0)
# En concusion , la serie de tiempo gas NO ES ESTACIONARIA

# Descomposicion de objeto gas
GasDescomp$x
class(GasDescomp$seasonal)
GasDescomp$trend
GasDescomp$random

# Otro opcion a decompose a stl:
help(stl)
decomposed <- stl(gas,s.window = "periodic")

class(decomposed)

# definamos variables para cada una de las componentes de la serie:
seasonal <- decomposed$time.series[,1]
trend <- decomposed$time.series[,2]
remainder <- decomposed$time.series[,3]

# Removamos la estacionalidad
des.data <- gas - seasonal
class(des.data)

# ploteemos esta nueva serie desestacionalizada 
plot(des.data,
     ylab = "Produccion",
     main = "Serie desestacionalizada")

# podemos apliar stl a des.data
stl(des.data,s.window = "periodic")

#### Ejemplo 2 ####
rm(list = ls())
library(TTR)
library(help = TTR)
Mercados <- stockSymbols()


# Consigamos acciones baratas
class(Mercados)
colnames(Mercados)

# Rango de precios de las acciones 
range(Mercados$LastSale, na.rm = TRUE)

# Baratas : LastSale < 10 [subjetivo : umbral = 10]
McdoBaratas <- Mercados[Mercados$LastSale < 10,]


# Veamos la variable sector:
McdoBaratas$Sector <- as.factor(McdoBaratas$Sector)
levels(McdoBaratas$Sector)

EnergyBaratas <- McdoBaratas[McdoBaratas$Sector=="Energy",]

# Demanera arbitraria escogemos : PAA
library(quantmod)
EnergyPAA <- getSymbols("PAA",from = "2000-01-01", 
                        auto.assign = FALSE)

# Grafiquemos el precios de apertura 
plot(EnergyPAA$PAA.Open)


# Usamos la funcion adf.test (dickey-fuller)
help("adf.test")
df_Open <- adf.test(EnergyPAA$PAA.Open)
df_Open$p.value
# ONSERVACION IMPORTANTE: Si el P-value del algoritmo de df aumentado 
# es mayor que el estandar 0.05, eso implica que la serie de tiempo
# analizada NO ES ESTACIONARIA (rechazamos la H0)

# Analicemos la estacionariedad de la data EnergyPAA, previamente diferenciada
library(forecast)
help("ndiffs")
df2_Open <- ndiffs(EnergyPAA$PAA.Open, test = c("kpss"))
df3_Open <- ndiffs(EnergyPAA$PAA.Open, test = c("adf"))
df4_Open <- ndiffs(EnergyPAA$PAA.Open, test = c("pp"))
# Bajo el critero de "mayoria gana" , decidimos diferencias 
# una unica vez nuestra serie de tiempo original 

y <- EnergyPAA$PAA.Open # Ya vimos (estadisticamente hablando)
# que y no es estacionaria (test de dickey-fuller)
yd <- diff(y)
class(yd)


# Grafiquemos las dos series y - yd
par(mfrow = c(1,2))
# definimos un eje Y adecuado 
ylm <- c(min(y,yd,na.rm = TRUE), max(y,yd,na.rm = TRUE))
plot(y, ylim = ylm, main="Serie de tiempo")
plot(yd, ylim = ylm, main="Serie de tiempo diferenciada")

ndiffs(na.omit(yd))
adf.test(na.omit(yd)) # yd es estacionaria
# p-value = 0.01 < 0.05 => acepto H0

#### Ejemplo 3 ####
rm(list = ls())
library(quantmod)

MicrosoftMensual <- getSymbols(Symbols = c("MSFT"),
                               from = "1986-03-01",
                               to = "2020-06-01", 
                               auto.assign = FALSE,
                               periodicity ="monthly")

AperturaMSFT <- ts(MicrosoftMensual$MSFT.Open,
                   start = c(1983,3),
                   frequency = 12)
start(AperturaMSFT)
end(AperturaMSFT)


# Descomponer la serie de tiempo
AperturaMSFT.Desc <- decompose(AperturaMSFT)
plot(AperturaMSFT.Desc, xlab = "Año")

class(AperturaMSFT.Desc$seasonal)
class(AperturaMSFT.Desc$trend)
class(AperturaMSFT.Desc$random)

graphics.off()
plot(AperturaMSFT.Desc$seasonal)
# El grafico muesta que la componente estacional tiene una
# peculiaridad en su disposicion

# Descompongamos la serie de manera multiplicativa
help("decompose")
AperturaMSFT.Desc.Mult <- decompose(x = AperturaMSFT,
                                    type = "multiplicative")

plot(AperturaMSFT.Desc.Mult, xlab = "Año")

#### transformaciones basicas de una serie de tiempo ####
plot(AperturaMSFT)

# Estabilizacion de la varianza
plot(log(AperturaMSFT),
     main = "Log(DATA)")

# Eliminacion de la tendencia 
plot(diff(log(AperturaMSFT)),
     main = "Logaritmo al vector de diferencias")

# Eliminacion de la estacionalidad
plot(diff(diff(log(AperturaMSFT)), lag = 12),
     main = "Eliminacion de estacionalidad")

