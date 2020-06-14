#### configuracion ####
rm(list =ls())
setwd("C:/Users/AZCH/Desktop/Clase5_R/")
getwd()
dir()

#### Modelos Autoregresivos ####
# Toda serie de tiempo se puede descomponer : decompose
# Esta descomposicion : Aditiva - Multiplicativa
# Aditiva : Y[t] = T[t] + S[t] + e[t]
# Multiplicativa : Y[t] = T[t]*S[t]*e[t]
# Y[t] : serie observada
# T : Componente tendencial
# S : componente estacional 
# e : componente irregular (random-error)
# e == Y[t] - (T[t] + S[t])

# Supuestos de estudio:
# Suponer que la data (Y[t]) es funcion del pasado de ella misma : procesos AR(p)
# Suponer que la data (Y[t]) es funcion de sus errores (componente irregular) : procesos MA(q)
# MA : moving - average
# ARIMA := AR + MA
# aima.sim : Sirve para todos los modelos y submodelos de tipo ARIMA
# "AR" : Autoregresivo
# "I" : Integrado
# "MA" : promedio movil (moving-average)
# Especificacion del orden de los modelos ARIMA 
# Parametros : p , d , q 
# Por Ejemplo : AR(2)  con ARIMA(2,0,0)
# MA(1) como ARIMA(0,0,1)
help(arima.sim)


# AR(1) : y[t] = mu + phi[1]*y[t-1] + Err[t]
# AR(2) : y[t] = mu + phi[1]*y[t-1] + phi[2]*y[t-2] + Err[t]

# Simulemos un AR(1) = ARIMA(1,0,0)
# Por un tema de reproduciblidad: fijemos una semilla
## Definamos nuestro modelo : la descripcion del modelo (list)
## del modelo AR(1) con coeficiente (phi[i]) pequeño
AR.sm <- list(order = c(1,0,0) , ar = 0.1 , sd=0.1)

## Definamos nuestro modelo : la descripcion del modelo (list)
## del modelo AR(1) con coeficiente (phi[i]) grande
AR.lg <- list(order = c(1,0,0) , ar = 0.9 , sd=0.1)

# Simulamos los procesos AR(1)
AR1.sm <- arima.sim(n = 50 , model = AR.sm)
AR1.lg <- arima.sim(n = 50, model = AR.lg)
class(AR1.lg)
class(AR1.sm)

# grafiquemos estas series de tiempo simuladas
par(mfrow = c(1,2))
# configuremos el eje Y adecuado
ylm <- c(min(AR1.sm, AR1.lg) , max(AR1.sm, AR1.lg))
plot.ts(AR1.sm, ylim = ylm, main = "phi[1] = 0.1")
plot.ts(AR1.lg, ylim = ylm, main = "phi[1] = 0.9")


# Generemos dos modelos AR(1) que tengan el mismo coeficiente en magnitud
# pero de signos opuestos, y comparemos su comportamento
# Fijemos una semilla
set.seed(666)

# Definamos la descripcion de nuestro modelo AR(1) con coeficiente positivo 
AR.pos <- list(order = c(1,0,0), ar = 0.5, sd=1)
# Definamos la descripcion de nuestro modelo AR(1) con coeficiente negativo
AR.neg <- list(order = c(1,0,0), ar = -0.5, sd=1)
# Simulemos AR(1)
AR1.pos <- arima.sim(n = 50, model = AR.pos)
AR1.neg <- arima.sim(n = 50,model = AR.neg)

graphics.off()
par(mfrow = c(1,2))
ylm = c(min(AR1.neg, AR1.pos) , max(AR1.neg, AR1.pos))
plot.ts(AR1.pos, ylim = ylm, main = "Phi[1] > 0 ")
plot.ts(AR1.neg, ylim = ylm, main = "Phi[1] < 0 ")


#### Modelos o procesos de media movil ####
# UN proceso de media movil de orden q, o MA(q), es una suma ponderada
# del error (aleatorio) actual con los mas recientes (Err[t-1] , Err[t-2], ....Err[t-q])
# Escribamos el modelo matematico : vector de errores : w
# Y[t] = w[t] + theta1*w[t-1] + theta2*w[t-2] + ... + thetaq*w[t-q]
# donde w[t] es una ruido blanco con media cero y varianza sigma^2
# Para propositos usuarles tomaremos w[t] siga una distribucion normal 
# N(0,q). Notar que los procesos de media movil (MA) son sumas finitas
# de errores estacionarios, por ellos los procesos MA son estacionarios.


# Simulacion de un MA(q)
rm(list = ls())
# fijemos una semilla 
set.seed(99)
# definamos nuestro descriptor del modelo (list) : MA(1) coeficiente pequeño 
# positivo y cercano a cero 
MA.sm <- list(order = c(0,0,1) , ma = 0.2 , sd = 0.1)

# definamos nuestro descriptor del modelo (list) : MA(1) coeficiente grande
# positivo y cercano a uno
MA.lg <- list(order = c(0,0,1) , ma = 0.8 , sd = 0.1)

# definamos nuestro descriptor del modelo (list) : MA(1) coeficiente negativo
MA.neg <- list(order = c(0,0,1) , ma = -0.5 , sd = 0.1)

# Simulemos los procesos (modelos) MA(1)
MA1.sm <- arima.sim(n = 500 , model = MA.sm)
MA1.lg <- arima.sim(n = 500 , model = MA.lg)
MA1.neg <- arima.sim(n = 500 , model = MA.neg)

# Generamsos unos graficos
par(mfrow=c(1,3))
plot.ts(MA1.sm, ylab = "ma = 0.2")
plot.ts(MA1.lg, ylab = "ma = 0.8")
plot.ts(MA1.neg, ylab = "ma = -0.5")


class(MA1.lg)
graphics.off()
acf(MA1.lg)


#### Modelos o procesos ARMA (Autoregresivo de media movil)
# Fijemos una semilla
set.seed(123)
ARMA22 <- list(order = c(2,0,2),
               ar = c(-0.07,0.2),
               ma = c(0.7,0.2))

# Media del proceso : mu 
mu = 5
# Simulamos el proceso arma + mu (media)
ARMA.sim <- arima.sim(n = 1000, model = ARMA22) + mu 

class(ARMA.sim)
plot.ts(ARMA.sim)
acf(ARMA.sim) # AR
pacf(ARMA.sim)  # MA

# ARMA(2,2) = AR(2) + MA(2)
# Estimar esos parametros
arima(x = ARMA.sim, order=c(2,0,2))










































































































