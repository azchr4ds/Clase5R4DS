# Describir modelos AR(2) , graficarlos para valores diferentes 
# de los argumentos (ar = c(p1,p2))
# AR(2)
AR2 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.2)) ,
          n = 100,sd = 0.1)
# Probar varias combinaciones de p1 y p2 , graficar las series de tiempo
# simuladas, y sus correspondientes funciones de autocorrelacion simple
# y funciones de autocorrelacion parcial 

# Repetir lo mismo para los procesos MA(2) 

