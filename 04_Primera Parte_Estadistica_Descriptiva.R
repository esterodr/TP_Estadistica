########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 4 #########################
########################################################


#' El sitio de la Reserva Federal de Sant Louis (EEUU) (https://fred.stlouisfed.org/) cuenta
#'  con una gran cantidad de datos económicos, tanto de EEUU como a nivel global.
#' 
#' Aquí utilizaremos uno de los precios de referencia del petróleo
#'  (WTI, https://fred.stlouisfed.org/series/DCOILWTICO) y otro del oro
#'   (https://fred.stlouisfed.org/series/GOLDAMGBD228NLBM).


#' Cargamos los datos posteriores a 1986 y realizamos un gráfico para conocer las series

###### Ejecutar desde aqui #################################
library(tidyverse)
load("./archivos/oro_petroleo.Rda")
datos %>% ggplot(aes(x=Fecha)) +
  geom_line(aes(y=Oro, colour="Oro")) +
  geom_line(aes(y=Petroleo, colour="Petroleo")) +
  theme_classic() +
  ylab("Dólares por barril (Petróleo) / Dólares por Onza (Oro)") +
  scale_color_manual(name="", values=c("darkgreen", "blue")) +
  ggtitle("Series de Tiempo del Oro y Petróleo") +
  theme(panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_line(linetype = "dotted"))
###### Hasta aqui ##########################################

 
#' En base al gráfico anterior, ¿qué serie tiene una media mayor? ¿cuál una mediana mayor?
#' ¿Qué serie presenta más volatilidad?
#' Puede utilizar las siguientes funciones para responder:
 

###### Ejecutar desde aqui #################################
summary(datos$Oro)
summary(datos$Petroleo)
sd(datos$Oro, na.rm = TRUE)
sd(datos$Petroleo, na.rm = TRUE)
###### Hasta aqui ##########################################


#' ¿Qué serie tiene un mayor coeficiente de variación?


###### Ejecutar desde aqui #################################
cat("El coeficiente de variación del oro es:",sd(datos$Oro, na.rm = TRUE)/mean(datos$Oro, na.rm = TRUE)) 
cat("El coeficiente de variación del petróleo es:",sd(datos$Petroleo, na.rm = TRUE)/mean(datos$Petroleo, na.rm = TRUE)) 
###### Hasta aqui ##########################################

 
#' Compare estos resultados con los de varianza y desvío standard
#' ¿Cuándo debería usarse el coeficiente de variación en lugar del desvío standard?

#' Como habrá notado, es difícil comparar dos series con valores muy distintos 
#' en un simple gráfico como el que se realizó al principio. Una solución puede 
#' ser construir “números índices”. Para ello, es necesario elegir una “base”, 
#' la cual tomará un valor de 100. En este caso, podemos tomar como base el 
#' primer día para el cual tenemos datos: el 2 de enero de 1986.
#' Para construir la serie “base 2 de enero de 1986 = 100” para el oro, lo que 
#' hacemos es dividir toda la serie por el valor que tenía el oro el 2 de enero de 1986, 
#' y se multiplican los resultados por 100. Se procede de la misma forma para obtener 
#' el número índice de la serie del petróleo. 

#' Luego de construir los números índice para cada serie, represente los mismos en un gráfico.


###### Ejecutar desde aqui #################################
datos %>% mutate(Oro = datos$Oro/datos$Oro[1]*100,
                 Petroleo = datos$Petroleo/datos$Petroleo[1]*100) %>% ggplot(aes(x=Fecha)) +
  geom_line(aes(y=Oro, colour="Oro")) +
  geom_line(aes(y=Petroleo, colour="Petroleo")) +
  theme_classic() +
  ylab("2 ene 1986 = 100") +
  scale_color_manual(name="", values=c("darkgreen", "blue")) +
  ggtitle("Índices de Precio del Oro y Petróleo") +
  theme(panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_line(linetype = "dotted"))
###### Hasta aqui ##########################################


#' Observe que ambos índices han alcanzado por momentos valores iguales o superiores a 400.
#' ¿Qué significa que el índice tenga un valor de 400?

#' ¿Nota algo raro en la serie del petróleo a mediados de 2020?