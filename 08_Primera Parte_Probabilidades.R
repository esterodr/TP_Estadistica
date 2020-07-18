########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 8 #########################
########################################################


#'En este ejercicio, jugaremos con el lanzamiento de dados de 6 caras.
#'En particular, analizaremos qué tan probable es que salga un 1 o un 6.

#' #### Ejercicio 8.a)
 
#' Realizar 12 lanzamientos de un dado *equilibrado*. Llamamos dado equilibrado a aquel en
#'  donde todos los números tienen la misma probabilidad de ocurrencia.
#' ¿Qué resultados obtuvo?


###### Ejecutar desde aqui #################################
library(tidyverse)
lanzamientos_12 <- sample(c(1:6), 12, replace = TRUE)
lanzamientos_12
###### Hasta aqui ##########################################


#' ¿Qué proporción de 1 y 6 obtuvo?
 

###### Ejecutar desde aqui #################################
cat("La proporción de 1 fue:",mean(lanzamientos_12==1)) 
cat("La proporción de 6 fue:",mean(lanzamientos_12==6))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico


###### Ejecutar desde aqui #################################
lanzamientos_12 %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("12 lanzamientos de un dado equilibrado") +
  ylab("Cantidad de lanzamientos")  +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' En base a los resultados, analice la pertinencia de la siguiente frase: 
 
#' *Como el dado es equilibrado, en 12 lanzamientos cada número tiene que salir*
#'  *exactamente 2 veces.*

 
#' #### Ejercicio 8.b)
 
#' Realizar 12 lanzamientos de un dado, donde la probabilidad de obtener un 6 es 0.20 y la de
#' obtener cada uno de los restantes números es 0.16. Es decir, el dado no es *equilibrado* sino que está cargado a favor del 6.
#' ¿Qué resultados obtuvo?


###### Ejecutar desde aqui #################################
lanzamientos_12b <- sample(c(1:6), 12, prob = c(0.16,0.16,0.16,0.16,0.16,0.20), replace = TRUE)
lanzamientos_12b
###### Hasta aqui ##########################################

 
#' ¿Qué proporción de 1 y 6 obtuvo?


###### Ejecutar desde aqui #################################
cat("La proporción de 1 fue:",mean(lanzamientos_12b==1)) 
cat("La proporción de 6 fue:",mean(lanzamientos_12b==6))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico
 

###### Ejecutar desde aqui #################################
lanzamientos_12b %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("12 lanzamientos de un dado no equilibrado") +
  ylab("Cantidad de lanzamientos")  +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' ¿Obtuvo resultados muy distintos respecto al ejercicio 8.a)?
 
#' En base a los resultados, analice la pertinencia de la siguiente frase: 

#' *Como el dado estaba cargado a favor del 6, en 12 lanzamientos tiene que salir más*
#'  *de dos veces el 6, mientras que los demás números tienen que salir una misma*
#'  *cantidad de veces.*


#' ¿Qué tan probables considera los resultados obtenidos,
#' tanto para el dado equilibrado como para el dado cargado?
#' Los siguientes gráficos de la distribución binomial pueden ayudarle a responder.


# Distribución Binomial para la cantidad de 1 obtenidos
###### Ejecutar desde aqui #################################
data.frame(Cantidad_de_Unos=c(0:12), "dado_equilibrado"=dbinom((0:12),12,1/6), "dado_cargado"=dbinom((0:12),12,0.16)) %>%
  gather(caso,Probabilidad,-Cantidad_de_Unos) %>%
  ggplot(aes(x=Cantidad_de_Unos, y=Probabilidad)) +
  geom_col(fill="cyan") +
  ylab("Probabilidad") +
  xlab("Cantidad de números 1 obtenidos") +
  facet_wrap(.~caso) +
  theme_classic() +
  scale_x_continuous(breaks=c(0:12),labels=c(0:12)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


# Distribución Binomial para la cantidad de 6 obtenidos
###### Ejecutar desde aqui #################################
data.frame(Cantidad_de_Seis=c(0:12), "dado_equilibrado"=dbinom((0:12),12,1/6), "dado_cargado"=dbinom((0:12),12,0.2)) %>%
  gather(caso,Probabilidad,-Cantidad_de_Seis) %>%
  ggplot(aes(x=Cantidad_de_Seis, y=Probabilidad)) +
  geom_col(fill="cyan") +
  ylab("Probabilidad") +
  xlab("Cantidad de números 6 obtenidos") +
  facet_wrap(.~caso) +
  theme_classic() +
  scale_x_continuous(breaks=c(0:12),labels=c(0:12)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' #### Ejercicio 8.c)

#' Repetiremos el ejercicio 8.a), pero esta vez lanzando **1 millón** de veces el dado equilibrado
#' Muestre los primeros 10 resultados


###### Ejecutar desde aqui #################################
lanzamientos_1m <- sample(c(1:6), 1000000, replace = TRUE)
head(lanzamientos_1m,10)
###### Hasta aqui ##########################################


#' ¿Qué proporción de 1 y 6 obtuvo en el millón de lanzamientos?


###### Ejecutar desde aqui #################################
cat("La proporción de 1 fue:",mean(lanzamientos_1m==1)) 
cat("La proporción de 6 fue:",mean(lanzamientos_1m==6))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico
 

###### Ejecutar desde aqui #################################
lanzamientos_1m %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("1 millón de lanzamientos de un dado equilibrado") +
  ylab("Cantidad de lanzamientos")  +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Viendo los resultados, ¿está en condiciones de afirmar que el dado es equilibrado?
#' Por el momento, puede dar una respuesta “a ojo”. En la segunda parte de la materia 
#' haremos un “test de hipótesis para la diferencia de proporciones” para poder 
#' argumentar de manera más rigurosa.
 

#' #### Ejercicio 8.d)

#' Repetiremos el ejercicio 8.b), pero esta vez lanzando 1 millón de veces el dado con
#'  probabilidades levemente favorables para el 6.
#' Muestre los primeros 10 resultados


###### Ejecutar desde aqui #################################
lanzamientos_1mb <- sample(c(1:6), 1000000, prob = c(0.16,0.16,0.16,0.16,0.16,0.20), replace = TRUE)
head(lanzamientos_1mb,10)
###### Hasta aqui ##########################################

 
#' ¿Qué proporción de 1 y 6 obtuvo?
 

###### Ejecutar desde aqui #################################
cat("La proporción de 1 fue:",mean(lanzamientos_1mb==1)) 
cat("La proporción de 6 fue:",mean(lanzamientos_1mb==6))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico


###### Ejecutar desde aqui #################################
lanzamientos_1mb %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("1 millón de lanzamientos de un dado no equilibrado") +
  ylab("Cantidad") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' Viendo los resultados, ¿está en condiciones de afirmar que el dado no es equilibrado?
#' Por el momento, puede dar una respuesta “a ojo”. En la segunda parte de la materia 
#' haremos un “test de hipótesis para la diferencia de proporciones” para poder 
#' argumentar de manera más rigurosa.

#' ¿Qué diferencias nota en la simulación con 12 lanzamientos y la de 1 millón de lanzamientos?
#' ¿Cuál es más confiable para sacar conclusiones?

