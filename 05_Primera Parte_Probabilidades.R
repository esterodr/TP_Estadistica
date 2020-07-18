########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 5 #########################
########################################################


#' En este ejercicio y en los siguientes, recurriremos a juegos de azar para 
#' entender cómo juegan las probabilidades.
#' En este caso, simularemos un mazo de naipes españolas de 40 cartas, 
#' como el que se utiliza para jugar al Truco 


###### Ejecutar desde aqui #################################
library(tidyverse)
mazo <- paste(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4),
                rep(6,4), rep(7,4), rep("Sota",4), rep("Caballo",4), rep("Rey",4)),
              c("Oros", "Espadas", "copas", "Bastos"))
mazo # Comprobar que el mazo está generado correctamente
###### Hasta aqui ##########################################

 
#' En una mano de Truco, cada jugador recibe 3 cartas. Podemos simular una mano 
#' realizando una extracción de 3 cartas sin reposición del mazo. Recuerde que 
#' “extracción sin reposición” significa que, luego de sacar una carta, no la volvemos 
#' a introducir en el mazo, por lo que no está disponible para la siguiente extracción.


###### Ejecutar desde aqui #################################
sample(mazo, 3, replace=FALSE)
###### Hasta aqui ##########################################


#' ¿Cuántas manos distintas se pueden obtener? Esto se puede responder de manera analítica 
#' o mediante una simulación. Empecemos por ésta última manera, simulando 1 millón de manos 
#' y contando cuántas son únicas. Obviamente, para realizar esto es imprescindible una 
#' computadora.

#' *(La simulación puede durar unos segundos, esperar antes de continuar)*.

###### Ejecutar desde aqui #################################
repeticiones <- 1000000
manos <- replicate(repeticiones, {
  mano <- sample(mazo, 3, replace=FALSE)
  manos <- mano[order(mano)] }) # Ordena alfabeticamente las cartas
###### Hasta aqui ##########################################

 
#' La simulación quedó almacenada en una matriz de 3 filas y 1 millón de columnas:
#' Ver las primeras 5 manos de la simulación (recuerde que, en la matriz, cada 
#' columna es una mano distinta):


###### Ejecutar desde aqui #################################
dim(manos) # Ver dimensiones de la matriz 
manos[1:3,1:5] # Primeras 5 manos
###### Hasta aqui ##########################################


#' Observación.
#' Las cartas de cada mano están ordenadas alfabéticamente, de modo que si en una 
#' mano salieron el 1 de espadas, el 1 de bastos y el 1 de copas, en la matriz 
#' queda almacenada la mano de la siguiente forma "1 bastos, 1 copas, 1 espadas".
 
#' En el millón de manos, seguramente hay muchas conformadas por las mismas cartas.
#' Nos quedamos sólo con las manos únicas.
#' La cantidad de columnas de esta nueva matriz nos da la cantidad de manos únicas obtenidas
#' en la simulación.
#' ¿Cuántas manos únicas se obtuvieron en la simulación?


###### Ejecutar desde aqui #################################
manos_unicas <- unique(manos, MARGIN = 2)
ncol(manos_unicas)
###### Hasta aqui ##########################################


#' Calcular la cantidad de manos anticipada por la teoría. Esto equivale al problema de calcular
#' cuántos subconjuntos de 3 elementos podemos conformar en base a 40 elementos. La respuesta la
#' da el número combinatorio:


###### Ejecutar desde aqui #################################
factorial(40)/(factorial(40-3)*factorial(3))
###### Hasta aqui ##########################################


#' ¿Coinciden estos resultados con los de la simulación? 
 
#' ¿Todas estas manos tienen la misma probabilidad?

#' ¿Qué cree que hubiera pasado si, en lugar de simular 1 millón de manos se hubieran simulado 10 mil? 
#' Haga la prueba. Realice una simulación de 10 mil manos y cuente cuántas manos únicas obtuvo.


###### Ejecutar desde aqui #################################
repeticiones <- 10000
manos_2 <- replicate(repeticiones, {
  mano <- sample(mazo, 3, replace=FALSE)
  manos <- mano[order(mano)] # Ordena alfabeticamente las cartas
})
ncol(unique(manos_2, MARGIN = 2))
###### Hasta aqui ##########################################


#'¿Obtuvo los mismos resultados? ¿Qué simulación es más confiable, la de 1 millón de casos o la de 10 mil? ¿Por qué?

#' Vayamos ahora a la probabilidad de que salgan cartas específicas ¿Cuál es la probabilidad 
#' de que me toque el 1 de espadas? De nuevo, esto se puede calcular de manera analítica o 
#' mediante simulaciones.

#' Empecemos contando cuántas veces apareció el 1 de espadas en la simulación de 1 millón de casos:
  
###### Ejecutar desde aqui #################################
carta_buscada <- t(manos) %>%
  as.data.frame() %>%
  filter(V1 == "1 Espadas" | V2 == "1 Espadas" | V3 == "1 Espadas")
cat("La cantidad de manos simuladas en las que salió el 1 de espadas es:",nrow(carta_buscada)) 
cat("La proporción de manos simuladas en las que salió el 1 de espadas es:",nrow(carta_buscada)/ncol(manos)) 
###### Hasta aqui ##########################################


#' Hagamos ahora el cálculo analítico. ¿Cuál es el valor teórico de la probabilidad 
#' de que toque el 1 de espadas? 
#' Si de un mazo de 40 cartas sacamos una de ellas, la probabilidad de que sea el 1
#'  de espadas es 1/40. Pero cada mano consta de 3 cartas, por lo que el 1 de espada
#'   puede salir en la primera, la segunda o la tercer carta.


###### Ejecutar desde aqui #################################
cat("La probabilidad teórica de que salga el 1 de espadas es", (1/40)*3)
###### Hasta aqui ##########################################


#' Seguramente los resultados de la simulación no resultaron exactamente iguales 
#' a la proporción calculada analíticamente. ¿Son suficientemente cercanos? 
#' ¿O debemos sospechar que calculamos mal la probabilidad de que salga el 
#' 1 de espadas en una mano?
#' Hay distintas formas de responder a esta pregunta. Las más rigurosas, 
#' se verán en la segunda parte de la materia. Por el momento, podemos ver 
#' qué nos dice la distribución Binomial.
#' Pensemos en un evento binario: sale el 1 de espadas en una mano vs. no sale 
#' el 1 de espadas en una mano. La probabilidad de estos resultados es siempre 
#' la misma, independientemente de los resultados previos. Por lo tanto, 
#' podemos definir la siguiente variable:

#'   X:"cantidad de manos en que sale el 1 de espadas"
#'   X~Binomial(n,p) 

#' En este caso, n es 1 millón y p es la probabilidad calculada analíticamente.
#' ¿Cuál es el valor esperado de esta variable aleatoria?
  

###### Ejecutar desde aqui #################################
valor_esperado <- 1000000*(1/40)*3
valor_esperado
###### Hasta aqui ##########################################


#' ¿Cuál es el desvío standard de esta variable aleatoria?
 
###### Ejecutar desde aqui #################################
desvio <- sqrt(1000000*(1/40)*3*(1-(1/40)*3)) 
desvio
###### Hasta aqui ##########################################


#' Volviendo a la cantidad de veces que salió el 1 de espadas 
#' en la simulación de 1 millón de manos, ¿a cuántos desvíos 
#' standards se encuentra del valor esperado de X? Es decir, 
#' calcule el valor z del resultado obtenido en la simulación.


###### Ejecutar desde aqui #################################
valor_z <- (nrow(carta_buscada)-valor_esperado)/desvio
valor_z
###### Hasta aqui ##########################################


#' En base a lo anterior, ¿cree que los resultados obtenidos 
#' en la simulación son consistentes con lo anticipado con la teoría? 
#' Justifique.