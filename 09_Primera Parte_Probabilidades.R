########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 9 #########################
########################################################


#' En este ejercicio simularemos un bolillero con 2 bolillas blancas y 5 negras
 
#' Creamos el bolillero y comprobamos que tenga la composición adecuada:
 

###### Ejecutar desde aqui #################################
library(tidyverse)
bolillero <- c(rep("blanca",2), rep("negra",5))
bolillero
###### Hasta aqui ##########################################


#' #### Ejercicio 9.a)

#' Realizar 3 extracciones **sin reposición**
#' ¿Qué resultados obtuvo?
 

###### Ejecutar desde aqui #################################
extraccion_sin_r <- sample(bolillero, 3, replace=FALSE)
extraccion_sin_r
cat("Se extrajeron" ,sum(extraccion_sin_r=="blanca"), "bolillas blancas")
cat("Se extrajeron" ,sum(extraccion_sin_r=="negra"), "bolillas negras")
###### Hasta aqui ########################################## 

 
#' ¿Cuántos resultados distintos son posibles en 3 extracciones sin reposición?
 
#' ¿Cuál es la probabilidad de cada uno de estos resultados? Haga el cálculo de
#' manera analítica. Más adelante realizaremos el cálculo mediante una simulación.

#' Repetir 1 millón de veces la extracción de 3 bolillas sin reposición y contar la 
#' cantidad de bolillas blancas (las bolillas negras van a ser 3 - cantidad de bolillas 
#' blancas)
#' Mostrar los primeros 10 resultados


###### Ejecutar desde aqui #################################
repeticiones <- 1000000
resultados_sin_r <- replicate(repeticiones, {
  extraccion_sin_r <- sample(bolillero, 3, replace=FALSE)
  cantidad_blancas <- sum(extraccion_sin_r=="blanca")
  resultados_sin_r <- cantidad_blancas})
head(resultados_sin_r,10)
###### Hasta aqui ##########################################
 

#' Mostrar los resultados unicos y sus probabilidades estimadas en base a la simulación.


###### Ejecutar desde aqui #################################
prob_estimadas_sin_r <- sapply(unique(resultados_sin_r), function(x) {
  prob <- mean(resultados_sin_r==x)
  texto <- paste(x,"blancas con probabilidad estimada de", round(prob,4))
  prob_estimadas_sin_r <- texto
  prob_estimadas_sin_r
}) 
prob_estimadas_sin_r
###### Hasta aqui ##########################################

 
#' Graficar los resultados en un histograma de frecuencias relativas


###### Ejecutar desde aqui #################################
resultados_sin_r %>% as.data.frame() %>%
  ggplot(aes(.)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, fill="cyan", color="black") +
  xlab("Cantidad de bolillas blancas") +
  theme_classic() +
  ggtitle("Proporción de bolillas blancas en 3 extracciones\n SIN REPOSICIÓN") +
  ylab("Frecuencia Relativa") +
  theme(panel.background = element_blank(),
             panel.grid.major.y = element_line(linetype = "dotted"),
             panel.ontop = TRUE)
###### Hasta aqui ########################################## 


#' Comparar los resultados obtenidos con las probabilidades calculadas analíticamente.


#' #### Ejercicio 9.b)

#' Repetir las 3 extracciones pero con **reposición**
#' ¿Qué resultados obtuvo? 


###### Ejecutar desde aqui #################################
extraccion_con_r <- sample(bolillero, 3, replace=TRUE)
extraccion_con_r
cat("Se extrajeron" ,sum(extraccion_con_r=="blanca"), "bolillas blancas")
cat("Se extrajeron" ,sum(extraccion_con_r=="negra"), "bolillas negras")
###### Hasta aqui ########################################## 


#' ¿Cuántos resultados distintos son posibles con reposición?
 
#' ¿Cuál es la probabilidad de cada uno de estos resultados? Haga el cálculo de
#' manera analítica. Más adelante realizaremos el cálculo mediante una simulación.

#' ¿Qué diferencias hay con la extracción sin reposición del ejercicio 9.a)?

#' Repetir 1 millón de veces la extracción de 3 bolillas con reposición y contar la 
#' cantidad de bolillas blancas (las bolillas negras van a ser 3 - cantidad de bolillas blancas)
#' Mostrar los primeros 10 resultados


###### Ejecutar desde aqui #################################
repeticiones <- 1000000
resultados_con_r <- replicate(repeticiones, {
  extraccion_con_r <- sample(bolillero, 3, replace=TRUE)
  cantidad_blancas <- sum(extraccion_con_r=="blanca")
  resultados_con_r <- cantidad_blancas})
head(resultados_con_r,10)
###### Hasta aqui ##########################################

 
#' Mostrar los resultados unicos y sus probabilidades estimadas mediante la simulación


###### Ejecutar desde aqui #################################
prob_estimadas_con_r <- sapply(unique(resultados_con_r), function(x) {
  prob <- mean(resultados_con_r==x)
  texto <- paste(x,"blancas con probabilidad estimada de", round(prob,4))
  prob_estimadas_con_r <- texto
  prob_estimadas_con_r
}) 
prob_estimadas_con_r
###### Hasta aqui ##########################################


#' Graficar los resultados en un histograma de frecuencias relativas


###### Ejecutar desde aqui #################################
resultados_con_r %>% as.data.frame() %>%
  ggplot(aes(.)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1, fill="cyan", color="black") +
  xlab("Cantidad de bolillas blancas") +
  theme_classic() +
  ggtitle("Proporción de bolillas blancas de 3 extracciones\n CON REPOSICIÓN") +
  ylab("Frecuencia Relativa") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Comparar los resultados obtenidos con las probabilidades teóricas

#' #### Ejercicio 9.c)

#' En base a los resultados de los ejercicios 9.a) y 9.b), explique las diferencias entre
#'  una extracción *sin reposición* y una *con reposición*.
