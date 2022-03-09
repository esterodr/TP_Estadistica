########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 3 #########################
########################################################


#' En este ejercicio utilizaremos datos de la base “Gapminder”, 
#' la cual contiene distintos indicadores socioeconómicos para una gran cantidad de países. 
#'  (https://www.gapminder.org/data/documentation/).


###### Ejecutar desde aqui #################################
library(gapminder)
library(tidyverse)
datos <- gapminder
# Cambiamos el nombre de las variables a idioma español
names(datos) <- c("Pais", "Continente", "Año", "Expectativa_de_vida", "Poblacion", "PIB_per_capita")
###### Hasta aqui ##########################################


#' Luego de descargar los datos, realizar una exploración inicial de los mismos. 
#' Por ejemplo, explore las primeras 10 y últimas 10 líneas de la base.


###### Ejecutar desde aqui #################################
# Primeros 10 datos de la base:
head(datos,10)
# Últimos 10 datos de la base:
tail(datos,10)
###### Hasta aqui ##########################################


#' Observamos que hay datos de distintos países para distinto año. En particular, 
#' hay información sobre la expectativa de vida, el PIB per cápita y la cantidad 
#' de habitantes en cada país para cada año. También hay una variable que nos 
#' indica a qué continente pertenece cada país.

#' ¿De cuántos países distintos hay información?


###### Ejecutar desde aqui #################################
unique(datos$Pais)
cantidad_de_paises <- length(unique(datos$Pais))
cantidad_de_paises
###### Hasta aqui ##########################################


#' ¿Para qué años hay datos disponibles?
 

###### Ejecutar desde aqui #################################
unique(datos$Año)
###### Hasta aqui ##########################################


#' Para este ejercicio, utilizaremos datos sólo del año 2007


###### Ejecutar desde aqui #################################
datos_2007 <- datos %>% filter(Año==2007)
###### Hasta aqui ##########################################



#' #### Ejercicio 3. a)
 
#' Comenzaremos a explorar la variable 'Expectativa de vida'. 
#' Indicar medidas de centralidad y de dispersión de la variable
 
#' Puede utilizar los siguientes comandos 


###### Ejecutar desde aqui #################################
summary(datos_2007$Expectativa_de_vida)
var(datos_2007$Expectativa_de_vida)
sd(datos_2007$Expectativa_de_vida)
###### Hasta aqui ##########################################

 
 
#' #### Ejercicio 3. b)
#' Realizar el histograma y el boxplot de la variable 'Expectativa de vida' en 2007
 
#### Histograma


###### Ejecutar desde aqui #################################
datos_2007 %>% ggplot() +
  geom_histogram(aes(Expectativa_de_vida), binwidth = 5, fill="cyan", color="black") +
  theme_classic() +
  labs(x="Expectativa de vida en años", y="Cantidad de países",
       title="Histograma de Exp. de Vida") +
  scale_x_continuous(breaks=seq(35,85,5)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#### Boxplot


###### Ejecutar desde aqui #################################
datos_2007 %>% ggplot() +
  geom_boxplot(aes(y=Expectativa_de_vida), fill="cyan") +
  theme_classic() +
  labs(y="Expectativa de vida en años", title = "Boxplot de Exp. de Vida") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' ¿Cómo clasificaría a esta distribución: simétrica, sesgada a la izquierda o sesgada a la derecha?

#' ¿Hay valores atípicos?

#' ¿Entre qué valores se encuentra la mayoría de los países?


#' #### Ejercicio 3. c)

#' Calcule el valor z de cada país, para la variable 'Expectativa de vida' en 2007


###### Ejecutar desde aqui #################################
valores_z <- datos_2007 %>%
  mutate(valor_z = (Expectativa_de_vida-mean(Expectativa_de_vida))/sd(Expectativa_de_vida)) %>%
  select(Pais, valor_z)
###### Hasta aqui ##########################################


#' A modo de ejemplo ¿Cuál es el valor z de Angola? ¿Y el de Francia?


###### Ejecutar desde aqui #################################
# Primeros 10 valores
head(valores_z,10)
# Valor z de Francia:
valores_z %>% filter(Pais == "France")
###### Hasta aqui ##########################################


#' ¿Qué significa tener un valor z negativo? ¿Y uno positivo?

#' ¿Cómo se interpreta el valor z de 0.689 que tiene Argentina?
 
#' ¿Qué significaría que alguien tenga un valor z igual a 0?
 
#' Calcule la proporción de países que tienen un valor z entre -1 y 1.
#'  Haga lo mismo para los intervalos (-2,2) y (-3,3). Represente los resultados en un gráfico
 

###### Ejecutar desde aqui #################################
prop_1 <- mean(abs(valores_z$valor_z)<1)
prop_2 <- mean(abs(valores_z$valor_z)<2)
prop_3 <- mean(abs(valores_z$valor_z)<3)
valores_z %>% ggplot() +
  geom_point(aes(x=valor_z, y=0), color="red", alpha=0.5) +
  geom_errorbarh(aes(y=0.05, xmin=-1, xmax=1), height=0.05, color="blue", size=2) +
  geom_errorbarh(aes(y=0.15, xmin=-2, xmax=2), height=0.05, color="blue", size=2) +
  geom_errorbarh(aes(y=0.25, xmin=-3, xmax=3), height=0.05, color="blue", size=2) +
  geom_vline(xintercept = 0) +
  annotate("text", x=0.0, y=0.1, label=paste0(round(100*prop_1,1),"% entre -1 y 1")) +
  annotate("text", x=0.0, y=0.2, label=paste0(round(100*prop_2,1),"% entre -2 y 2")) +
  annotate("text", x=0.0, y=0.3, label=paste0(round(100*prop_3,1),"% entre -3 y 3")) +
  theme_classic() +
  ylim(-0.1,0.4) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted")) +
  scale_x_continuous(breaks=seq(-3,3,1)) +
  labs(x="Valores z", y="", title="Valores z de Exp. de Vida")
###### Hasta aqui ##########################################


#' Observe el gráfico y analice si se cumple el Teorema de Chebyshev
#' ¿Es normal que no haya valores fuera del rango (-3,3)?

#' #### Ejercicio 3. d)

#' Realizar el histograma y el boxplot de la variable 'Expectativa de vida' para Europa y África
 
#### Histograma


###### Ejecutar desde aqui #################################
datos_2007 %>% filter(Continente %in% c("Africa","Europe")) %>%
  ggplot(aes(x=Expectativa_de_vida, fill=Continente)) +
  geom_histogram(binwidth = 5, alpha=.5, position="identity", color="black") + theme_classic() +
  labs(x="Expectativa de vida en años", y="Cantidad de Países",
       title="Histograma de Exp. de Vida") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#### Boxplot


###### Ejecutar desde aqui #################################
datos_2007 %>% filter(Continente %in% c("Africa","Europe")) %>%
  ggplot(aes(x=Continente, y=Expectativa_de_vida)) + geom_boxplot(fill="cyan") + theme_classic() +
  labs(y="Expectativa de vida en años", title="Boxplot de Exp. de Vida") +
  theme(panel.grid.major.y = element_line(linetype = "dotted"))
###### Hasta aqui ##########################################


#' ¿Observa alguna diferencia entre ambos continentes? ¿Cuáles?

#' ¿En qué continente la expectativa de vida es mayor?
 
#' ¿En qué continente la expectativa de vida es más 'dispersa'?


#' #### Ejercicio 3. e)

#' Incorporemos al análisis la variable PIB per cápita, la cual puede tomarse como un indicador 
#' del desarrollo económico. 
 
#' Realizar un diagrama de dispersión de las variables 'Expectativa de vida' y 'PIB_per_cápita'
#' Calcular el coeficiente de correlación entre estas dos variables


###### Ejecutar desde aqui #################################
datos_2007 %>% ggplot(aes(x=PIB_per_capita, y=Expectativa_de_vida)) +
  geom_point() +
  annotate("text", x=30000, y=60,
           label=paste("Coef. de correlación=",round(cor(datos_2007$PIB_per_capita,datos_2007$Expectativa_de_vida),2))) +
  theme_classic() +
  labs(x="PIB per cápita (dólares)", y="Expectativa de vida",
       title="Gráfico de Dispersión") +
  theme(panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_line(linetype = "dotted"))
###### Hasta aqui ##########################################


#' ¿Encuentra alguna relación entre estas variables? ¿Puede haber alguna relación de causalidad?

#' Realice el mismo gráfico de dispersión pero sólo para Europa y África


###### Ejecutar desde aqui #################################
datos_2007 %>% filter(Continente %in% c("Africa","Europe")) %>%
  ggplot(aes(x=PIB_per_capita, y=Expectativa_de_vida)) +
  geom_point(aes(color=Continente)) +
  theme_classic() +
  labs(x="PIB per cápita (dólares)", y="Expectativa de vida",
       title="Gráfico de Dispersión") +
  theme(panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_line(linetype = "dotted"))
###### Hasta aqui ##########################################


#' Viendo este gráfico, ¿Qué opina sobre la relación entre las variables
#' Expectativa de Vida y PIB per cápita? ¿Es igual para los dos continentes?

#' Calcule el coeficiente de correlación entre estas dos variables, 
#' pero diferenciando entre Europa y África.
 

###### Ejecutar desde aqui #################################
datos_2007 %>% filter(Continente %in% c("Africa","Europe")) %>%
  group_by(Continente) %>% 
  summarise(Coef_Correlacion = cor(Expectativa_de_vida, PIB_per_capita)) %>% 
  ungroup()
###### Hasta aqui ##########################################


#' ¿Qué puede comentar de estos resultados?