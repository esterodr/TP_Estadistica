########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 1 #########################
########################################################

#' Una práctica habitual de los principales bancos centrales del mundo es realizar un 
#' **relevamiento sistemático de los principales pronósticos macroeconómicos de corto y mediano plazo**
#' que distintos analistas realizan sobre la evolución de la economía. Además de contribuir 
#' con la política de transparencia en la comunicación, la información que proporciona este
#' tipo de encuestas resulta de **gran relevancia para las decisiones de política monetaria y económica**.
#' Adicionalmente, esta información es relevante para las decisiones de consumo e inversión,
#' constituyéndose como un bien público al proveer a la comunidad la mejor información posible
#' respecto de las estimaciones que realizan los especialistas sobre el comportamiento futuro
#' de las principales variables económicas.
#' 
#' El Banco Central de la República Argentina (BCRA) publica todos los meses el 
#' **Relevamiento de Expectativas de Mercado (REM)**. Para ello, en los últimos 3 días hábiles
#' de cada mes realiza un relevamiento sobre las expectativas de los precios minoristas,
#' la tasa de interés, el tipo de cambio nominal, el nivel de actividad económica y el resultado
#' primario del sector público nacional no financiero. Para mayor información sobre el REM, visitar el 
#' siguiente link: 
#' 
#' http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/Consideraciones%20del%20Relevamiento%20de%20Expectativas%20de%20Mercado%20(REM).pdf
#'    
#' A continuación, se presenta una simulación de los pronósticos que 48 agentes tenían en
#' diciembre de 2018 sobre la inflación anual de 2019. Las respuestas de cada agente se almacenan
#' en el objeto 'inflacion_2019':
#' 

###### Ejecutar desde aqui #################################
library(tidyverse) ## Cargamos un paquete de funciones necesario para el ejercicio
inflacion_2019 <- c(31.5, 28.5, 27.5, 27.0, 31.5, 36.0, 29.0, 26.0, 28.5, 28.5, 27.0, 31.0, 31.0, 27.5,
                    30.0, 26.0, 27.0, 41.0, 26.5, 34.5, 28.5, 25.5, 30.0, 26.5, 31.0, 30.5, 32.0, 33.0,
                    28.0, 28.0, 29.0, 28.0, 26.0, 24.6, 29.5, 30.0, 34.5, 25.0, 30.0, 26.0, 27.0, 25.0,
                    34.5, 26.0, 28.5, 28.0, 330, 30.0)
###### Hasta aqui ##########################################


#' El ejercicio consiste en analizar qué esperan estos agentes sobre la tasa de inflación de 2019.

##### Ejercicio a)

#' Calcular las siguientes medidas de centralidad: Media, Mediana, Moda.
#' 
#' Calcular las siguientes medidas de dispersion: Rango, Rango intercuartilico, Varianza y Desvio Standard.
#' 
#' Las siguientes funciones pueden ser utiles para calcular estos valores:

###### Ejecutar desde aqui #################################
summary(inflacion_2019) # Esta función calcula máximo, mínimo, Q1, Q3, media y mediana
var(inflacion_2019) # Esta función calcula la varianza
sd(inflacion_2019) # Esta función calcula el desvío standard
###### Hasta aqui ##########################################


#' Calcular una tabla de frecuencias. ¿Cuáles son los 5 pronósticos más frecuentes?

###### Ejecutar desde aqui #################################
tibble(inflacion_2019=inflacion_2019) %>%
  count(inflacion_2019) %>% 
  arrange(desc(n))
###### Hasta aqui ##########################################
 
#' Dibujar histograma y boxplot de las expectativas de inflación.
#' 

##### Histograma:

###### Ejecutar desde aqui #################################
inflacion_2019 %>% as.data.frame() %>%
  ggplot() +
  geom_histogram(aes(inflacion_2019), binwidth = 1, fill="cyan", color="black") +
  theme_classic() +
  labs(x="Inflación en 2019", y="Frecuencia Absoluta", 
       title="Histograma de Expectativas de Inflación") +
  scale_x_continuous(breaks=seq(0,350,25))
###### Hasta aqui ##########################################

##### Boxplot:
 
###### Ejecutar desde aqui #################################
inflacion_2019 %>% as.data.frame() %>%
  ggplot() +
  geom_boxplot(aes(y=inflacion_2019), fill="cyan") +
  theme_classic() +
  labs(x="", y="Pronóstico de Inflación para 2019", 
       title="Boxplot de Expectativas de Inflación") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
###### Hasta aqui ##########################################

#' ¿Identifica algún valor que pueda considerarse atípico? ¿Cuál?

 
##### Ejercicio b)
 
#' Al analizar los resultados y gráficos anteriores, se sospecha de un error al ingresar los datos.
#' En efecto, la respuesta número 47 fue registrada como 330, cuando en realidad debería figurar 33.0.

#' Para corregir este error, crearemos un nuevo objeto 'inflacion_2019_corregida' con el dato correcto.

###### Ejecutar desde aqui #################################
inflacion_2019_corregida <- inflacion_2019
inflacion_2019_corregida[47] <- 33.0
###### Hasta aqui ##########################################


#' Repita el calculo de medidas de centralidad y dispersion.
#' ¿Cuáles se modifican y cuales quedan igual? 
#' Tambien realice nuevamente el histograma y el boxplot.
#' Explique las diferencias que observa con el ejercicio a)
#' 

###### Ejecutar desde aqui #################################
summary(inflacion_2019_corregida)
var(inflacion_2019_corregida)
sd(inflacion_2019_corregida)
###### Hasta aqui ##########################################

#### Histograma

###### Ejecutar desde aqui #################################
inflacion_2019_corregida %>% as.data.frame() %>%
  ggplot() +
  geom_histogram(aes(inflacion_2019_corregida), binwidth = 1, fill="cyan", color="black") +
  theme_classic() +
  labs(x="Inflación en 2019", y="Frecuencia Absoluta", 
       title="Histograma de Expectativas de Inflación") +
  scale_x_continuous(breaks=seq(0,45,5)) +
  scale_y_continuous(breaks = seq(0,10,2)) +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

#### Boxplot

###### Ejecutar desde aqui #################################
inflacion_2019_corregida %>% as.data.frame() %>%
  ggplot() +
  geom_boxplot(aes(y=inflacion_2019_corregida), fill="cyan") +
  theme_classic() +
  labs(x="", y="Pronóstico de Inflación para 2019", 
       title="Boxplot de Expectativas de Inflación") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##### Ejercicio c)

#' Viendo los resultados de a) y b), explique el significado de la siguiente frase:

#' 'La mediana y el RIC son robustas a la ocurrencia de valores atipicos, mientras que la media y la
#'  varianza no lo son'


##### Ejercicio d)

#' Supongamos que un grupo de 15 agentes envió tarde su respuesta, por lo que no fueron incluidos
#' originalmente en el REM. El BCRA decide actualizar su publicación incorporando estos nuevos 15 pronósticos.
#' Sorprendentemente, los 15 pronósticos son iguales: todos esperan una inflación del 30% para 2019.
#' Incorpore estas notas al objeto inflacion_2019_corregida:
#' 

###### Ejecutar desde aqui #################################
inflacion_2019_corregida <- c(inflacion_2019_corregida, rep(30,15))
###### Hasta aqui ##########################################

 
#' Revise que se han incorporado las notas correctamente:

###### Ejecutar desde aqui #################################
inflacion_2019_corregida
###### Hasta aqui ##########################################


#' Repita el cálculo de los indicadores de centralidad y dispersión.
#' También realice histograma y boxplot.
#' Compare con los resultados del ejercicio b)
#' ¿Qué efectos tiene que se hayan incorporado 15 pronósticos iguales?
#' 

###### Ejecutar desde aqui #################################
summary(inflacion_2019_corregida)
var(inflacion_2019_corregida)
sd(inflacion_2019_corregida)
###### Hasta aqui ##########################################

#### Histograma

###### Ejecutar desde aqui #################################
inflacion_2019_corregida %>% as.data.frame() %>%
  ggplot() +
  geom_histogram(aes(inflacion_2019_corregida), binwidth = 1, fill="cyan", color="black") +
  theme_classic() +
  labs(x="Inflación en 2019", y="Frecuencia Absoluta", 
       title="Histograma de Expectativas de Inflación") +
  scale_x_continuous(breaks=seq(0,45,5)) +
  scale_y_continuous(breaks = seq(0,20,4)) +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

#### Boxplot

###### Ejecutar desde aqui #################################
inflacion_2019_corregida %>% as.data.frame() %>%
  ggplot() +
  geom_boxplot(aes(y=inflacion_2019_corregida), fill="cyan") +
  theme_classic() +
  labs(x="", y="Pronóstico de Inflación para 2019", 
       title="Boxplot de Expectativas de Inflación") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

# Interpretar los resultados.

# Fin del ejercicio. 