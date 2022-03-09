########################################################
## TP de Estadística ###################################
## Curso de Tamara Burdisso - FCE UBA ##################
########################################################
## Primera Parte - Ejercicio 2 #########################
########################################################


#' El 11 de Agosto de 2019 se realizaron las elecciones primarias para las 
#' presidenciales en Argentina. El resultado, aparentemente no era lo anticipado 
#' por el mercado, por lo que el 12 de Agosto de 2019 se produjo una fuerte caída 
#' en el índice MERVAL (índice que reúne las principales empresas que cotizan en 
#' la Bolsa de Comercio local).

#' En este ejercicio analizaremos qué tan excepcional fue el evento.

#' Para comenzar, cargamos la serie histórica de precios del índice MERVAL y
#' veamos un simple gráfico de la serie. La línea punteada indica la fecha de referencia.


###### Ejecutar desde aqui #################################
library(tidyverse) ## Cargamos un paquete de funciones necesario para el ejercicio
load("./archivos/MERVAL.Rda")
###### Hasta aqui ##########################################


#' Para comenzar, veamos un simple gráfico de la serie.
#' Sólo se visualizarán datos desde 2019.
#' La línea roja indica la fecha de referencia


###### Ejecutar desde aqui #################################
MERVAL %>% filter(Fecha>="2019-01-01" & Fecha<="2019-12-31") %>%
  ggplot(aes(x=Fecha, y=MERVAL)) +
  geom_line() +
  geom_line(data = . %>% filter(Fecha %in% as.Date(c("2019-08-09","2019-08-12"))), color="red", size=1) +
  labs(title="Índice MERVAL desde 2019") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.minor.x = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' No nos interesa trabajar con el valor diario del índice, 
#' sino con las variaciones porcentuales diarias. 


###### Ejecutar desde aqui #################################
MERVAL$Rendimientos <- c(NA,100*(diff(MERVAL$MERVAL)/MERVAL$MERVAL[1:nrow(MERVAL)-1]))
###### Hasta aqui ##########################################


#' Para visualizar lo que se acaba de hacer, transcriba el 
#' valor del índice MERVAL y la variación porcentual diaria 
#' para los primeros días de julio de 2020:


###### Ejecutar desde aqui #################################
MERVAL %>% filter(Fecha>="2020-07-01", Fecha <="2020-07-07")
###### Hasta aqui ##########################################

#'Redondeando a 2 decimales, ¿qué significa el valor de 9,01
#' en la columna Rendimientos para el 6 de julio de 2020? 
#' ¿y qué significa el valor de -1,94 en la columna Rendimientos
#'  para el 7 de julio de 2020?

#' Calcular indicadores de centralidad y dispersión para el rendimiento diario del índice 
#' Pueden ser útiles los siguientes comandos


###### Ejecutar desde aqui #################################
summary(MERVAL$Rendimientos)
var(MERVAL$Rendimientos, na.rm = TRUE)
sd(MERVAL$Rendimientos, na.rm = TRUE)
###### Hasta aqui ##########################################


#' Dibujar histograma y boxplot de los rendimientos

#### Histograma

###### Ejecutar desde aqui #################################
MERVAL %>% ggplot(aes(x=Rendimientos)) +
  geom_histogram(binwidth = 0.05, alpha=.5, position="identity", fill="cyan", color="black") +
  theme_classic() +
  labs(y="Frecuencia Absoluta", title="Rendimientos diarios del MERVAL") 
###### Hasta aqui ##########################################


#### Boxplot

###### Ejecutar desde aqui #################################
MERVAL %>%
  ggplot(aes(y=Rendimientos)) + 
  geom_boxplot(fill="cyan") + 
  theme_classic() +
  labs(title="Rendimientos diarios del MERVAL") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Ahora vayamos al evento que nos interesa estudiar.
#' ¿De cuánto fue la caída que se produjo el 12 de Agosto de 2019?


###### Ejecutar desde aqui #################################
MERVAL$Rendimientos[MERVAL$Fecha=="2019-08-12"]
###### Hasta aqui ##########################################


#' ¿Cuál es el valor z de esta caída?
 

###### Ejecutar desde aqui #################################
media <- mean(MERVAL$Rendimientos, na.rm = TRUE)
desvio <- sd(MERVAL$Rendimientos, na.rm = TRUE)
valor_z <- (MERVAL$Rendimientos[MERVAL$Fecha=="2019-08-12"] - media) / desvio 
valor_z
###### Hasta aqui ##########################################


#' Analicemos si se cumple con el Teorema de Chebyshev
#' Calcular la cantidad de observaciones que, según Chebyshev, debería
#' haber cómo mínimo a k desvíos standard de la media. Tome valores de k
#' entre 1 y 18.
#' Calcule el porcentaje de días que el rendimiento del MERVAL estuvo a k
#' desvíos standards de la media (k de 1 a 18).


###### Ejecutar desde aqui #################################
chebyshev <- data.frame(k=c(2:18)) %>%
  mutate(Limite_Chebyshev = 1-1/k^2,
         Porcentaje_Observaciones = c(mean(abs((MERVAL$Rendimientos-media)/desvio)<=2, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=3, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=4, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=5, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=6, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=7, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=8, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=9, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=10, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=11, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=12, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=13, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=14, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=15, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=16, na.rm = TRUE),mean(abs((MERVAL$Rendimientos-media)/desvio)<=17, na.rm = TRUE),
         mean(abs((MERVAL$Rendimientos-media)/desvio)<=18, na.rm = TRUE)),
         Cantidad_Observaciones = c(sum(abs((MERVAL$Rendimientos-media)/desvio)<=2, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=3, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=4, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=5, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=6, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=7, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=8, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=9, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=10, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=11, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=12, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=13, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=14, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=15, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=16, na.rm = TRUE),sum(abs((MERVAL$Rendimientos-media)/desvio)<=17, na.rm = TRUE),
                                    sum(abs((MERVAL$Rendimientos-media)/desvio)<=18, na.rm = TRUE)))
chebyshev
###### Hasta aqui ##########################################


#' Preste atención a los resultados anteriores. Note que, a partir de cierto valor de k,
#' la cantidad de observaciones se mantiene constante. ¿Qué significa esto? 

#' Grafique los resultados anteriores
 

###### Ejecutar desde aqui #################################
chebyshev %>%  
  select(-Cantidad_Observaciones) %>%
  gather(Tipo, Valor, -k) %>%
  ggplot(aes(x=k, color=Tipo)) +
  geom_point(aes(y=Valor), size=2) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(2:18)) +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_line(linetype = "dotted")) +
  labs(title="Chebyshev en el MERVAL")
###### Hasta aqui ##########################################


#' ¿Se cumple con el Teorema de Chebyshev?

#' Luego de este análisis, ¿Qué tan raro fue el evento del 12 de Agosto? 

