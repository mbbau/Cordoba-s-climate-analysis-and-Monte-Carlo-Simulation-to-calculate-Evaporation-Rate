# En este link se explica como poder analizar por mes una dataset historica
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/


#Librerias ----


library(tidyverse)
library(lubridate)
library(skimr)
library(ggridges)
library(viridis)
library(ggrepel)
library(readxl)
library(ggtext)

datos_observatorio <- read_excel("Observatorio.xlsx")

#Transofmación del archivo ----

## Data Type Transformation ----
## Primero, transformo el formato de los datos del data frame a los tipos que corresponden.

names(datos_observatorio)[10] <- "vto_med_veloc" 

datos_observatorio$fecha <- as.Date(datos_observatorio$fecha)
datos_observatorio$tmax <- as.double(datos_observatorio$tmax)
datos_observatorio$tmin <- as.double(datos_observatorio$tmin)
datos_observatorio$tmed <- as.double(datos_observatorio$tmed)
datos_observatorio$prcp <- as.double(datos_observatorio$prcp)
datos_observatorio$hum_relat <- as.double(datos_observatorio$hum_relat)
datos_observatorio$vto_med_veloc <- as.double(datos_observatorio$vto_med_veloc)
datos_observatorio$vto_med_veloc <- as.double(datos_observatorio$vto_med_veloc)
datos_observatorio$vto_max_veloc <- as.double(datos_observatorio$vto_max_veloc)
datos_observatorio$vto_max_direcc <- as.double(datos_observatorio$vto_max_direcc)


# Ahora que son date type, voy a crear una columna que tenga el nombre del mes al cual el dato pertenece
# ya que me interesa la distribución mensual de los parámetros en el período analizado.

## Data shape transformation ----

datos_observatorio <- datos_observatorio %>%
  mutate(month = month(fecha))

skim_without_charts(datos_observatorio)

# poder analizar los dtos por año también parece una buena idea, por lo que voy a agregar
# una columna con el año.
# Además la amplitud termica tambien parece ser un buen dato a analizar.

datos_observatorio <- datos_observatorio %>%
  mutate(year = year(fecha)) %>%
  mutate(amplitud_termica = (tmax - tmin))

# Ahora voy a graficar las distribuciones mensuales de las 3 variables que nos interesan para
# calcular la tasa de evaporación del hormigón.
# Datos sobre la paleta de colores se pueden encontrar en este link:
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

# Gráfico de distribuciones mensuales de humedad relativa ----

hum_relat_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = hum_relat, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Hum. Relat. [%]", option = "mako") +
  labs(title = 'Distribución de Humedades relativas mensuales en Córdoba',
       subtitle = "Distribución de registro de humedades relativas medias de Córdoba desde 1961 a 2021",
       x = "Humedad Relativa %",
       y = "Mes") +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 10),
    plot.title.position = "plot")

# Gráfico de distribuciones mensuales de velocidad de viento ----

vto_max_veloc_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = vto_max_veloc, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Hum. Relat. [%]", option = "viridis") +
  labs(title = 'Distribución de Velocidades máximas de viento en Córdoba',
       subtitle = "Distribución de registro de velocidades de viento máximas de Córdoba desde 1961 a 2021",
       x = "Velocidad máxima del viento (m/seg)",
       y = "Mes") +
  theme_ridges() +
  xlim(0,35) + 
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 10),
    plot.title.position = "plot"
  )

# Gráfico de distribuciones mensuales de Temperaturas máximas ----

tmax_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = tmax, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "magma") +
  labs(title = 'Distribución de Temperaturas máximas mensuales en Córdoba',
       subtitle = "Distribución de registro de temperaturas máximas de Córdoba desde 1961 a 2021",
       x = "Temperatura máxima diaria en ºC",
       y = "Mes") +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 10),
    plot.title.position = "plot"
  )

plot(tmax_ridges)
plot(hum_relat_ridges)
plot(vto_max_veloc_ridges)


# Ahora que en función de los ridge plots hemos podido establecer que las distribuciones de los parámetros 
# que usaremos son normales, necesitaremos obtener mes a mes el promedio y el desvio de cada parámetro
# para poder realizar muestreos aleatorios y así calcular la tasa de evaporacion mes a mes.
# Ahora crearé un data frame que contenga resumenes mensuales de los parametros analizados

# Creación de tabla para simulaciones ----

datos_observatorio_mensual <- datos_observatorio %>%
  drop_na() %>%
  group_by(month) %>%
  summarise(tmax_promedio = mean(tmax),
            tmax_desvio = sd(tmax),
            tmin_promedio = mean(tmin),
            tmin_desvio = sd(tmin),
            tmed_promedio = mean(tmed),
            tmed_desvio = sd(tmed),
            hum_relat_promedio = mean(hum_relat),
            hum_relat_desvio = sd(hum_relat),
            vto_max_veloc_promedio = mean(vto_max_veloc),
            vto_max_veloc_desvio = sd(vto_max_veloc),
            vto_med_veloc_promedio = mean(vto_med_veloc),
            vto_med_veloc_desvio = sd(vto_med_veloc))

# Algunos links útiles sobre simulaciones:
# https://www.countbayesie.com/blog/2015/3/3/6-amazing-trick-with-monte-carlo-simulations
# https://www.rdocumentation.org/packages/compositions/versions/1.40-3/topics/rnorm
# https://www.youtube.com/watch?v=xuUMz8exU8Q
# El mejor de todos: https://www.youtube.com/watch?v=OgO1gpXSUzU&t=3s

# Para las fórmulas de la tasa de evaporación este link puede servir de referencia
# https://www.researchgate.net/publication/260209439_Plastic_Shrinkage_Cracking_and_Evaporation_Formulas

# Hare data frames para cada mes con los valores calculados de la tasa de evaporación
# utilizando la siguiente formula:
# E = 5 * ( [Tc + 18]^2,5 - r*[Ta + 18]^2,5)*(v + 4) * 10^-6

# Primera Simulación de MonteCarlo con temperatura igual a temperatura del hormigón ----

runs <- 10000
simulaciones <- data.frame()
datos_observatorio_simulados <- data.frame()

for (i in 1:12){
  temperaturas <- rnorm(runs, datos_observatorio_mensual[[i,2]], datos_observatorio_mensual[[i,3]])
  vientos <- rnorm(runs, datos_observatorio_mensual[[i,10]], datos_observatorio_mensual[[i,11]])
  humedades_relativas <- rnorm(runs, datos_observatorio_mensual[[i,8]], datos_observatorio_mensual[[i,9]])
  tasa_de_evaporacion_simulada <- 5*((temperaturas+18)^(2.5)-(humedades_relativas/100)*(temperaturas+18)^(2.5))*(vientos+4)*10^(-6)
  mes <- rep(i, 10000)
  simulaciones <- data.frame(mes,
                             temperaturas,
                             vientos,
                             humedades_relativas,
                             tasa_de_evaporacion_simulada)
  datos_observatorio_simulados <- rbind(datos_observatorio_simulados, simulaciones)
  i = i + 1
}

# Gráfico de las distribuciones simuladas ----

tasas_de_evaporacion_ridges <- ggplot(datos_observatorio_simulados) +
  geom_density_ridges_gradient(scale = 3.5,
                               rel_min_height = 0.01,
                               aes(x = tasa_de_evaporacion_simulada, y = as.factor(mes),
                                   fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "turbo") +
  labs(title = 'Distribución de tasas de evaporación mensuales en Córdoba',
       subtitle = "Distribución de simulación de tasas de evaporación para Córdoba a partir de datos de clima de 1961
a 2021, considerando que la temperatura del hormigón es aproximadamente igual a la ambiente.",
       x = "Tasa de evaporación ( Kg/m^2/hr)",
       y = "Mes") +
  theme_ridges() +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.5)) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 10),
    plot.title.position = "plot"
  )

plot(tasas_de_evaporacion_ridges)

# Ahora obtendré el promedio y el desvio mensual de las distribuciones 
# definidas en la simulación.

# Tabla Resumen de primera simulación ----

datos_observatorio_mensual_simulacion <- datos_observatorio_simulados %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio = sd(tasa_de_evaporacion_simulada))

datos_observatorio_mensual_simulacion <- datos_observatorio_mensual_simulacion %>%
  mutate(probabilidad_tasa_mayor_igual_0.5 = pnorm(0.5, mean = tasa_evaporacion_promedio, sd = tasa_evaporacion_desvio, lower.tail = FALSE)) %>%
  mutate(p80 = qnorm(0.8, mean = tasa_evaporacion_promedio, sd = tasa_evaporacion_desvio, lower.tail = TRUE))

names(datos_observatorio_mensual_simulacion)[1] <- "month"

## Gráfico de barras resultados primera simulación ----

probabilidades_mensuales_tasa_0.5 <- ggplot(datos_observatorio_mensual_simulacion)+
  geom_col(mapping = aes(x= month ,y = probabilidad_tasa_mayor_igual_0.5, fill = probabilidad_tasa_mayor_igual_0.5))+
  labs(title = "Probabilidades mensuales para tasas de Evaporación para Córdoba",
       subtitle = "Detalle de las probabilidades de que la tasa de evaporación para cada en Córdoba mes sea igual o mayor a 0,5 considerando
la temperatura del hormigón igual a la temperatura ambiente.
       ",
       x = "Mes",
       y = "Probabilidad")+
  theme_ridges() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10),
        plot.title.position = "plot")

plot(probabilidades_mensuales_tasa_0.5)

# Todo lo analizado anteriormente suponia que la temperatura del hormigón era similar
# a la temperatura ambiente. Ahora repetiré el analisis para cuatro temperaturas del
# hormigón fijas.

# Segunda Simulación de MonteCarlo con temperatura del hormigón constante igual a 20 grados centigrados ----

datos_observatorio_simulados_T20 <- data.frame()

for (i in 1:12){
  temperaturas <- rnorm(runs, datos_observatorio_mensual[[i,2]], datos_observatorio_mensual[[i,3]])
  vientos <- rnorm(runs, datos_observatorio_mensual[[i,10]], datos_observatorio_mensual[[i,11]])
  humedades_relativas <- rnorm(runs, datos_observatorio_mensual[[i,8]], datos_observatorio_mensual[[i,9]])
  tasa_de_evaporacion_simulada <- 5*((20+18)^(2.5)-(humedades_relativas/100)*(temperaturas+18)^(2.5))*(vientos+4)*10^(-6)
  mes <- rep(i, 10000)
  simulaciones <- data.frame(mes,
                             temperaturas,
                             vientos,
                             humedades_relativas,
                             tasa_de_evaporacion_simulada)
  datos_observatorio_simulados_T20 <- rbind(datos_observatorio_simulados_T20, simulaciones)
  i = i + 1
}

# Tercera Simulación de MonteCarlo con temperatura del hormigón constante igual a 25 grados centigrados ----

datos_observatorio_simulados_T25 <- data.frame()

for (i in 1:12){
  temperaturas <- rnorm(runs, datos_observatorio_mensual[[i,2]], datos_observatorio_mensual[[i,3]])
  vientos <- rnorm(runs, datos_observatorio_mensual[[i,10]], datos_observatorio_mensual[[i,11]])
  humedades_relativas <- rnorm(runs, datos_observatorio_mensual[[i,8]], datos_observatorio_mensual[[i,9]])
  tasa_de_evaporacion_simulada <- 5*((25+18)^(2.5)-(humedades_relativas/100)*(temperaturas+18)^(2.5))*(vientos+4)*10^(-6)
  mes <- rep(i, 10000)
  simulaciones <- data.frame(mes,
                             temperaturas,
                             vientos,
                             humedades_relativas,
                             tasa_de_evaporacion_simulada)
  datos_observatorio_simulados_T25 <- rbind(datos_observatorio_simulados_T25, simulaciones)
  i = i + 1
}

# Cuarta Simulación de MonteCarlo con temperatura del hormigón constante igual a 30 grados centigrados ----

datos_observatorio_simulados_T30 <- data.frame()

for (i in 1:12){
  temperaturas <- rnorm(runs, datos_observatorio_mensual[[i,2]], datos_observatorio_mensual[[i,3]])
  vientos <- rnorm(runs, datos_observatorio_mensual[[i,10]], datos_observatorio_mensual[[i,11]])
  humedades_relativas <- rnorm(runs, datos_observatorio_mensual[[i,8]], datos_observatorio_mensual[[i,9]])
  tasa_de_evaporacion_simulada <- 5*((30+18)^(2.5)-(humedades_relativas/100)*(temperaturas+18)^(2.5))*(vientos+4)*10^(-6)
  mes <- rep(i, 10000)
  simulaciones <- data.frame(mes,
                             temperaturas,
                             vientos,
                             humedades_relativas,
                             tasa_de_evaporacion_simulada)
  datos_observatorio_simulados_T30 <- rbind(datos_observatorio_simulados_T30, simulaciones)
  i = i + 1
}

# Quinta Simulación de MonteCarlo con temperatura del hormigón constante igual a 35 grados centigrados ----


datos_observatorio_simulados_T35 <- data.frame()

for (i in 1:12){
  temperaturas <- rnorm(runs, datos_observatorio_mensual[[i,2]], datos_observatorio_mensual[[i,3]])
  vientos <- rnorm(runs, datos_observatorio_mensual[[i,10]], datos_observatorio_mensual[[i,11]])
  humedades_relativas <- rnorm(runs, datos_observatorio_mensual[[i,8]], datos_observatorio_mensual[[i,9]])
  tasa_de_evaporacion_simulada <- 5*((35+18)^(2.5)-(humedades_relativas/100)*(temperaturas+18)^(2.5))*(vientos+4)*10^(-6)
  mes <- rep(i, 10000)
  simulaciones <- data.frame(mes,
                             temperaturas,
                             vientos,
                             humedades_relativas,
                             tasa_de_evaporacion_simulada)
  datos_observatorio_simulados_T35 <- rbind(datos_observatorio_simulados_T35, simulaciones)
  i = i + 1
}

# Resumen de simulaciones ----

# Ahora que ya he simulado el comportamiento para diferentes temperaturas del hormigón
# podremos recrear las probabilidades.

datos_observatorio_mensual_simulacion_T20 <- datos_observatorio_simulados_T20 %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio_T20 = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio_T20 = sd(tasa_de_evaporacion_simulada)) %>%
  mutate(probabilidad_tasa_mayor_igual_0.5_T20 = pnorm(0.5, mean = tasa_evaporacion_promedio_T20, sd = tasa_evaporacion_desvio_T20, lower.tail = FALSE)) %>%
  mutate(p80_T20 = qnorm(0.8, mean = tasa_evaporacion_promedio_T20, sd = tasa_evaporacion_desvio_T20, lower.tail = TRUE))

datos_observatorio_mensual_simulacion_T25 <- datos_observatorio_simulados_T25 %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio_T25 = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio_T25 = sd(tasa_de_evaporacion_simulada)) %>%
  mutate(probabilidad_tasa_mayor_igual_0.5_T25 = pnorm(0.5, mean = tasa_evaporacion_promedio_T25, sd = tasa_evaporacion_desvio_T25, lower.tail = FALSE)) %>%
  mutate(p80_T25 = qnorm(0.8, mean = tasa_evaporacion_promedio_T25, sd = tasa_evaporacion_desvio_T25, lower.tail = TRUE))

datos_observatorio_mensual_simulacion_T30 <- datos_observatorio_simulados_T30 %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio_T30 = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio_T30 = sd(tasa_de_evaporacion_simulada)) %>%
  mutate(probabilidad_tasa_mayor_igual_0.5_T30 = pnorm(0.5, mean = tasa_evaporacion_promedio_T30, sd = tasa_evaporacion_desvio_T30, lower.tail = FALSE)) %>%
  mutate(p80_T30 = qnorm(0.8, mean = tasa_evaporacion_promedio_T30, sd = tasa_evaporacion_desvio_T30, lower.tail = TRUE))

datos_observatorio_mensual_simulacion_T35 <- datos_observatorio_simulados_T35 %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio_T35 = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio_T35 = sd(tasa_de_evaporacion_simulada)) %>%
  mutate(probabilidad_tasa_mayor_igual_0.5_T35 = pnorm(0.5, mean = tasa_evaporacion_promedio_T35, sd = tasa_evaporacion_desvio_T35, lower.tail = FALSE)) %>%
  mutate(p80_T35 = qnorm(0.8, mean = tasa_evaporacion_promedio_T35, sd = tasa_evaporacion_desvio_T35, lower.tail = TRUE))


names(datos_observatorio_mensual_simulacion_T20)[1] <- "month"
names(datos_observatorio_mensual_simulacion_T25)[1] <- "month"
names(datos_observatorio_mensual_simulacion_T30)[1] <- "month"
names(datos_observatorio_mensual_simulacion_T35)[1] <- "month"

# Ahora pondré todos los resultados de las simulaciones en un único Dataframe

Simulaciones_finales <- merge(datos_observatorio_mensual_simulacion, datos_observatorio_mensual_simulacion_T20, by = "month")
Simulaciones_finales <- merge(Simulaciones_finales, datos_observatorio_mensual_simulacion_T25, by = "month")
Simulaciones_finales <- merge(Simulaciones_finales, datos_observatorio_mensual_simulacion_T30, by = "month")
Simulaciones_finales <- merge(Simulaciones_finales, datos_observatorio_mensual_simulacion_T35, by = "month")

# Gráficos de las simulaciones con temperaturas constantes ----

# Ahora realizaré el gráfico de probabilidades para todas las temperaturas

probabilidades_mensuales_T35 <- ggplot(Simulaciones_finales)+
  geom_col(mapping = aes(x= month ,y = probabilidad_tasa_mayor_igual_0.5_T35, fill = probabilidad_tasa_mayor_igual_0.5_T35))+
  labs(title =  "Probabilidades mensuales de Tasa de evaporacion superior a 0.5",
      subtitle = "Detalle de las probabilidades de que la tasa de evaporación para cada mes sea igual o mayor a 0,5
considerando la temperatura del hormigón es igual a 35 ºC, en función de las distribuciones obtenidas 
mediante simulación",
      x = "Mes",
      y = "Probabilidad")+
  theme_minimal() +
  scale_fill_viridis()+
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        legend.position="none",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10),
        plot.title.position = "plot")

plot(probabilidades_mensuales_T35)

tasas_de_evaporacion_ridges_T35 <- ggplot(datos_observatorio_simulados_T35) +
  geom_density_ridges_gradient(scale = 3.5,
                               rel_min_height = 0.01,
                               aes(x = tasa_de_evaporacion_simulada, y = as.factor(mes),
                                   fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "turbo") +
  labs(title = 'Distribución de tasas de evaporación mensuales en Córdoba',
       subtitle = "Las distribuciones fueron generadas considerando que la temperatura del hormigón para los cálculos era
igual a 35 ºC",
       x = "Tasa de evaporación ( Kg/m^2/hr)",
       y = "Mes") +
  theme_ridges() +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        legend.position="none",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10),
        plot.title.position = "plot")

plot(tasas_de_evaporacion_ridges_T35)


probabilidades_mensuales_T30 <- ggplot(Simulaciones_finales)+
  geom_col(mapping = aes(x= month ,y = probabilidad_tasa_mayor_igual_0.5_T30, fill = probabilidad_tasa_mayor_igual_0.5_T30))+
  labs(title =  "Probabilidades mensuales de Tasa de evaporacion superior a 0.5",
       subtitle = "Detalle de las probabilidades de que la tasa de evaporación para cada mes sea igual o mayor a 0,5
considerando la temperatura del hormigón es igual a 30 ºC, en función de las distribuciones obtenidas 
mediante simulación",
       x = "Mes",
       y = "Probabilidad")+
  theme_minimal() +
  scale_fill_viridis()+
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        legend.position="none",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10),
        plot.title.position = "plot")

plot(probabilidades_mensuales_T30)

# position=position_dodge(width=0.9), hjust=0.75

tasas_de_evaporacion_ridges_T30 <- ggplot(datos_observatorio_simulados_T30) +
  geom_density_ridges_gradient(scale = 3.5,
                               rel_min_height = 0.01,
                               aes(x = tasa_de_evaporacion_simulada, y = as.factor(mes),
                                   fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "turbo") +
  labs(title = 'Distribución de tasas de evaporación mensuales en Córdoba',
       subtitle = "Las distribuciones fueron generadas considerando que la temperatura del hormigón para los cálculos era
igual a 30 ºC",
       x = "Tasa de evaporación ( Kg/m^2/hr)",
       y = "Mes") +
  theme_ridges() +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5), 
        legend.position="none",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 10),
        plot.title.position = "plot")

plot(tasas_de_evaporacion_ridges_T30)

probabilidad_0.5_diferentes_temperaturas <- Simulaciones_finales[1:12,c(8,12,16,20)]
probabilidad_0.5_diferentes_temperaturas <- data.frame(t(probabilidad_0.5_diferentes_temperaturas))

row.names(probabilidad_0.5_diferentes_temperaturas) <- c("20", "25", "30", "35")
probabilidad_0.5_diferentes_temperaturas <- cbind(probabilidad_0.5_diferentes_temperaturas, c("20", "25", "30", "35"))

colnames(probabilidad_0.5_diferentes_temperaturas) <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                                                        "Diciembre", "temperaturas")

probabilidad_0.5_diferentes_temperaturas$temperaturas <- as.double(probabilidad_0.5_diferentes_temperaturas$temperaturas)

nueva_tabla <- probabilidad_0.5_diferentes_temperaturas %>%
  pivot_longer(!temperaturas,
               values_to = "Probabilidades",
               names_to = "Mes")

nueva_tabla <- tibble(nueva_tabla)
nueva_tabla$temperaturas <- as.double(nueva_tabla$temperaturas)
nueva_tabla$Mes <- as.factor(nueva_tabla$Mes)
nueva_tabla$Probabilidades <- as.double(nueva_tabla$Probabilidades)

tabla_para_grafico <- nueva_tabla %>%
  filter(temperaturas == 35) %>%
  arrange(desc(Probabilidades))

evolucion_tasa_evaporación_según_temp_hormigon_version_pivot <- ggplot(nueva_tabla)+
  geom_line(mapping = aes(x = temperaturas, y = Probabilidades, color = Mes), size = .75, alpha = .9)+
  theme_minimal()+
  labs(x = "Temperatura del hormigón ºC",
       y = "Probabilidad",
       title = "Tasa de Evaporación vs Temperatura del hormigón",
       subtitle = "Probabilidad de que la tasa de evaporaciòn sea igual o mayor a 0,5 en función de la temperatura de colocación del hormigón diferenciado por mes.")+
  theme(axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0.95), 
        legend.position="none",
        plot.title = element_text(hjust = 0, size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(b = 10)),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))+
  scale_color_manual(values = c("Enero" = "grey75",
                                "Febrero" = "dodgerblue4",
                                "Marzo" = "grey75",
                                "Abril" = "grey75",
                                "Mayo" = "grey75",
                                "Junio" = "grey75",
                                "Julio" = "grey75",
                                "Agosto" = "grey75",
                                "Septiembre" = "darkcyan",
                                "Octubre" = "grey75",
                                "Noviembre" = "grey75",
                                "Diciembre" = "grey75"))+
  scale_x_continuous(limits = c(20,36.5),
                     breaks = seq(20,35, by = 1),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1, by = 0.2),
                     expand = c(0,0))+
  geom_text(data = nueva_tabla %>% filter(Mes == "Febrero" & temperaturas == 35),
            aes(label = paste(Mes, "\n", round(Probabilidades,digits = 2))),
            x = 35.25,
            color = "dodgerblue4",
            y = 0.685,
            hjust = 0,
            vjust = 0.75,
            size = 3.2,
            lineheight = 0.9,
            fontface = "bold")+
  geom_text(data = nueva_tabla %>% filter(Mes == "Septiembre" & temperaturas == 35),
            aes(label = paste(Mes, "\n", round(Probabilidades, 2))),
            x = 35.25,
            color = "darkcyan",
            y = 0.925,
            hjust = 0,
            vjust = 0.75,
            size = 3.2,
            lineheight = 0.9,
            fontface = "bold")+
  geom_segment(y = 0.5, yend = 0.5,
               x = 20, xend = 35,
             linetype = "dashed")+
  geom_text(label = "Con 30 grados centígrados,\n 7 meses tienen una probabilidad cercana \n o inferior al 50 % de que la tasa de evaporación \n sobre el hormigón  sea igual o mayor que 0,5", 
            x = 25.5, 
            y = 0.84,
            size = 3.2,
            lineheight = 1.1)+
  annotate(geom = "curve", x = 25.5, y = 0.72,
           xend = 29.5, yend = 0.52,
           curvature = 0.15,
           arrow = arrow(length = unit(2, "mm")))


plot(evolucion_tasa_evaporación_según_temp_hormigon_version_pivot)

# Preparación para lolipop y gráfico cleveland dots ----

nueva_tabla_ordenada_cleveland <- probabilidad_0.5_diferentes_temperaturas %>% 
  t()

colnames(nueva_tabla_ordenada_cleveland) <- c("t20", "t25", "t30", "t35") 

nueva_tabla_ordenada_cleveland <- nueva_tabla_ordenada_cleveland[1:12,]

nueva_tabla_ordenada_cleveland <- data.frame(nueva_tabla_ordenada_cleveland)

nueva_tabla_ordenada_cleveland <- nueva_tabla_ordenada_cleveland %>%
  mutate(mes = c("Enero",
                 "Febrero",
                 "Marzo",
                 "Abril",
                 "Mayo",
                 "Junio",
                 "Julio",
                 "Agosto",
                 "Septiembre",
                 "Octubre",
                 "Noviembre",
                 "Diciembre"))

nueva_tabla_ordenada_cleveland <- nueva_tabla_ordenada_cleveland %>%
  rowwise() %>%
  mutate(mymean = mean(c(t30,t35))) %>% 
  arrange(-mymean) %>%
  mutate(mes = factor(mes, mes))

cleveland_mensual <- ggplot(nueva_tabla_ordenada_cleveland)+
  geom_segment(aes(x=mes, xend=mes, y=t30, yend=t35), color="grey")+
  geom_point( aes(x=mes, y=t30), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=mes, y=t35), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",
    axis.line = element_line(size = 0.5,
                             color = "black"),
    plot.title = element_text(hjust = 0, vjust = 0.2, size = 18),
    plot.subtitle = element_text(hjust = 0, vjust = 1.5, size = 10),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white")) +
  xlab("")+
  ylab("")+
  geom_text(label = "Hormigón a 30°C", 
            x = 6, 
            y = 0.43,
            size = 3.2,
            lineheight = 1.1,
            color= rgb(0.2,0.7,0.1,0.5))+
  geom_text(label = "Hormigón a 35°C", 
            x = 10, 
            y = 0.78,
            size = 3.2,
            lineheight = 1.1,
            color= rgb(0.7,0.2,0.1,0.5))+
  labs(title = "Tasa de Evaporación vs Temperatura del hormigón",
       subtitle = "Evolución de la probabilidad de que la tasa de evaporación del hormigón sea igual o mayor a 0,5  al pasar \n la temperatura del hormigón de 30°C a 35°C")
  
ggsave("Tasas de evaporación.png", width = 7, height = 9)
