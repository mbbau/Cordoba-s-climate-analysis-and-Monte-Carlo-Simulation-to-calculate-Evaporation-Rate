# En este link se explica como poder analizar por mes una dataset historica
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/

library(tidyverse)
library(lubridate)
library(skimr)
library(ggridges)
library(viridis)
library(ggrepel)
library(readxl)

datos_observatorio <- read_excel("Observatorio.xlsx")

# Primero, transformo el formato de los datos del data frame a los tipos que corresponden.

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

datos_observatorio_mensual_simulacion <- datos_observatorio_simulados %>%
  drop_na()%>%
  group_by(mes)%>%
  summarise(tasa_evaporacion_promedio = mean(tasa_de_evaporacion_simulada),
            tasa_evaporacion_desvio = sd(tasa_de_evaporacion_simulada))


datos_observatorio_mensual_simulacion <- datos_observatorio_mensual_simulacion %>%
  mutate(probabilidad_tasa_mayor_igual_0.5 = pnorm(0.5, mean = tasa_evaporacion_promedio, sd = tasa_evaporacion_desvio, lower.tail = FALSE)) %>%
  mutate(p80 = qnorm(0.8, mean = tasa_evaporacion_promedio, sd = tasa_evaporacion_desvio, lower.tail = TRUE))

names(datos_observatorio_mensual_simulacion)[1] <- "month"

probabilidades_mensuales_tasa_0.5 <- ggplot(datos_observatorio_mensual_simulacion)+
  geom_col(mapping = aes(x= month ,y = probabilidad_tasa_mayor_igual_0.5, fill = probabilidad_tasa_mayor_igual_0.5))+
  labs(title = "Probabilidades mensuales para tasas de Evaporación",
       subtitle = "Detalle de las probabilidades de que la tasa de evaporación para cada mes sea igual o mayor a 0,5",
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

# Ahora realizaré el gráfico de probabilidades para todas las temperaturas
#######
#######

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

#######
#######

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


