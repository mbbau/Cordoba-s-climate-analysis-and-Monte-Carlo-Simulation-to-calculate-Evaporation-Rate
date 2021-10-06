# En este link se explica como poder analizar por mes una dataset historica
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/

library(tidyverse)
library(lubridate)
library(skimr)
library(ggridges)
library(viridis)

datos_observatorio <- read_excel("Observatorio.xlsx")

# Primero, transformo el formato de los datos del data frame a los tipos que corresponden.

names(datos_observatorio)[10] <- "vto_med_veloc" 
head(datos_observatorio)

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


tmed_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = tmed, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "magma") +
  labs(title = 'Distribución de Temperaturas medias mensuales en Córdoba',
       subtitle = "Distribución de registro de temperaturas medias de Córdoba desde 1961 a 2021",
       x = "Temperatura media diaria en C",
       y = "Mes") +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
  )

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
  )


vto_max_veloc_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = vto_max_veloc, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Hum. Relat. [%]", option = "viridis") +
  labs(title = 'Distribución de Velocidades máximas de viento mensuales en Córdoba',
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
  )

plot(tmed_ridges)
plot(hum_relat_ridges)
plot(vto_max_veloc_ridges)

tmax_ridges <- ggplot(datos_observatorio) +
  geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.01, aes(x = tmax, y = as.factor(month), fill = ..x..)) +
  scale_fill_viridis(name = "Temp. [C]", option = "magma") +
  labs(title = 'Distribución de Temperaturas máximas mensuales en Córdoba',
       subtitle = "Distribución de registro de temperaturas máximas de Córdoba desde 1961 a 2021",
       x = "Temperatura media diaria en C",
       y = "Mes") +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
  )

plot(tmax_ridges)

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

head(datos_observatorio_mensual)

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
       subtitle = "Distribución de simulación de tasas de evaporación para Córdoba
       a partir de datos de clima de 1961 a 2021.",
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
  )

plot(tasas_de_evaporacion_ridges)
