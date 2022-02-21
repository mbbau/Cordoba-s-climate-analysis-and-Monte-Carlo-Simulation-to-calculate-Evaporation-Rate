# Córdoba's climate analysis and Monte Carlo Simulation to calculate Evaporation Rate

## Introduction

In this repository, I analyze climate data from Córdoba's City given by the National Meteorologic services of Argentina, in order to obtain the distribution of the evaporation rate for concrete from the climate date by doing a Monte Carlo Simulation.

With this distribution I was able to associate plastic crack risk with the month of the year that the concrete is being pour.

## Climate's data

The [National Meteorologic Services of Argentina](https://www.smn.gob.ar/) in their open data policy, shared with me Córdoba's climate data from the last 60 years. You will be able to find the files with the data in this reppository. With this data, I made ridge plots of the probability distribution for each month for the variables that interest us in order to obtain the evaporation rate, as can be seing in the next figures.

### Temperature

The maximun mean temperature occurs ussually between December and January and is aproximatedly around 32º C. The minimum mean occurs around July but for this study we are interested only in hot weather, because it is in this climate that the risk for plastic cracks is higher.

![Distribucion de temperaturas maximas mensuales](https://user-images.githubusercontent.com/61053776/154969792-a4e70fa0-8195-4128-a7e5-42413a2c860b.png)

### Humidity

In Córdoba City, the minimun mean relative humidity occurs around September, being this situation the most adverse for plastic cracks.

![Distribucion de humedades relativas mensuales](https://user-images.githubusercontent.com/61053776/154969837-c7070654-0609-497a-904b-412141ca5b29.png)

### Wind Speed

The wind's speed distribution are more stable than the other variables previously presented, but it can be seeing that the standart deviation is the main factor that makes the last cuarter of the year the most adverce in the case of the speed of wind.

![Distribuciones de vientos maximos mensuales](https://user-images.githubusercontent.com/61053776/154969866-13de96a0-3b49-4ffa-8f97-3c90b23364dd.png)




