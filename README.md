# Córdoba's climate analysis and Monte Carlo Simulation to calculate Evaporation Rate

(This is still a work in progress)

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


## Evaporation rate

In order to obtain the evaporation rate I made use of the formulaes presented by **Paul John Uno** in his work [Plastic Shrinkage Cracking and Evaporation Formulas](https://www.researchgate.net/publication/260209439_Plastic_Shrinkage_Cracking_and_Evaporation_Formulas) so that it makes the calculation easier to compute, insted of the more well known ACI Nomograph. In his work, **Uno** makes a great reference material to learn about plastic cracks and how the envirommentals factors play their role, so if you want to learn more about this, I encourage you to visit the previous link.
For this work, I made use of the next equation to calculate the evaporation rate from the climate's data:

<img src="https://latex.codecogs.com/svg.image?E&space;=&space;5&space;*&space;(&space;[T_{c}&space;&plus;&space;18]^{2,5}&space;-&space;r*[T_{a}&space;&plus;&space;18]^{2,5})*(v&space;&plus;&space;4)&space;*&space;10^{-6}" title="E = 5 * ( [T_{c} + 18]^{2,5} - r*[T_{a} + 18]^{2,5})*(v + 4) * 10^{-6}" />

(Thanks to [CodeCogs | Equation Editor](https://editor.codecogs.com/) for rendering the equation)

Where E is the evaporation rate, Tc is the temperatura of concrete, Ta is the air temperature, r is the relative humidity and v is the speed of the wind.

## Monte Carlo Simulation

Having the weather parameters and the equation that controls the evaporation rate, I faced the need to set the last variable: the concrete's temperature (Tc).
For this I defined two posible situations. First, set the concrete's temperature to be equal to the air temperature or set it to be a constant value. Given the fact that the computation to perform was simple and did not consume a great amount of resources, I decided to go througth both paths.

1 - Making the simulation with the concrete temperature equal to the air temperature;
2 - Setting the concrete's temperature to a constant value (in this case I used 4 values: 20ºC , 25ºC, 30ºC and 35ºc).

The simulation consisted in picking a random sample from the air temperature, the wind's speed and the relative humidity, and calculate the evaporation rate for the both conditions stated above. I repeated this procedure ten thousand times for every month, obtaining this way a monthly distribution for the evaporation rate.

### Case 1: Concrete temperature equal to air temperature

The distribution obtained for this case can be seeing in the figure belove. As the graph shows, in this case the more adverce conditions occur in the last two months of the year, where the means are below 0,5 (condition to consider to be favorable for cracks to develop).

![Distribucion tasas de evaporacion para temp hormigon igual temp ambiente](https://user-images.githubusercontent.com/61053776/155150150-cf567672-860f-47f5-a907-49796f0232df.png)



