# Plastic Shrinkage Cracking Risk in Córdoba: Climate Analysis and Monte Carlo Simulation of Concrete Evaporation Rate

Using 60 years of weather data from Argentina's National Meteorological Service (SMN) and a Monte Carlo simulation, this project estimates, **for each month of the year, the probability that the evaporation rate of fresh concrete in Córdoba exceeds 0.5 kg/m²/h**, the threshold above which plastic shrinkage cracking becomes likely and preventive measures are recommended.

## Key findings

**Case 1: concrete at air temperature.** The riskiest period is October to January: in November and December, close to **half of the days** exceed 0.5 kg/m²/h. From May to July the risk is practically zero.

| Month | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| P(E ≥ 0.5) | 0.39 | 0.20 | 0.07 | 0.02 | ~0 | 0 | ~0 | 0.12 | 0.28 | 0.36 | 0.46 | 0.49 |

**Case 2: concrete warmer than the air.** When the concrete is placed at 30 °C, 7 months have a probability close to or below 50%. At 35 °C, every month is above 65%, peaking in August and September (~0.92), when humidity is lowest.

| Month | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| P(E ≥ 0.5), Tc = 30 °C | 0.29 | 0.24 | 0.27 | 0.39 | 0.53 | 0.64 | 0.71 | 0.75 | 0.74 | 0.63 | 0.54 | 0.40 |
| P(E ≥ 0.5), Tc = 35 °C | 0.71 | 0.67 | 0.69 | 0.77 | 0.83 | 0.86 | 0.88 | 0.91 | 0.92 | 0.86 | 0.84 | 0.77 |

The concrete temperature matters as much as the weather: with warm concrete, even winter months carry a high risk because the air is dry.

## Why it matters

Plastic shrinkage cracks appear in the first hours after placing concrete, when water evaporates from the surface faster than bleed water rises. They are especially common in slabs, floors and pavements exposed to sun and wind, and they are often blamed on the concrete itself when the real cause is the weather and the curing practices. Knowing when the risk is high allows builders to choose better pouring windows and to apply wind breaks, fogging, evaporation retarders or early curing.

## Data

The [SMN](https://www.smn.gob.ar/), under its open data policy, provided daily climate records for Córdoba (Observatorio and Aeropuerto stations, 1961–2021). The files are in the `Data` folder. Data for Mendoza, Posadas and Trelew is also included.

For each month I analyzed the distributions of the variables that drive evaporation.

### Temperature

The highest mean maximum temperatures occur in December and January, around 32 °C. The study focuses on hot weather, when the risk of plastic cracking is highest.

![Monthly maximum temperature distribution](https://user-images.githubusercontent.com/61053776/154969792-a4e70fa0-8195-4128-a7e5-42413a2c860b.png)

### Relative humidity

The lowest mean relative humidity in Córdoba occurs around September, the most adverse condition for plastic cracking.

![Monthly relative humidity distribution](https://user-images.githubusercontent.com/61053776/154969837-c7070654-0609-497a-904b-412141ca5b29.png)

### Wind speed

Wind speed distributions are more stable across the year, but their higher dispersion makes the last quarter the most adverse period.

![Monthly maximum wind speed distribution](https://user-images.githubusercontent.com/61053776/154969866-13de96a0-3b49-4ffa-8f97-3c90b23364dd.png)

## Evaporation rate

Instead of the ACI nomograph, I used the equation proposed by **Paul J. Uno** in [Plastic Shrinkage Cracking and Evaporation Formulas](https://www.researchgate.net/publication/260209439_Plastic_Shrinkage_Cracking_and_Evaporation_Formulas), which reproduces the nomograph and is easy to compute. Uno's paper is also an excellent introduction to plastic cracking and the role of environmental factors.

$$
E = 5 \left[ (T_c + 18)^{2.5} - r \,(T_a + 18)^{2.5} \right] (V + 4) \times 10^{-6}
$$

Where:

- **E**: evaporation rate (kg/m²/h)
- **T<sub>c</sub>**: concrete temperature (°C)
- **T<sub>a</sub>**: air temperature (°C)
- **r**: relative humidity (fraction, 0–1)
- **V**: wind speed (km/h)

## Monte Carlo simulation

For each month, I fitted the distributions of air temperature, relative humidity and wind speed, and drew **10,000 random samples** of each variable to compute 10,000 evaporation rates. From the resulting monthly distribution I estimated the probability of exceeding 0.5 kg/m²/h.

The last variable, the concrete temperature, was handled in two ways:

1. **Case 1:** concrete temperature equal to air temperature.
2. **Case 2:** concrete temperature fixed at 20, 25, 30 and 35 °C.

### Case 1: concrete temperature equal to air temperature

November and December are the most adverse months, with the largest share of simulated days above the threshold.

![Evaporation rate distribution, concrete at air temperature](https://user-images.githubusercontent.com/61053776/155150150-cf567672-860f-47f5-a907-49796f0232df.png)

### Case 2: fixed concrete temperature

The full distributions are in the `Charts` folder. The figure below summarizes how the probability of exceeding 0.5 kg/m²/h changes with the concrete temperature for every month.

![Probability of exceeding 0.5 kg/m²/h by concrete temperature](Charts/Evolución%20de%20las%20tasas%20de%20evaporación%20según%20la%20temperatura%20del%20hormigón.png)

The next figure zooms in on the change between 30 °C and 35 °C.

![Evaporation rate, concrete at 30 °C vs 35 °C](Charts/Tasas%20de%20evaporación.png)

## Repository structure

| Path | Content |
|---|---|
| `Data/` | SMN daily climate records (Córdoba Observatorio and Aeropuerto, Mendoza, Posadas, Trelew) |
| `Tasas de evaporación Córdoba.R` | Main analysis and Monte Carlo simulation for Córdoba |
| `Tasas de evaporación Mendoza.R`, `... Posadas.R`, `... Trelew.R` | Same analysis for other cities |
| `Charts/` | All generated figures |
| `*.Rmd` | Report drafts |

## How to reproduce

1. Install R and the packages: `tidyverse`, `lubridate`, `readxl`, `ggridges`, `viridis`, `ggrepel`, `ggtext`, `skimr`.
2. Open `Tasas de evaporacion mensuales.Rproj`.
3. Run `Tasas de evaporación Córdoba.R`. Charts are saved to the working directory.

## Limitations

- Temperature, humidity and wind are sampled **independently** from normal distributions. In reality they are correlated (hot days tend to be drier), and normal sampling can produce out-of-range values.
- The analysis uses **daily maximum temperature** and **daily maximum wind speed**, a conservative choice.
- Wind is measured at station height (about 10 m), while the ACI method refers to wind about 0.5 m above the concrete surface. This also makes the results conservative.
- The probability that E exceeds 0.5 kg/m²/h does not mean the concrete will crack: mix design, curing and site practices also play a major role.

## Next steps

- Resample real historical days (bootstrap) to keep the correlation between variables.
- Correct wind speed to the height used by the ACI method.
- Port the analysis to Python and extend it to forecast data, to estimate the hour-by-hour risk for the next days.

## Author

**Matías Baudino**: civil engineer and data professional, former Head of Laboratory and Quality Control at a ready-mix concrete producer in Córdoba.

[LinkedIn](https://www.linkedin.com/in/TU-USUARIO) · [GitHub](https://github.com/mbbau)
