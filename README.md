README
================

# ETpML

This repository accompanies the article *(under review)* that presents
**ETpML**, a programming framework developed to automate the prediction
of **potential evapotranspiration (ETp)** using a reduced set of climate
predictors. The framework addresses the limitations of complex weather
data availability and enhances watershed-scale hydrological service
assessments.

**ETpML** provides a modular and reproducible workflow that includes:

- Building weather dataset and calculating ETp  
- Comparing different machine learning methods:
  - Random Forest (RF)  
  - Stepwise Forward Selection  
  - Multi-Layer Perceptron (MLP)  
- Applying the best-performing model to forecast ETp for unseen periods

## Repository Structure

📁 R/ → Contains R scripts for data pre-processing, preparation and
other functionalities:

- `etp_baseline.R` calculate ETp
- `climatology.R` calculates the climatogical average
- `nasa_power_download.R` download the data from NASA-POWER
- `water_balance.R` calculate simplified water balance (P - ETp)
- `climate_stationarity.R` test the stationarity of watershed climate
- `model_r2.R` generate a plot of the R squared

📁 data/ → Contains processed datasets and a Python script for modeling
ETp in each evaluated watershed

📁 data-raw/ → Contains raw input data organized by geographic
coordinates
