# Extreme Precipitation

## Citing this software
[Citation information](https://github.com/Falke96/extreme_precipitation_austria/blob/main/CITATION.cff)

[![DOI](https://zenodo.org/badge/611209754.svg)](https://zenodo.org/badge/latestdoi/611209754)


## Setup
### General
Some R-packages (`terra`, `sf`) have a couple dependencies. Install:
```bash
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsqlite0-dev
```

### renv
We use R `v4.1.2` for this project. Use renv to install all required R modules from `renv.lock`: 
```r
renv::restore()
```

After manually adding a package (by updating `DESCRIPTION`), update the `renv.lock` using this command:
```r
renv::init() 
```
and choosing
```
2: Discard the lockfile and re-initialize the project.
```

## Usage
The provided code is quite memory-consuming and was executed on a workstation with 32GB RAM.

### Data
The data is provided by the Austrian Zentralanstalt für Meteorologie und Geodynamik.

### Method
The methods used are summarized in the submodule `mevr`. This file contains all functions related to the MEV, SMEV and TMEV.
Note: It is planned to release the submodule as a separate R-package.

### Runnable files (Spatiotemporal model)
All the files concering the spatio-temporal model are contained in the folder `spatio_temporal_model`.
The order of the following list defines the order in which the scripts should be run.
* `spattempmodel_etl.R`: Transforms the raw data into the format that is required for training, testing and analysing with the following files.
* `spattempmodel_train.R`: Trains a spatio-temporal model using the bamlss framework.
* `spattempmodel_predict.R`: Computes a prediction of the underlying Weibull parameters over the whole domain of Austria.
* `spattempmodel_returnlevels.R`: Evaluates the return levels for daily precipitation sums based on the TMEV approach. Either the return levels are based on the whole year or separatly computed for each month.
* `plot_spattempmodel.R`: Plots the return levels. In case of monthly return levels a figure is created for each month and in addition a map of the month with the highest return is plotted.

### Runnable files (Individual stations)

* `10_years_window.R`: Median 10-, 50- and 100-year daily rainfall return levels of Austrian stations with more than 50 years series length are computed using the TMEV.
* `Poinestimates_error.R`: Compares the performance of the TMEV with the SMEV as pointestimates.
* `monthly_returns.R`: For each available Austrian weather station the month with the highest 50-year daily return level is computed.
* `motivation.R`: Illustrates the advantage of the TMEV comparing two stations with different seasonal behaviour.
* `parameterevolution.R`: Shows the annual trend of the underlying Weibull parameters from 1900 to 2016.
* `data_mayrhofen_plots.R`: Creates the data for station mayrhofen, that is used for plotting.
* `plotting_mayrhofen.R`: Creates several plots for the station of Mayrhofen illustrating the change in seasonality.

### Helper files

* `ccc.R`: Defining some global constants.
* `plotting_insert.R`: Plotting an insert with a stations location.
