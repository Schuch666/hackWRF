# example scripts for WRF evaluation using METAR

## 0. Instalation
Instalation process of [hackWRF](https://github.com/Schuch666/hackWRF#installation) package, other R-packages needed ([riem](https://docs.ropensci.org/riem/) and [openair](https://davidcarslaw.com/files/openairmanual.pdf)), can be installed inside R session:
`install.packages('riem')
install.packages('openair')`

## 1. Download and processing of the METAR data
To download the data, run the R-script `download.R`, this script download all METAR stations with the code included in the variable `sites` including some preliminary preprocessing of the data, the data/sites can also be found in [link](https://mesonet.agron.iastate.edu/request/download.phtml).

## 2. Processing of site locations
The script `process_stations.R` create a data.frame with the latitude and longitude using the metadata from all downloaded METAR sites and save in `metar-sites.Rds`.

## 3. Extracting model variables
The bash script `post.sh` (a PBS job script for Cheyenne) is a high-level script that passes the folder and the variable name (and other arguments if needed) to the R-script `extract_metar.R` that extract values from domain d01, d02 and d03 (in series) using `metar-sites.Rds` information and create a tar file with the processed data. Each variable is extracted in parallel.

## 4. Perform model evaluation
The R-script `all_tables.R` control the paths and other control variables and call `table_metar_T2.R`, `table_metar_Wind.R` and `table_metar_Q.R`.

Each script opens the extracted series from 3 domains and perform the unit conversion (T2, Q2) and calculate other variables (WS and WD).
Open all downloaded METAR data and select the time to match the simulation period and perform the unit conversion (Q2) and other variables (Q2).
Perform temporal processing (daily average for Q2).
Calculate the evaluation site by site, a summary of each evaluation and a table with a fair comparison of model performance, use `?evaluation` for the description and options available.
The output is .csv files for each variable (T2, WD, WS, Q2) and each domain (d01, d02, d03 and the comparison with different domains).
Plot time series.
