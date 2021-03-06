# hackWRF
An R-package with functions to work with WRF, NetCDF and model evaluation.

The functions include:
- `meta()` a function to read and write metadata on NetCDF files;
- `export_serie()` and `export_mean()` that export output for a data.table (.Rds) or NetCDF (.nc) files;
- `evaluate()` a geral function to evaluate model results performing some tests and pairing the data;
- `stats()` a custon wraper to the `openair::modStats()` with some customisation;
- `ccbind()` and `crbind()` conditional `cbind()` and conditional `rbind()`;
- `last()` but not least, a fnunctions to return the last member of a vector.

### System dependencies

### To Ubuntu
The following steps are required for installation on Ubuntu:
```bash
  sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes
  sudo apt-get --yes --force-yes update -qq
  # netcdf dependencies:
  sudo apt-get install --yes libnetcdf-dev netcdf-bin
  # units/udunits2 dependency:
  sudo apt-get install --yes libudunits2-dev
  # sf dependencies (without libudunits2-dev):
  sudo apt-get install --yes libgdal-dev libgeos-dev libproj-dev
```

### To Fedora
The following steps are required for installation on Fedora:
```bash
  sudo dnf update
  # netcdf dependencies:
  sudo yum install netcdf-devel
  # units/udunits2 dependency:
  sudo yum install udunits2-devel
  # sf dependencies (without libudunits2-dev):
  sudo yum install gdal-devel proj-devel proj-epsg proj-nad geos-devel
```

### To Windows
No additional steps for windows installation.

Detailed instructions can be found at [netcdf](https://www.unidata.ucar.edu/software/netcdf/), [libudunits2-dev](https://r-quantities.github.io/units/) and [sf](https://r-spatial.github.io/sf/#installing) developers page.

### Package installation

with devtools:

```r
# install.packages("devtools")
devtools::install_github("schuch666/hackWRF")
```

or remotes:

```r
# install.packages("remotes")
remotes::install_github("schuch666/hackWRF")
```

## Licence

![Creative Commons Attribution 4.0 International License](https://github.com/creativecommons/cc-cert-core/blob/master/images/cc-by-88x31.png "CC BY")
Licensed under a [Creative Commons Attribution 4.0 International License (CC BY)](https://creativecommons.org/licenses/by/4.0/).
