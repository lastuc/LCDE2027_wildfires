# Lancet Countdown Europe 2027: Wildfire smoke indicator

This repository contains the code to generate the wildfire smoke indicators for the Lancet Countdown Europe 2027 edition, as well as the figures and tables included in the report and appendix. Please refer to the appendix file for a complete description of the methods.

## Analysis

The workflow to compute the indicators is structured in a sequential set of steps that can be run from a root [makefile](makefile.R) script. Note that some of the steps were performed in a High Performance Computing (HPC) cluster due to memory usage. The steps to be run are:

* [1. Population data processing](R/1_population.R): Clean GEOSTAT gridded population data and create raster at exposure geometries (HPC).
* [2. NUTS boundary data processing](R/2_nuts.R): Assign NUTS and region codes to each of the exposure geometries (HPC).
* [3. Mortality data processing](R/3_mortality.R): Clean EUROSTAT mortality time series and disaggregation to daily counts.
* [4. Exposure data processing](R/4_assemble.R): Extract daily exposures, population and NUTS codes and aggregate population counts (HPC).
* [5. Exposure analysis](R/5_exposures.R): Construct wildfire-PM2.5 exposure subindicator (HPC).
* [6. Health Impact Assessment (HIA) analysis](R/6_HIA.R): Construct attributable mortality subindicator (HPC).
* [7. Fire weather index analysis](R/7_FWI.R): Construct Forest fire Weather Index (FWI), subindicator.
* [8. Tables and figures](R/8_figtab.R): Code to generate the figures and tables included in the report.

## Preparation

The indicator uses a set of open datasets (NUTS and regional boundaries, mortality time series, gridded population estimates) that can be easily obtained from Eurostat. Those should be placed in the folder `data/raw/boundaries`, `data/raw/regions`, `data/raw/population`, `data/raw/mortality`, respectively. These are not included in the repository due to large file sizes. 

Furthermore, the indicator uses wildfire-PM2.5 exposure data derived from the Integrated System for wild-land Fires (IS4FIRES) and the System for Integrated modelling of Atmospheric composition (SILAM) models, as well as Forest fire Weather Index (FWI) information computed using ERA5 data. Please contact the authors of the indicator (Lara.Stucki@isglobal.org, Mikhail.Sofiev@fmi.fi, Risto.Hanninen@fmi.fi, Cathryn.Tonne@isglobal.org) for details on how to get access to the data. SILAM data should be placed in the `data/raw/SILAM` folder and fire danger data should be placed in the `data/raw/FWI` folder.
