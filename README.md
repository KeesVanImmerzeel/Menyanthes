
# Package menyanthes

<!-- badges: start -->
<!-- badges: end -->

The goal of the package menyanthes is to read / write / manipulate [HydroMonitor](https://www.kwrwater.nl/tools-producten/hydromonitor/) Observationwell data. 

## Installation

You can install the released version of menyanthes from with:


`install_github("KeesVanImmerzeel/menyanthes")`

Then load the package with:

`library("menyanthes")` 

## Functions in this package
- `hm_read_export_csv()`: Read export HydroMonitor file with ObservationWell data.
- `hm_read_export_csv2()`: Read export HydroMonitor file with ObservationWell data with missing header.
- `hm_filter_on_year()`: Filter HydroMonitor ObservationWell data on year.
- `hm_calc_gxg()`: Calculate GxG's of HydroMonitor ObservationWell data.

## Get help

To get help on the functions in this package type a question mark before the function name, like `?hm_read_export_csv()`



