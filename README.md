
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SpatioTemporal.cluster

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/kongdd/heatwave?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/heatwave)
[![Travis build
status](https://travis-ci.org/kongdd/heatwave.svg?branch=master)](https://travis-ci.org/kongdd/heatwave)
[![Codecov test
coverage](https://codecov.io/gh/kongdd/heatwave/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/heatwave?branch=master)
<!-- badges: end -->

The goal of SMI is to …

## Installation

### 安装Julia

  - Install Julia (\>=1.5.3)
  - Install `StatsBase`, `RCall` package
  - In Julia, `build RCall`, ensure `using RCall` works

### 安装SpatioTemporal.cluster

``` r
devtools::install_github("kongdd/SpatioTemporal.cluster")
```

## Example

推荐使用julia版本的`cluster_SpatioTemporal`，且设置`method = "tree"`，此版本下速度最快

``` r
library(SpatioTemporal.cluster)
clusterIds <- cluster_SpatioTemporal_julia(arr, method = "tree",
    ncell_connected = 1L, ncell_overlap = 5L, factor = 1e4, diag = FALSE) 
```

## covr::package_coverage()

```R
covr::package_coverage()
SpatioTemporalCluster Coverage: 12.06%
R/cluster_stats.R: 0.00%
R/HW_indices.R: 0.00%
R/plot.R: 0.00%
src/init.c: 0.00%
src/InputOutput.f90: 0.00%
src/mo_drought_evaluation.f90: 0.00%
src/mo_global_variables.f90: 0.00%
src/mo_julian.f90: 0.00%
src/mo_kind.f90: 0.00%
src/mo_percentile.f90: 0.00%
src/mo_sort.f90: 0.00%
src/mo_utils.f90: 0.00%
src/mo_weight.f90: 0.00%
src/sub_ClusterStats2.f90: 0.00%
R/connect_temporal_Rjulia.R: 40.00%
R/cluster_SpatioTemporal_Fortran.R: 40.82%
R/tools.R: 46.34%
R/cluster_SpatioTemporal_Rjulia.R: 75.56%
R/connect_temporal_Rfortran.R: 76.47%
R/clusterID_recode.R: 92.86%
R/connect_spatial.R: 95.24%
```

## References

  - Samaniego et al. (2013), “Implications of Parameter Uncertainty on
    Soil Moisture Drought Analysis in Germany”, J Hydrometeor, 2013
    vol. 14 (1) pp. 47-68.
    <http://journals.ametsoc.org/doi/abs/10.1175/JHM-D-12-075.1>

  - Samaniego et al. (2018), “Anthropogenic warming exacerbates European
    soil moisture droughts”, Nature Climate change, 2018 pp. 1-9.
    <http://dx.doi.org/10.1038/s41558-018-0138-5>
