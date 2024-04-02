# REMIND integration of IIASA's `climate-assessment` package

R package **remindClimateAssessment**, version **0.0.2**

[![CRAN status](https://www.r-pkg.org/badges/version/remindClimateAssessment)](https://cran.r-project.org/package=remindClimateAssessment)  [![R build status](https://github.com/pik-piam/remindClimateAssessment/workflows/check/badge.svg)](https://github.com/pik-piam/remindClimateAssessment/actions) [![codecov](https://codecov.io/gh/pik-piam/remindClimateAssessment/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/remindClimateAssessment) 

## Purpose and Functionality

The REMIND integration of IIASA's `climate-assessment` provides a standardized interface to simple climate models such as MAGICC7.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remindClimateAssessment")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Tonn Rüter <tonn.rueter@pik-potsdam.de>.

## Citation

To cite package **remindClimateAssessment** in publications use:

Rüter T (2024). _remindClimateAssessment: REMIND integration of IIASA's `climate-assessment` package_. R package version 0.0.2, <https://github.com/pik-piam/remindClimateAssessment>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remindClimateAssessment: REMIND integration of IIASA's `climate-assessment` package},
  author = {Tonn Rüter},
  year = {2024},
  note = {R package version 0.0.2},
  url = {https://github.com/pik-piam/remindClimateAssessment},
}
```
