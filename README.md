# Logib
## Introduction
The package `logib` is an `R` implementation of the Swiss Confederation's salary analysis tool for assessing equal pay practices (https://www.logib.admin.ch)

## Installation
### GitHub
In order to install the package from GitHub, use the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("JLDC/logib")
```

### CRAN
Not yet released...

### Examples

```R
library(logib)

path_to_my_datalist <- "path/to/my/datalist.xlsx"
```

### Further Resources
+ Official datalists: 
	- [English](https://www.logib.admin.ch/assets/Data/Datalist_e.xlsx)
	- [German](https://www.logib.admin.ch/assets/Data/Datalist_d.xlsx)
	- [French](https://www.logib.admin.ch/assets/Data/Datalist_f.xlsx)
	- [Italian](https://www.logib.admin.ch/assets/Data/Datalist_i.xlsx)
+ Methodology: TODO
+ [Federal Office for Gender Equality's information page on Logib](https://www.logib.ch) 
+ [Web implementation of the Logib salary equality model](https://www.logib.admin.ch)