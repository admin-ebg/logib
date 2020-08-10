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

# ------------------------------------------------------------------------------
# Variant 1: Using an official datalist

# Indicate path to the pre-filled datalist (see 'Further Resources' below)
path_to_my_datalist <- "path/to/my/datalist.xlsx"
# Read the data from an official datalist to R
my_data <- read_data(data_path = path_to_my_datalist)

# ------------------------------------------------------------------------------
# Variant 2: Using a pre-loaded dataframe called 'my_dataframe'

# Read the data from the pre-loaded dataframe
my_data <- read_data(custom_data = my_dataframe)

# ==============================================================================
# Run the analysis and store the results
results <- analysis(data = my_data, reference_month = 8, reference_year = 2020)
# Display the results of the analysis
summary(results)
```

### Further Resources
+ Official datalists: 
	- [English](https://www.logib.admin.ch/assets/Data/Datalist_e.xlsx)
	- [German](https://www.logib.admin.ch/assets/Data/Datalist_d.xlsx)
	- [French](https://www.logib.admin.ch/assets/Data/Datalist_f.xlsx)
	- [Italian](https://www.logib.admin.ch/assets/Data/Datalist_i.xlsx)
+ Methodology: TODO
+ [Federal Office for Gender Equality's information page on Logib](www.logib.ch) 
+ [Web implementation of the Logib salary equality model](https://www.logib.admin.ch)