# Logib
## Introduction
The package `logib` is an `R` implementation of the Swiss Confederation's salary analysis tool for assessing equal pay practices (https://www.logib.admin.ch)

## Installation
### GitHub
In order to install the package from GitHub, use the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("admin-ebg/logib")
```

### CRAN
In order to install the latest release from CRAN, use:
```R
install.packages("logib")
```

### Examples

```R
library(logib)

# ------------------------------------------------------------------------------
# Variant 1: Using an official datalist or exportfile

# Indicate path to the pre-filled datalist or exportfile 
# (see 'Further Resources' below)
path_to_my_data <- "path/to/my/datalist_or_exportfile.xlsx"
# Read the data from an official datalist to R
my_data <- read_data(data_path = path_to_my_data)

# ------------------------------------------------------------------------------
# Variant 2: Using a pre-loaded dataframe called 'my_dataframe'

# Read the data from the pre-loaded dataframe
my_data <- read_data(custom_data = my_dataframe)

# ------------------------------------------------------------------------------
# Variant 3: Using the included example datalist called 'datalist_example'

datalist_example

# ==============================================================================
# Run the analysis and store the results
results <- analysis(data = datalist_example, 
   reference_month = 1, reference_year = 2019, usual_weekly_hours = 40, 
   female_spec = "F", male_spec = "M", age_spec = "age")
# Display the results of the analysis
summary(results)
```

### Further Resources
+ [Federal Office for Gender Equality's information page on Logib](https://www.ebg.admin.ch/en/equal-pay-analysis-with-logib) 
+ [Web implementation of the Logib salary equality model](https://www.logib.admin.ch)
+ [Methodology](https://www.ebg.admin.ch/en/equal-pay-analysis-with-logib)
+ Official datalists: 
	- [English](https://www.logib.admin.ch/assets/Data/Datalist_e.xlsx)
	- [German](https://www.logib.admin.ch/assets/Data/Datalist_d.xlsx)
	- [French](https://www.logib.admin.ch/assets/Data/Datalist_f.xlsx)
	- [Italian](https://www.logib.admin.ch/assets/Data/Datalist_i.xlsx)
