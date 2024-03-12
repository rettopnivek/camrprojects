# camrprojects

An R package with portable functions used in R projects for the Center for Addiction Medicine (CAM) at Massachusetts General Hospital (MGH).

## Getting started

### Prerequisites

First install the statistical software [R](https://www.r-project.org/). It is also recommended to install [RStudio](https://www.rstudio.com/products/rstudio/). Finally, the following R packages are required:

* [devtools](https://cran.r-project.org/web/packages/devtools/index.html)
* [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
* [furrr](https://cran.r-project.org/web/packages/furrr/index.html)
* [httr](https://cran.r-project.org/web/packages/httr/index.html)
* [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
* [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
* [progress](https://cran.r-project.org/web/packages/progress/index.html)
* [progressr](https://cran.r-project.org/web/packages/progressr/index.html)
* [purrr](https://cran.r-project.org/web/packages/purrr/index.html)
* [rlang](https://cran.r-project.org/web/packages/rlang/index.html)
* [stringr](https://cran.r-project.org/web/packages/stringr/index.html)
* [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)

### Installation

To easily install the 'camrprojects' package, you'll need the 'devtools' package:  
```R
install.packages("devtools")
library(devtools)
```

The 'camrprojects' package can then be installed via the following command:  
```R
install_github("rettopnivek/camrprojects")
```

## Using the package

To load the package:
```R
library(camrprojects)
```

To list the available functions:
```R
ls(pos = "package:camrprojects")
```

Details and examples for a specific function can be obtained via `help( "function" )` substituting the appropriate function name for "function".

### Standardized naming schemes

#### File names

To aid in file organization and interpretability, file names in CAM R projects will ideally follow the formats below:

* **TXX-Description-MM_DD_YYYY.ext** or...
* **TXX-Description-Created_MM_DD_YYYY_at_H_M.ext**.

The components of this naming scheme are:
* **T** - A letter for grouping and categorization. For example...
    * Files starting with 'R' or 'S' can refer to R scripts.
    * Files starting with 'D' can refer to data files (.RData, .csv, etc.).
    * Files starting with 'F' can refer to image files (.png, .pdf, etc.).
    * Files starting with 'W' can refer to Word documents.
    * Files starting with 'P' can refer to Powerpoint slides.
    * files starting with 'T' can refer to plain text files.
* **XX** - A numbering scheme from '01' onward.
* **Description** - A brief human-readable description of the file with words separated by underscores (e.g., 'Data_preparation' or 'Descriptive_summaries').
* **MM_DD_YYYY** or **Created_MM_DD_YYYY_at_H_M** - the date or date and time when a file is created. This is most useful for data and output files.

#### Column names in data frames

Data frames containing data from CAM studies should preferably follow a consistent variable naming scheme. Ideally, variables will be in the style:
* **GGG.TTT.Var_name** or...
* **GGG.TTT.Optional.Var_name**.

Each component has a specific interpretation/use:
* **GGG** is a 3-letter abbreviation giving the overarching group for a variable (e.g., 'Identifiers', 'Session details', etc.);
* **TTT** is a 3-letter abbreviation for the type of data (e.g., 'Integer', 'Character string', etc.);
* **Optional** refers to an optional sub-category (e.g., indicators for time points like 'BL' for baseline and 'Y1' or 'Y2' or subsequent years);
* **Var_name** is a human-readable label (e.g., 'Treatment_groups', 'Session_dates', etc.).

The goal of this naming scheme is to 1) aid in filtering and selecting columns and 2) improve readability and interpretation of columns.

#### Data Dictionary Tools

Forthcoming.

### Reading data from REDCap

Forthcoming.

## Authors

### Contributors

* Eylul Ackman
* Michael Pascale
* Kevin Potter
* William Schmitt
* Bryn Evohr

### Current maintainers

* Kevin Potter
* Bryn Evohr

## License

MIT
