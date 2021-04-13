# camrprojects

An R package with portable functions used in R projects for the Center for Addiction Medicine (CAM) at Massachusetts General Hospital (MGH).

## Getting started

### Prerequisites

The program R ( version >= 3.0 )

The R package 'stringr'

The R package 'jsonlite'

The R package 'devtools'

The R package 'dplyr'

The R package 'tidyr'

The R package 'magrittr'

The R package 'httr'

The R package 'purrr'

The R package 'furrr'

The R package 'progressr'

The R package 'progress'

### Installation

To easily install the 'camrprojects' package, you'll need the 'devtools' package:  
```
install.packages("devtools")
library(devtools)
```

The 'utilityf' package can then be installed via the following command:  
```
install_github("rettopnivek/camrprojects")
```

## Using the package

To load the package:
```
library(camrprojects)
```

To list the available functions:
```
ls(pos = "package:camrprojects")
```

Details and examples for a specific function can be obtained via `help( "function" )` substituting the appropriate function name for "function".

## Authors

Kevin Potter

## License

MIT
