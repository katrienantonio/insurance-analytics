## Insurance analytics - actuarial science meets data science

You find here a selection of material for a course in insurance analytics, including lecture sheets and computer labs in R.

### Software Requirements

Please bring a laptop with a recent version of R and RStudio installed.

- R (at least 3.5.2 <https://cloud.r-project.org/bin/windows/base/> )
- RStudio ( <https://www.rstudio.com/products/rstudio/download/#download> )

Run the following script in your R session to install the required packages

```r
packages <- c("readxl", "data.table", "ggplot2", "dplyr", "ggmap", "grid", "gridExtra", 
              "rpart", "gbm", "smurf", "partykit", "pdp", "rgdal", "devtools", "plyr", "glmnet")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
```

The session on neural networks will use the keras package in R. You can follow this set of instructions to get this working:

* download and install Anaconda from https://www.anaconda.com/distribution/; make sure you select the right operating system
* open an Anaconda prompt and install keras/tensorflow with the instruction
``pip install keras``
* install ``keras`` for R using 

```{r get_keras_github eval = FALSE}
install.packages("devtools")
library(devtools)
devtools::install_github("rstudio/keras")
```
* load the ``keras`` package in your R session.

### Course material

Will be added gradually - stay tuned!