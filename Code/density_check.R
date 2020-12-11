
## Lascheck


# Load Library ------------------------------------------------------------
library(lidR)

# Read Data ---------------------------------------------------------------

las1 <- readLAS("data/mod1.las")
lidR::density(las1)
rm(las1)

las2 <- readLAS("data/mod2.las")
lidR::density(las2)
rm(las2)

las3 <- readLAS("data/mod3.las")
lidR::density(las3)
rm(las3)

