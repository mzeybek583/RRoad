

## Improve FP results


# Load Library ------------------------------------------------------------
library(lidR)

# Load Data ---------------------------------------------------------------
las <- readLAS(files = "data/improve_FP/road.las")

plot(las)

metrics <- point_metrics(las, ~list(zdev = sd(Z)), k = 7, r = 3) # 3

las <- add_attribute(las, metrics$zdev, "Zdev")

plot(las2, color="Zdev", legend=T)

writeLAS(las2, "Zdev.las")
  