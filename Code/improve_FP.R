

## Improve FP results


# Load Library ------------------------------------------------------------
library(lidR)

# Load Data ---------------------------------------------------------------
las <- readLAS(files = "data/improve_FP/road.las") 

#plot(las)

metrics <- point_metrics(las, ~list(zdev = sd(Z)), k = 7, r = 3) # knn based sd

las <- add_attribute(las, metrics$zdev, "Zdev") # add metrics into our las file

#plot(las2, color="Zdev", legend=T)

Zdev_free = filter_poi(las2, Zdev <=0.005) # filter outside of deviation of 005
Zdev_out = filter_poi(las2, Zdev >0.005)  # filtered outlier points

#plot(Zdev_free, color="Zdev", legend=T)

writeLAS(Zdev_free, "Zdev.las") # write to las file
  