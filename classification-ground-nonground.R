
## Classification



library(lidR)

raw <- readLAS(files = "50m_raw.las")

# (Parameters chosen mainly for speed)
mycsf <- csf(TRUE, 1, 1, time_step = 1)
las <- classify_ground(raw, mycsf)
#plot(las, color = "Classification")

dtm <- grid_terrain(las, 1, tin(), use_class = 2L)

plot(dtm)
las <- normalize_height(las, dtm)

writeLAS(las = las,"50m_classified.las")

