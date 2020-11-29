
## Cross section geometry
## PSI point (Center point based)
##


# Load Library ------------------------------------------------------------

library(lidR)
library(ggplot2)
library(segmented)

# Read Data ---------------------------------------------------------------

las <- readLAS("data/ref.las")

data <- las@data

df.road <- data[data$Classification==11,]

# PCA ---------------------------------------------------------------------

rotated_df <- prcomp(x = df.road[,1:3])

road_sec.rotated <- data.frame(rotated_df$x[,1:3]) # convert data to dataframe


# Segments ----------------------------------------------------------------

y <- road_sec.rotated$PC3
x <- road_sec.rotated$PC1
z <- road_sec.rotated$PC2

df_xy <- data.frame(x=x, y=y)

#Cross section Center point
psi=c(0)
#r <- segmented(lm(y~x),  fixed.psi =  list(x = c(0)))
r <- segmented(lm(y~x),  seg.Z=~x, psi = list(x=c(0,-3,3)), 
               control=seg.control(display=TRUE), npsi=3)

# Plot --------------------------------------------------------------------

dat2 = data.frame(x = x, y = broken.line(r)$fit)

ggplot(df_xy, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = dat2, color = 'red', size=2)+coord_fixed(ratio = 10)+
  xlab("PC1")+ylab("PC3")+
  theme_classic(base_size = 20)
  