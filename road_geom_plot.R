
## Load Library
library(MASS)
library(lidR)

road_surf1 <- readLAS(files = "data.las")

df.road <- road_surf1@data ## convert las to data

# PCA ---------------------------------------------------------------------

rotated_df <- prcomp(x = df.road[,1:3])

road_sec.rotated <- data.frame(rotated_df$x[,1:3]) # convert data to dataframe


# Robust Fit Model --------------------------------------------------------

rlm_fit <- rlm(PC3 ~ PC1, road_sec.rotated, psi = psi.bisquare)  # robust regession model

summary(rlm_fit)

slope <- rlm_fit$coefficients[2]
intercept <- rlm_fit$coefficients[1]
geom.params <- data.frame(Slope=slope, Intercept=intercept)

# Plot --------------------------------------------------------------------

library(ggplot2)
ggplot(road_sec.rotated, aes(x = PC1, y = PC3)) +
  geom_point(color = "grey") +
  ggplot2::annotate("text", 
                    mean(road_sec.rotated$PC1),  median(road_sec.rotated$PC2)+3*median(road_sec.rotated$PC2), 
                    label=sprintf("Slope %% %3.1f",slope),
                    color = "red", hjust = 1, size=5, angle = 30) +
  theme_bw(base_size = 20)+
  geom_abline(slope = slope, intercept = intercept, col="red", size=1.3)+
  xlab("X") + ylab("Y")
