
## Load Library
library(MASS)
library(lidR)

road_surf1 <- readLAS(files = "data/data.las")
plot(road_surf1)
df.road <- road_surf1@data ## convert las to data

# PCA ---------------------------------------------------------------------

rotated_df <- prcomp(x = df.road[,1:3])

road_sec.rotated <- data.frame(rotated_df$x[,1:3]) # convert data to dataframe

## Added Weight around ZERO##
slp_ind <- road_sec.rotated$PC1<= 0.1 & road_sec.rotated$PC1>=-0.1
w1 <- 1
w2 <-0.5
w <- rep(0.5,length(slp_ind))
w[slp_ind] <- 1
range(w)
# Robust Fit Model --------------------------------------------------------

rlm_fit <- rlm(PC3 ~ PC1, road_sec.rotated, weights = w, psi = psi.bisquare)  # robust regession model

summary(rlm_fit)

slope <- rlm_fit$coefficients[2]
intercept <- rlm_fit$coefficients[1]
geom.params <- data.frame(Slope=slope, Intercept=intercept)

# Plot --------------------------------------------------------------------

library(ggplot2)
ggplot(road_sec.rotated, aes(x = PC1, y = PC3)) +
  geom_point(color = "grey") +
  ggplot2::annotate("text", 
                    cumsum((range(road_sec.rotated$PC1)))[2]/2,  cumsum((range(road_sec.rotated$PC3)))[2]/2 + quantile(road_sec.rotated$PC3)[5], 
                    label=sprintf("Slope %% %3.1f",slope),
                    color = "red", hjust = 1, size=5, angle = 5) + coord_fixed(ratio = 100)+
  theme_bw(base_size = 20)+
  geom_abline(slope = slope, intercept = intercept, col="red", size=1.3)+
  xlab("X") + ylab("Y")
abs(range(road_sec.rotated$PC1))
cumsum(abs(range(road_sec.rotated$PC1)))[2]/2
