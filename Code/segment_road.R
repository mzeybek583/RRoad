
## Segmented regression
## Version Date 31.10.2020


setwd("/media/mzeybek/7C6879566879105E/IGD_Mersin")
library(lidR)

dataFiles <- lapply(Sys.glob("data/cross_section*.las"), readLAS)
outFiles <- list()
i<- 40

for (i in 1:length(dataFiles)) {
print(i)
  time <- proc.time()
df <- dataFiles[[i]]

d.df <- data.frame(x=df@data$X,y=df@data$Y,z=df@data$Z)

### PCA step -----
df.pca <- prcomp(d.df)

rotated_df = as.data.frame(df.pca$x)
rotated_df$PC3 <- rotated_df$PC3*-1

### PLOT -----------
library(ggplot2)
ggplot()+
  geom_point(data=d.df ,aes(x,y),alpha = 0.5)+
  xlab("Y")+ ylab("X")+
theme_classic(base_size = 20)


ggplot()+
  geom_point(data=rotated_df ,aes(PC1,PC2),alpha = 0.5)+
  xlab("PC1")+ ylab("PC2")+
  theme_classic(base_size = 20)
ggplot()+
  geom_point(data=rotated_df ,aes(PC1,PC3),alpha = 0.5)+
  xlab("PC1")+ ylab("PC3")+coord_fixed(ratio = 10)+
  theme_classic(base_size = 20)

str(df.pca)

library(segmented)

y <- rotated_df$PC3
x <- rotated_df$PC1
z <- rotated_df$PC2

df_xy <- data.frame(x=x, y=y)
#r <- segmented(lm(y~x), seg.Z= ~x, npsi=2, psi = list(x = c(-6, 6)))
r <- segmented(lm(y~x), npsi=12)

## Plot Segment Results ----
library(ggplot2)
dat2 = data.frame(x = x, y = broken.line(r)$fit)

ggplot(df_xy, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = dat2, color = 'red', size=2)+coord_fixed(ratio = 10)+
  xlab("PC1")+ylab("PC3")+
  theme_classic(base_size = 20)

### ?Teration step ----
ind <- r$residuals<0.01
x <- x[ind]
y <- y[ind]
z <- z[ind]
df_xy2 <- data.frame(x=x, y=y)

r2 <- segmented(lm(y~x), npsi=8)

dat3 = data.frame(x = x, y = broken.line(r2)$fit)

ggplot(df_xy2, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat3, color = 'blue', size=2)+coord_fixed(ratio = 10)+
  xlab("PC1")+ylab("PC3")+
  theme_classic(base_size = 20)

### iteration 3
ind <- r2$residuals<0.01
x <- x[ind]
y <- y[ind]
z <- z[ind]
df_xy2 <- data.frame(x=x, y=y)

r3 <- segmented(lm(y~x), npsi=6)

dat3 = data.frame(x = x, y = broken.line(r3)$fit)

ggplot(df_xy2, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat3, color = 'blue', size=2)+coord_fixed(ratio = 10)+
  xlab("PC1")+ylab("PC3")+
  theme_classic(base_size = 20)

### iteration 4
ind <- r3$residuals<0.01
x <- x[ind]
y <- y[ind]
z <- z[ind]
df_xy2 <- data.frame(x=x, y=y)

r4 <- segmented(lm(y~x), npsi=3)

dat3 = data.frame(x = x, y = broken.line(r4)$fit)

ggplot(df_xy2, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat3, color = 'blue', size=2)+coord_fixed(ratio = 10)+
  xlab("PC1")+ylab("PC3")+
  theme_classic(base_size = 20)



## multivariate adaptive regression splines (MARS) Test ##
# library(rpart)
# df <- data.frame(x=x, y=y)
# tree <- rpart(y ~ x, data=df,control=rpart.control(minsplit=7, cp=.1))
# 
# plot_tree <- function(tree, x, y) {
#   s <- seq(-20, 20, by=1)
#   plot(x, y)
#   lines(s, predict(tree, data.frame(x=s)), col="red",lwd=2)
# }
# 
# plot_tree(tree, x, y)
########## MARS Test ###########
# # Helper packages
# library(dplyr)     # for data wrangling
# library(ggplot2)   # for awesome plotting
# 
# # Modeling packages
# library(earth)     # for fitting MARS models
# library(caret)     # for automating the tuning process
# 
# # Model interpretability packages
# library(vip)       # for variable importance
# library(pdp)       # for variable relationships
# 
# 
# mars1 <- earth(y~x,data = df, degree = 2,
#                thresh=0.001, varmod.method = "rlm", nfold=3, ncross=30)
# 
# print(mars1)
# 
# ###
# plot(mars1)
# plot(mars1, which = 1)
# plot(x, y)
# o <- order(x)
# lines( x[o], mars1$fitted.values[o], col="red", lwd=3 )


psi <- r4[["psi"]][,2]
psi
psi1 <-psi[1]
psi2 <- psi[3]


### Distance threshold -------------
x.export <- x[x>=-6 & x<=6]
y.export <- y[x>=-6 & x<=6]
z.export <- z[x>=-6 & x<=6]

Road <- data.frame(x.export,z.export,y.export*-1)
Road.mat <- data.matrix(Road)
class(df.pca$x)
class(Road.mat)


orig <- t(t(Road.mat %*% t(df.pca$rotation)) + df.pca$center)
#orig <- t(t(df.pca$x %*% t(df.pca$rotation)) + df.pca$center) #Test of conversion to original data

orig <- as.data.frame(orig)
#Road <- cbind(orig,Class = cls)

colnames(orig) <- c("X", "Y", "Z")
road_las <- LAS(orig)

lidR::plot(road_las)

hist.r4 <- data.frame(res=r4$residuals)

ggplot(hist.r4, aes(res))+
  geom_histogram(bins = 200)+ xlab("Residuals (m)") + ylab("Count")+
  theme_gray(base_size = 20)

res.plot <- data.frame(fitted=r4$fitted.values, res=r4$residuals)
p <- ggplot(res.plot, aes(fitted, res))+
  geom_point()+ xlab("Fitted Values") + ylab("Residuals")+
  theme_gray(base_size = 20)

p + geom_hline(yintercept = mean(res.plot$res), col="red", lwd=2)


plot(r4)
vcov(r4)
intercept(r4)
slope(r4)
summary(r4)

outFiles[[i]] <- road_las
sprintf("Segment hesaplandi %.0f", (proc.time() - time)[3])

}

for (i in 1:length(outFiles)) {
  
  writeLAS(outFiles[[i]], paste("data/slice_out",i,".las"))
  
}

