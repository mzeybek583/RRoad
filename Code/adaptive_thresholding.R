## Roadlane based centerline extraction

#Adaptive thresholding


# Set maximum memory allocation -------------------------------------------


memory.size(max = TRUE)
memory.limit(size=56000)

# Load libraries ----------------------------------------------------------

library(lidR)
library(RANN)
library(ggplot2)

time <- proc.time()

#data <- readLAS(files = "E:/DR_Sonrasi_Projeler/UAV_ROAD/Landslide/UAV2-18102020/R/CC_auto_Road_extracted.las")
data <- readLAS(files = "test4.las")

proc.time() - time



# New grayscale image = ( (0.3 * R) + (0.59 * G) + (0.11 * B) ).
# Agisoft formula: 0.21 R + 0.72 G + 0.07 B.
d <- data@data
#RGB value exportation
d.R <- d$R
d.G <- d$G
d.B <- d$B


# Compute composition -----------------------------------------------------

composite <- as.integer(0.3*d.R + 0.59*d.G + 0.11*d.B)
proc.time() - time

las.export <- LAS(d) # create LAS

#las.export <- add_lasattribute(las.export, composite, "int", "RGB composite")

las.export@data$Intensity <- composite 

plot(las.export, color="Intensity", legend=T)
#writeLAS(las.export, "Road_intensity.las")

#xx <- readLAS("Road_intensity.las")

# Smoth Intensity ---------------------------------------------------------
smooth_i <- function(x) { # user-defined function
   m <- median(x)
   return(m) # output
}

metrics <- point_metrics(las.export, ~smooth_i(Intensity), k = 6) # 1

las.export@data$Intensity <- metrics$V1

plot(las.export, color="Intensity", legend=T)

# Compute Neighbouring ----------------------------------------------------



df <- las.export@data

k <- 20 # Neighboor points

dist <- nn2(df[,1:3],df[,1:3], k=k+1, "kd")

proc.time() - time

df$road <- 0

# Histogram of Intensity --------------------------------------------------

#Normalize Intensity Value between 0-1

normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
}
df$Intensity <- normalize(df$Intensity)
summary(df$Intensity)
tj <- nrow(df)
# create progress bar
pb <- txtProgressBar(min = 0, max = tj, style = 3)

# Adaptive Threshold According to std around "k" points -------------------

for (j in 1:tj) {
#hist(df[dist$nn.idx[2:k]]$int)
std <- sd(df[dist$nn.idx[2:k]]$Intensity)
mm <- median(df[dist$nn.idx[2:k]]$Intensity)

sigma <- 1 # change sigma if require

cond.max <- mm + sigma*std
cond.min <- mm - sigma*std


if (df[dist$nn.idx[j]]$Intensity  > 0.6) { # Change parameter 0.6 if require
  road <- ifelse(df[dist$nn.idx[j]]$Intensity > cond.min && df[dist$nn.idx[j]]$Intensity < cond.max, 0, 1)
  } else{
  road <- 0
}

## Filter Certain Intensity Value

 # if (df[dist$nn.idx[j]]$Intensity  < 0.6) {
 #    road <- 0
 # } else{
 #    road <- 1
 # }

df[dist$nn.idx[j]]$road <- road
setTxtProgressBar(pb, j)
}
close(pb)

proc.time() - time


# Plot on R ---------------------------------------------------------------
df$road <- as.factor(df$road)
 ggplot(df, aes(X, Y,color= road))+
   geom_point()+
   scale_colour_manual(values=c("grey", "red"))+
   coord_fixed()+
   guides(col=guide_legend("Classification"))+
   theme_bw(base_size = 20)

 df$road <- as.integer(df$road)
 sprintf("Founded  %i points roadlane",  length(which(df$road==1)))# Non-roadlane
 sprintf("Founded  %i points roadlane",   length(which(df$road==2)))## roadlane
 
 
 sprintf("Process ending in %3.1f s", (proc.time() - time)[3])
 

# Export File -------------------------------------------------------------

 
write.csv(cbind(df[,1:3],intensity=df$Intensity, road=df$road),"result.csv")
