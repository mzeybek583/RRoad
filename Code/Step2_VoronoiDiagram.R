### This is an R code to extract road surface automatically from MLS based point cloud

### clear lines in Console and all variable in R environment
rm(list = ls())
cat("\f")

### Inputs  
concavity <- 2
length_threshold <- 4.5
diff<-0.5 #This is the buffer zone length
dg <- 10  #curve degree fit


### Set path directions to load input and save output
### For MAC
Path1 <- paste0("/Users/serkanbicici/Desktop/Centerline Extraction/Files in LAS")
Path2 <- paste0("/Users/serkanbicici/Desktop/Centerline Extraction/Voronoi_Diagram_Results/Degree_",dg)

### For WINDOWS
#Path1 <- paste0("C:/Users/serkanbicici/Desktop/Centerline Extraction/Files in LAS")
#Path2 <- paste0("C:/Users/serkanbicici/Desktop/Centerline Extraction/Voronoi_Diagram_Results/Degree_",dg)


library(lidR)
library(raster)
library(rgdal)
library(sp)
library(deldir)
library(rgeos)
library(rmapshaper)
library(MASS)
library(ggplot2)
library(concaveman)
library(MASS)


### Step 1: load extracted road
setwd(Path1)
df <- readLAS(files = paste0("3_Final_Road.las"))
road <- data.frame(x=df@data$X,y=df@data$Y,z=df@data$Z)
colnames(road) <- c("X", "Y", "Z")
rm(df)

### Step2: Get initial boundary
points <- data.frame(x=road$X,y=road$Y)
points <- as.matrix(points)
Initial_boundary <- concaveman(points) 
rm(points)

### Step3: Get initial boundary
points <- data.frame(x=road$X,y=road$Y)
points <- as.matrix(points)
Improved_boundary <- concaveman(points, 
                                concavity=concavity,
                                length_threshold=length_threshold)
rm(points,concavity,length_threshold,road)

Xmin = min(   min(Initial_boundary[,1]),    min(Improved_boundary[,1]) )
Xmax = max(   max(Initial_boundary[,1]),    max(Improved_boundary[,1]) )
Ymin = min(   min(Initial_boundary[,2]),    min(Improved_boundary[,2]) )
Ymax = max(   max(Initial_boundary[,2]),    max(Improved_boundary[,2]) )
plot(Initial_boundary,type = "l",col='red',
     xlim = c(Xmin,Xmax),
     ylim = c(Ymin,Ymax))
par(new=TRUE)
plot(Improved_boundary,type = "l",col='green',
     xlim = c(Xmin,Xmax),
     ylim = c(Ymin,Ymax))
rm(Xmin,Xmax,Ymin,Ymax)

### Step4: Clearing small edge (Before extracting the centerline, we remove small edge)
Improved_boundary <- data.frame(Improved_boundary)
colnames(Improved_boundary) <- c("V1", "V2")


p1 <- Improved_boundary[Improved_boundary$V1==min(Improved_boundary$V1),]
p2 <- Improved_boundary[Improved_boundary$V2==max(Improved_boundary$V2),]
p3 <- Improved_boundary[Improved_boundary$V2==min(Improved_boundary$V2),]
p4 <- Improved_boundary[Improved_boundary$V1==max(Improved_boundary$V1),]

d2 <- ( (p1$V1[1]-p2$V1[1])^2 + (p1$V2[1]-p2$V2[1])^2 ) ^ (1/2)
d3 <- ( (p1$V1[1]-p3$V1[1])^2 + (p1$V2[1]-p3$V2[1])^2 ) ^ (1/2)
d4 <- ( (p1$V1[1]-p4$V1[1])^2 + (p1$V2[1]-p4$V2[1])^2 ) ^ (1/2)
if (d2<=d3 & d2<=d4){
  if (p1[1,1]<p2[1,1]){    P1 <- p1[1,];    P2 <- p2[1,]  };  if (p1[1,1]>p2[1,1]){    P1 <- p2[1,];    P2 <- p1[1,]  }
  if (p3[1,1]<p4[1,1]){    P3 <- p3[1,];    P4 <- p4[1,]  };  if (p3[1,1]>p4[1,1]){    P3 <- p4[1,];    P4 <- p3[1,]  }
}
if (d3<=d2 & d3<=d4){
  if (p1[1,1]<p3[1,1]){    P1 <- p1[1,];    P2 <- p3[1,]  };  if (p1[1,1]>p3[1,1]){    P1 <- p3[1,];    P2 <- p1[1,]  }
  if (p2[1,1]<p4[1,1]){    P3 <- p2[1,];    P4 <- p4[1,]  };  if (p2[1,1]>p4[1,1]){    P3 <- p4[1,];    P4 <- p3[1,]  }
}
if (d4<=d2 & d4<=d3){
  if (p1[1,1]<p4[1,1]){    P1 <- p1[1,];    P2 <- p4[1,]  };  if (p1[1,1]>p4[1,1]){    P1 <- p4[1,];    P2 <- p1[1,]  }
  if (p2[1,1]<p3[1,1]){    P3 <- p2[1,];    P4 <- p3[1,]  };  if (p2[1,1]>p3[1,1]){    P3 <- p3[1,];    P4 <- p2[1,]  }
}
rm(p1,p2,p3,p4,d2,d3,d4)

diffy1 <- diff/(P3$V1-P1$V1)*(P1$V2-P3$V2)
Cor1 <- data.frame(V1=P1$V1[1]+diff,V2=P1$V2[1]-diffy1)
Cor2 <- data.frame(V1=P3$V1[1]-diff,V2=P3$V2[1]+diffy1)

diffy2 <- diff/(P4$V1-P2$V1)*(P2$V2-P4$V2)
Cor3 <- data.frame(V1=P2$V1[1]+diff,V2=P2$V2[1]-diffy2)
Cor4 <- data.frame(V1=P4$V1[1]-diff,V2=P4$V2[1]+diffy2)

Cor5 <- data.frame(V1=P1$V1[1]+diff,V2=P3$V2[1]+diffy1)
Cor6 <- data.frame(V1=P4$V1[1]-diff,V2=P2$V2[1]-diffy2)
rm(diffy1,diffy2,diff)

Buffer <-rbind(Cor5,Cor1,Cor3,Cor6,Cor4,Cor2,Cor5)

rm(Po,Po2)
rm(P1,P2,P3,P4,Cor1,Cor2,Cor3,Cor4,Cor5,Cor6)

Improved_boundary[,3] <- 1
colnames(Improved_boundary) <- c("X", "Y", "Z")
pcl <- LAS(Improved_boundary)
clipped.las <- clip_polygon(pcl,Buffer[,1],Buffer[,2])
Improved_boundary2 <- data.frame(clipped.las@data[["X"]],clipped.las@data[["Y"]],clipped.las@data[["Z"]])
colnames(Improved_boundary2) <- c("X", "Y", "Z")
rm(clipped.las)

plot(Improved_boundary$X,Improved_boundary$Y,
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="black",xlab ="X",ylab="Y")
par(new=TRUE)
plot(Improved_boundary2$X,Improved_boundary2$Y,
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="red",xlab ="X",ylab="Y")
par(new=TRUE)
plot(Buffer[,1],Buffer[,2], type="l",
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="grey",xlab ="X",ylab="Y")
rm(pcl,Buffer)
################################################################################
coordinates(Improved_boundary2) <- ~X + Y

### Step 5: Apply voronoi diagram
rVoronoi <- tile.list(deldir(Improved_boundary2@coords[,1], Improved_boundary2@coords[,2]))
Vertax <- SpatialPoints(do.call(rbind,lapply(rVoronoi, function(x) cbind(x$x, x$y))))
plot(rVoronoi)
plot(Vertax)
rm(rVoronoi)

### Step 6: Remove vertaces that are outside the road boundary
Improved_boundary2 <- Polygon(Improved_boundary2@coords)
ps <- Polygons(list(Improved_boundary2),1)
sps <- SpatialPolygons(list(ps))
Vertax <- gIntersection(gBuffer(sps, width=-1), Vertax)
plot(Vertax)
rm(ps)

### Step 7: Creating lines from vertax
rLine <- SpatialLines(list(Lines(Line(Vertax), ID="1")))
rLine.simp <- ms_simplify(rLine, keep = 0.1)
plot(sps,axes = TRUE)
plot(rLine, col="red", add=TRUE)
plot(rLine.simp, col="green", add=TRUE)
line.df <- data.frame(x=Vertax@coords[,1],y=Vertax@coords[,2])
plot(line.df)
rm(Vertax,rLine.simp)

### Step 8: Fitting 2 degree line in centerline
lm.poly <- rlm(y~poly(x,dg),data=line.df)  # Robust fit
#lm.poly <- lm(y ~ poly(x,dg),data=line.df)
plot(line.df$x,lm.poly$fitted.values)
cor.line <- data.frame(x=line.df$x,y=lm.poly$fitted.values)

plot(cor.line$x,cor.line$y)
rLine2 <- SpatialLines(list(Lines(Line(cor.line), ID="1")))

rm(cor.line,lm.poly,Improved_boundary2,rLine)
rm(sps)

### Step 9: Saving Boundary and Centerline
setwd(Path2)
Initial_boundary <- data.frame(Initial_boundary[,1],Initial_boundary[,2])
Initial_boundary[,3] <- 0
colnames(Initial_boundary) <- c("X", "Y", "Z")
lasData <- LAS(Initial_boundary)
writeLAS(lasData, paste0("Initial_boundary.las"))
rm(lasData)
setwd(Path2)
colnames(Improved_boundary) <- c("X", "Y", "Z")
lasData <- LAS(Improved_boundary)
writeLAS(lasData, paste0("Improved_boundary.las"))
rm(lasData)

setwd(Path2)
Centerline <- data.frame(rLine2@lines[[1]]@Lines[[1]]@coords[,1],rLine2@lines[[1]]@Lines[[1]]@coords[,2])
Centerline[,3] <- 0
colnames(Centerline) <- c("X", "Y", "Z")
lasData <- LAS(Centerline)
writeLAS(lasData, paste0("CenterlineFromVoronoi",dg,".las"))

setwd(Path2)
shapefile(rLine2, 'Centerline.shp', overwrite=TRUE)
#LineLength(as.matrix(line.df), longlat = FALSE, sum = TRUE)
rm(lasData,rLine2)

setwd(Path2)
png(filename="Centerline.png")
plot(Centerline$X,Centerline$Y, type="l",lty=2,
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="green",lwd=1,xlab ="X",ylab="Y")
par(new=TRUE)
plot(Initial_boundary$X,Initial_boundary$Y, type="l",
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="red",lwd=1,xlab ="X",ylab="Y")
par(new=TRUE)
plot(Improved_boundary$X,Improved_boundary$Y, type="l",
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="black",lwd=1,xlab ="X",ylab="Y")
legend("bottomleft",legend = c("Centerline","Initial Boundary","Improved Boundary"),
       lty=c(2,1,1),lwd=c(3,3,2),col=c("green","red","black"))
dev.off()

setwd(Path2)
setEPS()
postscript("Centerline.eps")
plot(Centerline$X,Centerline$Y, type="l",lty=2,
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="green",lwd=1,xlab ="X",ylab="Y")
par(new=TRUE)
plot(Initial_boundary$X,Initial_boundary$Y, type="l",
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="red",lwd=1,xlab ="X",ylab="Y")
par(new=TRUE)
plot(Improved_boundary$X,Improved_boundary$Y, type="l",
     xlim=c(min(Improved_boundary$X),max(Improved_boundary$X)),
     ylim=c(min(Improved_boundary$Y),max(Improved_boundary$Y)),
     col="black",lwd=1,xlab ="X",ylab="Y")
legend("bottomleft", legend = c("Centerline","Initial Boundary","Improved Boundary"),
       lty=c(2,1,1),lwd=c(3,3,2),col=c("green","red","black"))
dev.off()



