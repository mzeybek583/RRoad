
## Calculate centerline of polygon

#library(cmgo)

library(raster)
library(rgdal)

#poly <- readOGR(dsn = "road_polygon.shp")

poly<- read.csv("convex_hull_pts.csv")
library(sp)
coordinates(poly) <- ~X + Y
## Find points on boundary of rPoly (see question)
#rPolyPts <-  coordinates(as(as(poly, "SpatialLinesDataFrame"),
 #                          "SpatialPointsDataFrame"))

#plot(rPolyPts)
## Perform Voronoi tessellation of those points and extract coordinates of tiles


library(deldir)
rVoronoi <- tile.list(deldir(poly@coords[,1], poly@coords[,2]))
rVoronoiPts <- SpatialPoints(do.call(rbind, 
                                     lapply(rVoronoi, function(x) cbind(x$x, x$y))))
plot(rVoronoi)
plot(rVoronoiPts)

## Find the points on the Voronoi tiles that fall inside 
## the linear feature polygon
## N.B. That the width parameter may need to be adjusted if coordinate
## system is fractional (i.e. if longlat), but must be negative, and less
## than the dimension of a cell on the original raster.
library(rgeos)
poly <- Polygon(poly@coords)

ps = Polygons(list(poly),1)
sps = SpatialPolygons(list(ps))
plot(sps)

rLinePts <- gIntersection(gBuffer(sps, width=-1), rVoronoiPts)

## Create SpatialLines object
rLine <- SpatialLines(list(Lines(Line(rLinePts), ID="1")))

library(rmapshaper)
rLine.simp <- ms_simplify(rLine, keep = 0.1)


plot(sps,axes = TRUE)
plot(rLine, col="red", add=TRUE)
plot(rLine.simp, col="green", add=TRUE)


library(MASS)

line.df <- data.frame(x=rLinePts@coords[,1],y=rLinePts@coords[,2])

plot(line.df)

lm.poly <- lm(y ~ poly(x,5), data=line.df)
summary(lm.poly)

#plot(lm.poly)

plot(line.df$x,lm.poly$fitted.values)
cor.line <- data.frame(x=line.df$x,y=lm.poly$fitted.values)
rLine2 <- SpatialLines(list(Lines(Line(cor.line), ID="1")))

plot(sps, axes=TRUE)
plot(rLine, col="red", add=TRUE)
plot(rLine2, col="green", add=TRUE)

outfile <- 'centerline.shp'
shapefile(rLine2, outfile, overwrite=TRUE)
LineLength(as.matrix(line.df), longlat = FALSE, sum = TRUE)

writeOGR(rLine2, dsn= "centerline.shp", driver = "ESRI Shapefile")


