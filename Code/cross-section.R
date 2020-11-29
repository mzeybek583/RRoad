

## Cross sectioner function

library(rgdal)
library(sp)
library(raster)
library(tictoc)

tic()
str_name<-"50m_dik_dsm.tif"
rr=raster(str_name)

c.line <- readOGR(dsn = "centerline.shp")
c.line.pts <- data.frame(coordinates(c.line))
colnames(c.line.pts) <- c("y","x")
plot(c.line.pts,  xlim = c(min(c.line.pts$y)-50, max(c.line.pts$y)+50), ylim=c(min(c.line.pts$x)-50, max(c.line.pts$x)+50) )

line.length <- SpatialLinesLengths(c.line)
dist <- 10
enk <- round(line.length/dist)

loc <- spsample(c.line, enk, "regular")
loc.coord <- data.frame(coordinates(loc))
colnames(loc.coord) <- c("y","x")

plot(loc, axes=TRUE, xlim = c(min(c.line.pts$y)-50, max(c.line.pts$y)+50), 
     ylim=c(min(c.line.pts$x)-50, max(c.line.pts$x)+50), cex.axis=1.5)
plot(c.line, col="red", add=TRUE)

dat <- data.frame()

for (i in 1:nrow(loc.coord)) {
  print(i)
  id1<- (paste("P.",i,"_sag", sep = ""))
  id2 <- (paste("P.",i,"_sol",sep = ""))
  P1 <- data.frame(loc.coord$y[i],loc.coord$x[i])
  colnames(P1) <- c("y", "x")
  P2 <- data.frame(loc.coord$y[i+1],loc.coord$x[i+1])
  colnames(P2) <- c("y", "x")
  
  if ((i+1)>nrow(loc.coord)) {
    P2 <- data.frame(loc.coord$y[i-1],loc.coord$x[i-1])
    colnames(P2) <- c("y", "x")
    deltax <- P2$x - P1$x
    deltay <- P2$y - P1$y
    semt <- atan(deltay/deltax)*200/pi
    if (semt<200) {
      semt <- semt+200
      
    } else {
      semt <- semt-200
    }
    if (deltay && deltax >0) {
      semt <- semt
    } else if (deltay >0 && deltax < 0){
      semt <- semt+200
    } else if (deltay <0 && deltax < 0){
      semt <- semt+200
    } else if (deltay <0 && deltax >0){
      semt <- semt + 400
    }
    
    a1 <- 100
    a2 <- 300
    d <- 20
    
    y_sag <- P1$y + d*sin ((semt+a1)*pi/200)
    x_sag <- P1$x + d*cos((semt+a1)*pi/200)
    y_sol <- P1$y + d*sin ((semt+a2)*pi/200)
    x_sol <- P1$x + d*cos((semt+a2)*pi/200)
    
    P3=data.frame(ID=id1, y=y_sag, x=x_sag)
    P4= data.frame(ID=id2, y=y_sol, x=x_sol)
    points <- data.frame(rbind(P3, P4 ))
    
    # plot(points, col="green", add=TRUE)
    dat <- rbind(dat,P3)
    dat <- rbind(dat,P4)
    
  } else {
    deltax <- P2$x - P1$x
    deltay <- P2$y - P1$y
    semt <- atan(deltay/deltax)*200/pi
    if (deltay && deltax >0) {
      semt <- semt
    } else if (deltay >0 && deltax < 0){
      semt <- semt+200
    } else if (deltay <0 && deltax < 0){
      semt <- semt+200
    } else if (deltay <0 && deltax >0){
      semt <- semt + 400
    }
    
    a1 <- 100
    a2 <- 300
    d <- 20
    
    y_sag <- P1$y + d*sin ((semt+a1)*pi/200)
    x_sag <- P1$x + d*cos((semt+a1)*pi/200)
    y_sol <- P1$y + d*sin ((semt+a2)*pi/200)
    x_sol <- P1$x + d*cos((semt+a2)*pi/200)
    
    P3=data.frame(ID=id1, y=y_sag, x=x_sag)
    P4= data.frame(ID=id2, y=y_sol, x=x_sol)
    points <- data.frame(rbind(P3, P4 ))
    
    # plot(points, col="green", add=TRUE)
    dat <- rbind(dat,P3)
    dat <- rbind(dat,P4)
  }
  
}

i<-1
sp.lines <- list()

while (i <= nrow(dat)) {
  line_obj <- sp::Line(cbind(dat$y[i:(i+1)],dat$x[i:(i+1)]))
  
  lines_obj <- sp::Lines(list(line_obj),ID=dat$ID[i])
  
  sp.lines[[i]] <- sp::SpatialLines(list(lines_obj))
  crs(sp.lines[[i]]) <- CRS("+init=epsg:5258")
  i<-i+2
}

sp.lines[sapply(sp.lines, is.null)] <- NULL
sp.lines
summary(sp.lines)
merged.lines <- do.call(rbind, sp.lines)
class(merged.lines)
length(merged.lines)
plot(merged.lines, col=1:(nrow(loc.coord)),add=TRUE)




# Get elevations from DEM -------------------------------------------------

plot(rr, cex.axis=1.5)
plot(merged.lines, col=1:(nrow(loc.coord)),add=TRUE, lwd=2)
plot(c.line, col="red", add=TRUE, lwd=2)

elevations <- extract(rr, merged.lines, along=TRUE, cellnumbers=TRUE)

library(purrr)
library(tibble)
for (i in 1:length(elevations)) {
  print(i)
  elev_df = purrr::map_dfr(elevations[i], as_data_frame, .id = "ID")
  
  transect_coords = xyFromCell(rr, elev_df$cell)
  
  library(spatstat)
  
  pair_dist <- pairdist(transect_coords)
  #elev_df$dist = c(0, cumsum(pair_dist)) 
  # plot(rr)
  #  plot(c.line, add=TRUE, col="red", lwd=2)
  plot(pair_dist[,1], elev_df$X50m_dik_dsm, 
       xlab="Distance (m)", ylab="Elevation (m)",
       cex=0.1, type="b", cex.axis=1.5, cex.lab=1.5)
}

toc()

