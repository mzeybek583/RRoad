
## Profiler

library(raster)
library(purrr)
library(tibble)
library(geosphere)
library(rgdal)
str_name<-"50m_dik_dsm.tif"
rr=raster(str_name)


#crs(rr) <- CRS("+init=epsg:5258")
#crs(rr) <- CRS("+init=epsg:4326")

#crs(rr)

c.line <- shapefile("centerline.shp")

#crs(c.line) <- CRS("+init=epsg:5258")
#crs(c.line)

#newcrs <- CRS("+init=epsg:4326")

#c.line_crs <- spTransform(c.line, newcrs)

#crs(c.line_crs)

elevations <- extract(rr, c.line, along=TRUE, cellnumbers=TRUE)

#> Warning in proj4string(x): CRS object has comment, which is lost in output

elev_df = purrr::map_dfr(elevations, as_data_frame, .id = "ID")

transect_coords = xyFromCell(rr, elev_df$cell)

library(spatstat)

pair_dist <- pairdist(transect_coords)

#elev_df$dist = c(0, cumsum(pair_dist)) 
plot(rr,cex.axis=1.5)
plot(c.line, add=TRUE, col="red", lwd=2)
plot(pair_dist[,1], elev_df$X50m_dik_dsm, 
     xlab="Distance (m)", ylab="Elevation (m)",
     cex=0.1, cex.axis=1.5, cex.lab=1.5)
#pair_dist = geosphere::distGeo(transect_coords)[-nrow(transect_coords)]
