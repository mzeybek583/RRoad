
# Centerline plo


# Load Libraries ----------------------------------------------------------
library(lidR)
library(rgdal)

# Read data ---------------------------------------------------------------
las <- readLAS("data/Boundary and Centerline-20201211T173158Z-001/Boundary and Centerline/Centerline.las")
las2 <- readLAS(files = "data/Boundary and Centerline-20201211T173158Z-001/Boundary and Centerline/Initial_boundary.las")
las3 <- readLAS(files = "data/Boundary and Centerline-20201211T173158Z-001/Boundary and Centerline/Improved_boundary.las")

centerline <- shapefile(x = "data/Boundary and Centerline-20201211T173158Z-001/Boundary and Centerline/Centerline.shp")
c_coord <- data.frame(centerline@lines[[1]]@Lines[[1]]@coords)
colnames(c_coord) <- c("x","y")
plot(centerline)
init <- data.frame(las2@data[,1:2])
colnames(init) <- c("x","y")
improved <- data.frame(las3@data[,1:2])
colnames(improved) <- c("x","y")
library(ggplot2)
library(concaveman)
library(rgdal)
library(raster)

polygon <- concaveman(as.matrix(init))
polygon2 <- concaveman(as.matrix(improved))

init <- data.frame(polygon)
colnames(init) <- c("x","y")
improved <- data.frame(polygon2)
colnames(improved) <- c("x","y")
#plot(points)
#plot(polygon, add = TRUE, type = "l")

prj <- CRS("+init=epsg:5254")

sp <- SpatialPolygons(list(Polygons(list(Polygon(polygon)), ID=1)))

colors <- c("Centerline" = "black", "Initial boundary" = "red", 
            "Refined boundary" = "green")
ggplot() + 
  geom_line(data = c_coord, aes(x = x, y = y, color = "Centerline"))+
  geom_polygon(data = init, aes(x=x, y=y, color = "Initial boundary"), fill = NA)+
  geom_polygon(data = improved, aes(x=x, y=y, color ="Refined boundary"), fill = NA)+
  theme_bw(base_size = 22)+
  labs(x="X", y= "Y", color = "Legend")+
  scale_color_manual(values = colors)+
  xlim(451940,451992)
ggsave("centerline.eps", width = 12, height = 6, dpi = 600)
