

## Import CC segmented LAS point Cloud
## Data


library(lidR)
library(alphahull)
library(igraph)

veri <- readLAS("CC_extracted_road_rm_duplicated2.las")
# density of raw data
plot(grid_density(veri))

# Select points randomly to reach an homogeneous density of 1
thinned <- decimate_points(veri, homogenize(1,30))
plot(grid_density(thinned))
#plot(thinned)
x <- thinned@data
x <- x[,1:2]


# Alpha-convex hull
alpha <- 3
ahull.obj <- ashape(x, alpha = alpha)
plot(ahull.obj, cex=0.1)


ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[,1])
  ashape_graph <- graph_from_edgelist(ashape$edges[,1:2], directed = FALSE)
  if (!is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - E(ashape_graph)[1]
  # Find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(ashape_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}


alphapoly_1 <- ashape2poly(ahull.obj)


plot(x, pch = 19, col = "darkseagreen")
# show the original alpha shape
plot(ahull.obj, lwd = 5, col = "gray", add = TRUE)
# plot the new polygon
lines(x[alphapoly_1, ], col = "magenta")

pts <- x[alphapoly_1,]

plot(pts)

write.csv(pts, file = "convex_hull_pts.csv", row.names = FALSE)


