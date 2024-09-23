# Install necessary packages if not installed
if (!require(rgl)) install.packages("rgl")
if (!require(pracma)) install.packages("pracma")  # For cross product to compute normals

library(rgl)
library(pracma)

# Function to generate cylinder points
generate_cylinder <- function(radius = 1, height = 2, n_points = 1000) {
  theta <- runif(n_points, 0, 2 * pi)
  z <- runif(n_points, -height / 2, height / 2)
  x <- radius * cos(theta)
  y <- radius * sin(theta)
  
  # Return points as a matrix
  points <- cbind(x, y, z)
  return(points)
}

# Function to calculate normals for cylinder points
calculate_normals <- function(points) {
  normals <- matrix(NA, nrow = nrow(points), ncol = 3)
  
  for (i in 1:nrow(points)) {
    x <- points[i, 1]
    y <- points[i, 2]
    
    # Normal for a cylinder is radial, ignore the z-component
    normal <- c(x, y, 0)
    
    # Normalize the normal vector
    normals[i, ] <- normal / norm(normal, type = "2")
  }
  
  return(normals)
}

# Function to plot cylinder with normals
plot_cylinder_with_normals <- function(points, normals, sample_rate = 50) {
  plot3d(points, col = "blue", size = 1, aspect = TRUE, type = "s")
  
  # Sample points for normal arrows
  sampled_idx <- seq(1, nrow(points), by = sample_rate)
  
  # Plot normals as arrows
  for (i in sampled_idx) {
    arrow3d(
      p0 = points[i, ],                # Starting point
      p1 = points[i, ] + normals[i, ] / 4,  # Arrow end point (scaled)
      col = "red", type = "flat"
    )
  }
  
  # Add axis
  axes3d()
}

# Generate 3D points for the cylinder
cylinder_points <- generate_cylinder(radius = 1, height = 2, n_points = 1000)

# Calculate surface normals
cylinder_normals <- calculate_normals(cylinder_points)

# Plot the cylinder and surface normals
plot_cylinder_with_normals(cylinder_points, cylinder_normals, sample_rate = 50)
