# Load necessary libraries
library(pracma)
library(rgl)

# Simulate 3D point cloud for a road surface
set.seed(123)
n <- 1000  # Number of points

# Road surface 1: A sloped plane (e.g., one side of the road)
x1 <- runif(n, min = -10, max = 0)
y1 <- runif(n, min = -5, max = 5)
z1 <- 0.5 * x1 + 0.1 * y1 + rnorm(n, sd = 0.1)

# Road surface 2: Another plane (e.g., other side of the road)
x2 <- runif(n, min = 0, max = 10)
y2 <- runif(n, min = -5, max = 5)
z2 <- 0.3 * x2 - 0.1 * y2 + rnorm(n, sd = 0.1)

# Combine both surfaces into one point cloud
x <- c(x1, x2)
y <- c(y1, y2)
z <- c(z1, z2)

# Create point cloud data
point_cloud <- cbind(x, y, z)

# Visualize the point cloud
plot3d(x, y, z, col = "blue", size = 5)

# Function to fit a plane using RANSAC
ransac_plane_fit <- function(points, iterations = 1000, threshold = 0.1) {
  best_inliers <- 0
  best_model <- NULL
  
  for (i in 1:iterations) {
    # Randomly select 3 points
    sample_points <- points[sample(1:nrow(points), 3), ]
    
    # Define two vectors from three points
    v1 <- sample_points[2, ] - sample_points[1, ]
    v2 <- sample_points[3, ] - sample_points[1, ]
    
    # Compute the normal to the plane using cross-product
    normal <- cross(v1, v2)
    
    # Compute the plane equation ax + by + cz + d = 0
    a <- normal[1]
    b <- normal[2]
    c <- normal[3]
    d <- -sum(normal * sample_points[1, ])
    
    # Calculate distance of all points to the plane
    distances <- abs(a * points[, 1] + b * points[, 2] + c * points[, 3] + d) /
      sqrt(a^2 + b^2 + c^2)
    
    # Count inliers (points within the threshold distance)
    inliers <- sum(distances < threshold)
    
    # Update best model if more inliers are found
    if (inliers > best_inliers) {
      best_inliers <- inliers
      best_model <- c(a, b, c, d)
    }
  }
  
  # Return the best model and the inliers
  distances <- abs(best_model[1] * points[, 1] + best_model[2] * points[, 2] + best_model[3] * points[, 3] + best_model[4]) /
    sqrt(best_model[1]^2 + best_model[2]^2 + best_model[3]^2)
  inliers_idx <- which(distances < threshold)
  
  return(list(model = best_model, inliers = inliers_idx, residuals = distances))
}

# Fit the first RANSAC plane
result1 <- ransac_plane_fit(point_cloud)
plane1 <- result1$model
inliers1 <- result1$inliers
residuals1 <- result1$residuals

# Remove the inliers of the first plane from the point cloud
point_cloud_remaining <- point_cloud[-inliers1, ]

# Fit the second RANSAC plane on the remaining points
result2 <- ransac_plane_fit(point_cloud_remaining)
plane2 <- result2$model
inliers2 <- result2$inliers
residuals2 <- result2$residuals

# Print the plane equations
cat("Plane 1 equation: ", plane1[1], "x +", plane1[2], "y +", plane1[3], "z +", plane1[4], "= 0\n")
cat("Plane 2 equation: ", plane2[1], "x +", plane2[2], "y +", plane2[3], "z +", plane2[4], "= 0\n")

# Visualization of fitted planes and point cloud
plot3d(x, y, z, col = "blue", size = 5)

# Function to plot a plane
plot_plane <- function(plane, xlim, ylim, color = "red") {
  grid_x <- seq(xlim[1], xlim[2], length.out = 30)
  grid_y <- seq(ylim[1], ylim[2], length.out = 30)
  grid <- expand.grid(grid_x, grid_y)
  grid_z <- (-plane[1] * grid[, 1] - plane[2] * grid[, 2] - plane[4]) / plane[3]
  
  # Draw the plane
  surface3d(grid_x, grid_y, matrix(grid_z, 30, 30), col = color, alpha = 0.5)
}

# Add the fitted planes to the plot
plot_plane(plane1, xlim = c(-10, 0), ylim = c(-5, 5), color = "red")
plot_plane(plane2, xlim = c(0, 10), ylim = c(-5, 5), color = "green")

# Plot residuals to assess fit quality
par(mfrow = c(2, 1))  # Plot two histograms side by side

# Residuals for Plane 1
hist(residuals1, breaks = 30, main = "Residuals for Plane 1", xlab = "Distance from Plane", col = "lightblue")
cat("Mean residual for Plane 1: ", mean(residuals1), "\n")

# Residuals for Plane 2
hist(residuals2, breaks = 30, main = "Residuals for Plane 2", xlab = "Distance from Plane", col = "lightgreen")
cat("Mean residual for Plane 2: ", mean(residuals2), "\n")
