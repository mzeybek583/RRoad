# Load necessary libraries
library(rgl)
library(MASS)   # For robust covariance (cov.rob)
library(rrcov)  # For robust PCA

# Simulate 3D point cloud (for demonstration)
set.seed(123)
n <- 1000

# Parameters for the point cloud (e.g., simulating a cylinder shape)
cylinder_radius <- 3
cylinder_height <- 10
theta <- runif(n, 0, 2 * pi)
z <- runif(n, -cylinder_height / 2, cylinder_height / 2)
x <- cylinder_radius * cos(theta)
y <- cylinder_radius * sin(theta)

# Add some noise and a few outliers to the points to simulate a real-world dataset
noise <- rnorm(n * 3, 0, 0.1)
point_cloud <- cbind(x + noise[1:n], y + noise[(n+1):(2*n)], z + noise[(2*n+1):(3*n)])

# Adding outliers
outliers <- matrix(rnorm(30, mean = 10), ncol = 3)
point_cloud <- rbind(point_cloud, outliers)

# Visualize the 3D point cloud with outliers
plot3d(point_cloud[, 1], point_cloud[, 2], point_cloud[, 3], col = "blue", size = 5)

# Function for robust covariance estimation with MCD
robust_covariance <- function(points) {
  cov_mcd <- covMcd(points)
  return(list(center = cov_mcd$center, cov_matrix = cov_mcd$cov))
}

# Function for iterative robust PCA
iterative_robust_pca <- function(points, max_iter = 10, tol = 1e-3, outlier_threshold = 3) {
  for (i in 1:max_iter) {
    # Estimate robust covariance matrix using MCD
    cov_est <- robust_covariance(points)
    
    # Perform PCA on the robust covariance matrix
    pca_result <- prcomp(points, center = cov_est$center, scale. = FALSE)
    
    # Compute Mahalanobis distance to filter out potential outliers
    mahal_dist <- mahalanobis(points, center = cov_est$center, cov = cov_est$cov_matrix)
    
    # Filter out points based on Mahalanobis distance (outlier detection)
    points_filtered <- points[mahal_dist < outlier_threshold, ]
    
    # Stop if the dataset is no longer changing significantly
    if (nrow(points_filtered) == nrow(points)) {
      break
    }
    
    points <- points_filtered
  }
  
  return(pca_result)
}

# Perform Iterative Robust PCA
robust_pca_result <- iterative_robust_pca(point_cloud)

# Get the robust neutral axis (first robust principal component)
robust_neutral_axis <- robust_pca_result$rotation[, 1]  # First robust principal component

# Print the robust neutral axis
cat("Robust neutral axis (first robust principal component):\n", robust_neutral_axis, "\n")

# Add the robust neutral axis to the plot for visualization
center_of_cloud <- colMeans(point_cloud)  # Mean of the point cloud (centroid)

# Create points along the robust neutral axis for visualization
line_length <- 10  # Length of the line for visualizing the neutral axis
neutral_line <- rbind(center_of_cloud - line_length * robust_neutral_axis, 
                      center_of_cloud + line_length * robust_neutral_axis)

# Add the robust neutral axis to the 3D plot
lines3d(neutral_line[, 1], neutral_line[, 2], neutral_line[, 3], col = "red", lwd = 3)
