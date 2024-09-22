# Load necessary libraries
library(rgl)
library(MASS)   # For Mahalanobis distance
library(rrcov)  # For robust PCA

# Simulate 3D point cloud for a cylinder
set.seed(123)
n <- 1000

# Parameters for the cylinder (length and radius)
cylinder_radius <- 3
cylinder_height <- 20

# Generate points around the cylinder
theta <- runif(n, 0, 2 * pi)
z <- runif(n, -cylinder_height / 2, cylinder_height / 2)
x <- cylinder_radius * cos(theta)
y <- cylinder_radius * sin(theta)

# Add some noise and a few outliers to the points to simulate a real-world dataset
point_cloud <- cbind(x, y, z)

# Adding outliers
outliers <- matrix(rnorm(30, mean = 10), ncol = 3)
point_cloud <- rbind(point_cloud, outliers)

# Perform Robust PCA to find the neutral axis (cylinder's axis)
robust_pca <- PcaHubert(point_cloud, k = 3)  # k is the number of principal components

# Get the robust neutral axis (first robust principal component)
neutral_axis <- robust_pca@loadings[, 1]  # First robust principal component (cylinder axis)

# Print the neutral axis
cat("Robust neutral axis (first principal component - cylinder axis):\n", neutral_axis, "\n")

# Step 1: Compute Mahalanobis distances for outlier detection
# Compute robust covariance using cov.rob
robust_cov <- cov.rob(point_cloud)

# Compute Mahalanobis distances
mahal_dist <- mahalanobis(point_cloud, robust_cov$center, robust_cov$cov)

# Define a threshold for inliers (points with Mahalanobis distance < threshold are considered inliers)
inlier_threshold <- qchisq(0.975, df = 3)  # 97.5% confidence level

# Identify inliers and outliers
inliers <- point_cloud[mahal_dist < inlier_threshold, ]
outliers <- point_cloud[mahal_dist >= inlier_threshold, ]

# Step 2: Project inliers onto the neutral axis
# The projection is the dot product of each point with the neutral axis
projected_distances <- inliers %*% neutral_axis

# Visualize the entire 3D point cloud with inliers in green and outliers in red
open3d()
plot3d(outliers[, 1], outliers[, 2], outliers[, 3], col = "red", size = 5, add = FALSE)  # Outliers in red
plot3d(inliers[, 1], inliers[, 2], inliers[, 3], col = "green", size = 5, add = TRUE)   # Inliers in green

# Step 3: Add the neutral axis to the plot (show it in blue)
center_of_cloud <- colMeans(point_cloud)  # Mean of the point cloud (centroid)
line_length <- 20  # Length of the line for visualizing the neutral axis
neutral_line <- rbind(center_of_cloud - line_length * neutral_axis, 
                      center_of_cloud + line_length * neutral_axis)

# Add the neutral axis (blue) to the plot
lines3d(neutral_line[, 1], neutral_line[, 2], neutral_line[, 3], col = "blue", lwd = 3)
title3d(main = "Cylinder Point Cloud with Neutral Axis (blue), Inliers (green), Outliers (red)")

# Step 4: Slice the cylinder perpendicular to the neutral axis
# Define the number of slices and slice thickness
num_slices <- 5
slice_thickness <- 3  # Slice thickness along the neutral axis

# Loop through each slice and filter points within the slice
for (i in 1:num_slices) {
  # Define the range of the slice along the neutral axis
  lower_bound <- min(projected_distances) + (i - 1) * slice_thickness
  upper_bound <- lower_bound + slice_thickness
  
  # Find the points within the slice
  slice_indices <- which(projected_distances >= lower_bound & projected_distances < upper_bound)
  
  # Extract the slice points
  slice_points <- inliers[slice_indices, ]
  
  # Open a new window for each slice visualization
  open3d()
  
  # Visualize the slice in 3D
  plot3d(slice_points[, 1], slice_points[, 2], slice_points[, 3], col = "green", size = 5)
  
  # Optionally: Add a label for the slice in the 3D plot
  title3d(main = paste("Slice", i, "- Inliers Only"))
}

# You can adjust the number of slices or the thickness as needed
