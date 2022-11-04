

# Extract Road Surface ----------------------------------------------------


# Load Libs ---------------------------------------------------------------

library(lidR)
library(cloudcompare)


# Read Data ---------------------------------------------------------------

data <- readLAS("data/distress_road_clipped_sample1cm.las")


# Remove noise ------------------------------------------------------------
las <- classify_noise(data, ivf(5,2))
las_denoise <- filter_poi(las, Classification != LASNOISE)


plot(las_denoise)

if (!dir.exists("data/Result")) {dir.create("data/Result")}


writeLAS(las_denoise,"data/Result/las_denoised.las")
rm(las_denoise)

# CloudCompare Feature Extract --------------------------------------------

las <- readLAS("data/Result/las_denoised.las") 
las@header@PHB[["X scale factor"]] <- 0.001
las@header@PHB[["Y scale factor"]] <- 0.001
las@header@PHB[["Z scale factor"]] <- 0.001
CC_dir <- set_CCdir(CCdir = "C:/Program Files/CloudCompare/CloudCompare.exe")

pc_files <- list.files("data/Result/", 
                       pattern='las_denoised.las$',full.names=T)

if (!dir.exists("data/CC_Result")) {dir.create("data/CC_Result")}

#setwd("data/CC_Result/")

# Compute Features --------------------------------------------------------

r <- 0.05 # Radius

CC(feature(file = pc_files, 
           type = "VERTICALITY",
           radius = r,
           output_dir = "data/CC_Result/verticality",
           global_shift = T,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))

CC(feature(file = pc_files, 
           type = "PLANARITY",
           radius = r,
           output_dir = "data/CC_Result/planarity",
           global_shift = T,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))
CC(feature(file = pc_files, 
           type = "EIGENTROPY",
           radius = r,
           output_dir = "data/CC_Result/eigentropy",
           global_shift = T,
           global_shift_type = "AUTO",
           filter_sf = F,
           c_export_fmt = "LAS",
           c_ext = "las",
           silent = T,
           no_timestamp = T))


vert <- readLAS("data/CC_Result/verticality.las")
eigen <- readLAS("data/CC_Result/eigentropy.las")
planar <- readLAS("data/CC_Result/planarity.las")


las <- add_lasattribute(las, vert$`Verticality__(0.05)`, "Verticality (0.05)", "Verticality")
las <- add_lasattribute(las, eigen$`Eigenentropy__(0.05)`, "Eigenentropy (0.05)", "Eigentropy")
las <- add_lasattribute(las, planar$`Planarity__(0.05)`, "Planarity (0.05)", "Planarity")

writeLAS(las, "data/CC_Result/feature_computed_las.las")

file.remove("data/CC_Result/eigentropy.las",
            "data/CC_Result/planarity.las",
            "data/CC_Result/verticality.las")
CC("CloudCompare -O data/CC_Result/feature_computed_las.las -SILENT")

x <- 5
