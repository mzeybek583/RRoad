

## Roadlane based centerline extraction


# Load libraries ----------------------------------------------------------

library(lidR)

data <- readLAS(files = "../Agisoft_Proje/per2_agi_auto_DENSE.las")

# New grayscale image = ( (0.3 * R) + (0.59 * G) + (0.11 * B) ).
# Agisoft formula: 0.21 R + 0.72 G + 0.07 B.
d <- data@data
#RGB value exportation
d.R <- d$R
d.G <- d$G
d.B <- d$B


# Compute composition -----------------------------------------------------

composite <- 0.3*d.R + 0.59*d.G + 0.11*d.B

las.export <- LAS(d) # create LAS
las.export <- add_lasattribute(las.export, composite, "int", "RGB composite")

plot(las.export, color="int")


# Filtre Points Threshold -------------------------------------------------

threshold <- filter_poi(las.export, int > 53000)

# Write result to LAS file ------------------------------------------------

writeLAS(las.export, "Roadlane.las")
writeLAS(threshold,"Roadlane_threshold.las")
