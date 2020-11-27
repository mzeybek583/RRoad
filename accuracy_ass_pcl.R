
## Accuracy Assesment For PCL

# Load Library ------------------------------------------------------------
library(lidR)
library(caret)

# Read Data ---------------------------------------------------------------

ref <- readLAS("ref.las")
model <- readLAS("model.las")

ref.df <- ref@data
model.df <- model@data
ref.fac <- factor(ref.df$Classification)
model.fac <- factor(model.df$Classification)

confusionMatrix(ref.fac, model.fac)


