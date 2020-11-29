
## Accuracy Assesment For PCL

# Load Library ------------------------------------------------------------
library(lidR)
library(caret)

# Read Data ---------------------------------------------------------------

ref <- readLAS("data/ref1.las")
model <- readLAS("data/mod1.las")

ref.df <- ref@data
model.df <- model@data
ref.fac <- factor(ref.df$Classification)
model.fac <- factor(model.df$Classification)

confusionMatrix(ref.fac, model.fac)


