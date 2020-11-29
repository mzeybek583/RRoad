
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

df <- confusionMatrix(ref.fac, model.fac)

TN <- df[["table"]][1]
FN <- df[["table"]][3]
FP <- df[["table"]][2]
TP <- df[["table"]][4]


Completeness <- TP/(TP+FN)
Correctness <- TP/(TP+FP)
Quality <- TP/(TP+FN+FP)
Accuracy <- (TP+TN)/(TP+TN+FN+FP)

## 2- nonroad 11-road (ClassID)
