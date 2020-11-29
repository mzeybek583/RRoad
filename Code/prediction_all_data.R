
## Prediction of MLS

##### Predict New Data ######
superModel <- readRDS("./finalModel.rds")
print(superModel)

# New Data Import ---------------------------------------------------------

veri2 <-  read.csv(file = "computed_features_all_data.txt")
#veri2 <- cbind(veri2,las@data$Z)

colnames(veri2) <- c("X","Y","Z", "nx","ny", "nz", "R","G","B","Curvature",
                     "Omnivariance","Planarity", "Linearity", "Surface_Variance","Anisotropy")
veri2$Class <- 0

finalPredictions <- predict(superModel, veri2)

export<-veri2
export$Class <- finalPredictions
export$Class <- as.numeric(as.character(export$Class))
#export$ID <- NULL
library(lidR)
export.las <- data.frame(X=export$X, Y= export$Y, Z=export$Z, Classification=as.integer(export$Class))
las.export <- LAS(export.las, header= list(), check = TRUE)

writeLAS(las.export, "classified_all_data.las")
#write.csv(export, "./classified_all_data.csv")

sprintf("Program ended", toc())
