

# Machine Learning Model

library(caret)
library(ellipse)
library(e1071)
library(kernlab)
library(randomForest)
library(rgl)
library(spatstat)
library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
veri2 <- read.csv(file = "computed_features.txt")
#veri2 <- cbind(pp,normals,curvature,omnivariance,planarity,linearity,surf_var,anisotropy,dist_nn)
#veri2 <- cbind(data1@data$X,data1@data$Y,data1@data$Z,normals[,1],normals[,2],normals[,3],
 #              data1@data$R2,data1@data$G2,data1@data$B2,curvature, omnivariance, planarity,
  #             linearity, surf_var, anisotropy, las@data$Z, data1@data$`Original cloud index`)
#veri2 <- cbind(veri2, las@data$Z, las@data$Classification)
colnames(veri2) <- c("X","Y","Z", "nx","ny", "nz","R","G","B", "Curvature",
                     "Omnivariance","Planarity", "Linearity", "Surface_Variance","Anisotropy","Class")

veri2 <- as.data.frame(veri2)


#SAmple data
veri2<- veri2[sample(nrow(veri2), 20000), ]

#veri2 <- as.data.frame(veri2)
#### Validation ###
validation_index <- createDataPartition(veri2$Class, p=0.7,list = FALSE)
validation <- veri2[-validation_index,]
validation$Class <- factor(validation$Class)
dataset <- veri2[validation_index,]

####### Dimensions of the dataset #####
dataset$Class <- factor(dataset$Class)
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$Class)

####### Building Models #######

control <- trainControl(method = "repeatedcv",number = 10,repeats=3,search='grid', allowParallel = TRUE)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = (1:13)) 
set.seed(7)

fit.rf <- train(Class~ Z+nx+ny+nz+R+G+B+Curvature+Omnivariance+Planarity+
                  Linearity+Surface_Variance+Anisotropy,data = na.omit(dataset), method = "rf",
                metric = metric, trControl = control,tuneGrid = tunegrid, preProcess = c("center","scale"), 
                #    ntree = 1000,
                nodesize=10)
#fit.rf <- train(Class~ nz+Omnivariance + Planarity, data = na.omit(dataset), method = "rf",
#                metric = metric, trControl = control)
stopCluster(cluster)
registerDoSEQ()
plot(fit.rf$finalModel)
summary(fit.rf)
print(fit.rf)
sprintf("Train islemi bitti", toc())

##### Make Predictions #######
predictions <- predict(fit.rf, validation)
result <- confusionMatrix(predictions, validation$Class, mode = "everything")
result
saveRDS(fit.rf, "./finalModel.rds")

# Accuracy Assessments ----------------------------------------------------


#### Variable importance in RBF

library(ggplot2)

#superModel <- readRDS("finalModel.rds")
print(fit.rf)

var <- varImp(fit.rf, scale = TRUE)
print(var)
aa <- var[["importance"]]
aa
x <- rownames(aa)
y <- aa$Overall
#par(mar=c(5,4,4,2)+1)
par(mar = c(0, 0, 0, 0))
p<-ggplot(data=aa, aes(x=x, y=y)) +
  geom_bar(stat="identity")+
  theme(text = element_text(size = 28))+
  xlab("Features")+ ylab("Variable Importance (%)")
p

plot(var, top = 13, xlab =" Variable Importance (%)", ylab= "Feature")


library(gmodels)
pred_train <-predict(fit.rf, dataset[,-16])
pred_valid <-predict(fit.rf, validation[,-16])

con_mat <- confusionMatrix(data = pred_train, reference = dataset$Class)
con_mat

con_mat_pred <- confusionMatrix(data = pred_valid, reference = validation$Class)
con_mat_pred

sprintf("Machine Learning Process is done", toc())
