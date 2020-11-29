

# Compute Features for Random Forest Train or Prediction


# Installing libs ---------------------------------------------------------
## Installing libs
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

library(RANN)
library(rgl)
library(matlab)
library(lidR)
library(pracma)
library(beepr)


tic()

# Read Data ---------------------------------------------------------------


data1 <- readLAS(files = "train.las")
sprintf("Las verisi okundu", toc())

plot(data1, color= "Classification")
data <- cbind(data1@data$X,data1@data$Y,data1@data$Z)

# Neighboorhood
k <- 50
nearest <- nn2(data = data,k=k+1)
nn <- nearest[["nn.idx"]]
nn <- nn[,-1]
nn.dist <- nearest[["nn.dists"]]
nn.dist <- nn.dist[,-1]
pp <- as.matrix(data)

r_mat <- kronecker(matrix(1,k,1),pp[,1:3])
p_mat <- pp[nn,1:3]

p <-  p_mat - r_mat;
p <- matlab::reshape(p, size(pp,1), k, 3);


C <-  matrix(0,dim(data),6)
C[,1] <- rowSums(p[,,1]*p[,,1])
C[,2] <- rowSums(p[,,1]*p[,,2])
C[,3] <- rowSums(p[,,1]*p[,,3])
C[,4] <- rowSums(p[,,2]*p[,,2])
C[,5] <- rowSums(p[,,2]*p[,,3])
C[,6] <- rowSums(p[,,3]*p[,,3])
C <- C/k


# Compute features --------------------------------------------------------


###### normals and curvature calculation
normals <-  matrix(0,dim(pp),3)
curvature <-  matrix(0,dim(pp))
omnivariance <-  matrix(0,dim(pp))
planarity <-  matrix(0,dim(pp))
linearity <-  matrix(0,dim(pp))
surf_var <-  matrix(0,dim(pp))
anisotropy <-  matrix(0,dim(pp))

for (i in 1:nrow(pp)) {
  #Covariance
  Cmat <- matrix( c(C[i,1], C[i,2], C[i,3],
                    C[i,2], C[i,4], C[i,5],
                    C[i,3], C[i,5], C[i,6]), byrow = TRUE,ncol = 3)  
  
  #Eigen values and vectors
  
  d <- eigen(Cmat)$values
  v <- eigen(Cmat)$vectors
  lambda  <- min(d);
  ind <- which(d==min(d))
  
  #store normals
  normals[i,] = t(v[,ind])
  
  #store curvature
  curvature[i] = lambda / sum(d);
  
  #store omni
  omnivariance[i] = (d[1]*d[2]*d[3])^(1/3)
  #store planarity
  planarity[i] = (d[2]-d[3])/d[1]
  #store linearity
  
  linearity[i] = (d[1]-d[2])/d[1] 
  #store surf_var
  
  surf_var[i] = d[3]/(d[1]+d[2]+d[3])
  #store anisotropy
  
  anisotropy[i] = (d[1]-d[3])/d[1]
}

sprintf("Eigenvalues hesaplandi", toc())

dist_nn <- nn.dist[,1]
range(data1@data$R)
bit8 <- function(x){
  y <- round((x/255)-1)
  return(y)
}
data1@data$R2 <- bit8(data1@data$R)
data1@data$G2 <- bit8(data1@data$G)
data1@data$B2 <- bit8(data1@data$B)

write.csv(cbind(pp,normals,data1@data$R2, data1@data$G2, data1@data$B2, 
                curvature,omnivariance,planarity,linearity,surf_var,anisotropy, data1@data$`Original cloud index`),
          file = "computed_features.txt", row.names = FALSE)
sprintf("Eigenvalues dosyasi export edildi", toc())
