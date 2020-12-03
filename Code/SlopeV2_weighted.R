  ### clear lines in Console and all variable in R environment
  rm(list = ls())
  cat("\f")
  
  
  ## R library
  library(lidR)
  library(MASS)
  library(ggplot2)
  
  ### Load road right side 
  df <- readLAS(files = "data/right_side.las")
  Extracted_road_right <- data.frame(x=df@data$X,y=df@data$Y,z=df@data$Z)
  colnames(Extracted_road_right) <- c("X", "Y", "Z")
  rm(df)
  cc <- c(min(Extracted_road_right$X)-0.2, min(Extracted_road_right$X)+0.2)
  
  slp_ind <- Extracted_road_right$X >= cc[1] & Extracted_road_right$X<= cc[2]
  w.right <- rep(0,length(slp_ind))
  w.right[slp_ind] <- 1
  
  ### Load road left side 
  df <- readLAS(files = "data/left_side.las")
  Extracted_road_left <- data.frame(x=df@data$X,y=df@data$Y,z=df@data$Z)
  colnames(Extracted_road_left) <- c("X", "Y", "Z")
  rm(df)
  
  slp_ind <- Extracted_road_left$X <= cc[2] & Extracted_road_left$X >= cc[1]
  w.left <- rep(0,length(slp_ind))
  w.left[slp_ind] <- 1

    
  ### Load nonroad
  df <- readLAS(files = "data/nonroad.las")
  Extracted_nonroad <- data.frame(x=df@data$X,y=df@data$Y,z=df@data$Z)
  colnames(Extracted_nonroad) <- c("X", "Y", "Z")
  rm(df)
  
  # Robust Fit Model for left side of the road
  rlm_fit <- rlm(Z ~ X, Extracted_road_left,weights = w.left, psi = psi.bisquare)  # robust regession model
  slope1 <- rlm_fit$coefficients[2]
  intercept1 <- rlm_fit$coefficients[1]
  P1 <- data.frame(max(Extracted_road_left$X),intercept1+slope1*max(Extracted_road_left$X))
  colnames(P1) <- c("X", "Y")
  P2 <- data.frame(min(Extracted_road_left$X),intercept1+slope1*min(Extracted_road_left$X))  
  colnames(P2) <- c("X", "Y")
  Line1 <- rbind(P1,P2)
  colnames(Line1) <- c("X", "Z")
  rm(rlm_fit,P1,P2)
  
  # Robust Fit Model for right side of the road
  rlm_fit <- rlm(Z ~ X, Extracted_road_right, weights = w.right, psi = psi.bisquare)  # robust regession model
  slope2 <- rlm_fit$coefficients[2]
  intercept2 <- rlm_fit$coefficients[1]
  P1 <- data.frame(max(Extracted_road_right$X),intercept2+slope2*max(Extracted_road_right$X))
  colnames(P1) <- c("X", "Y")
  P2 <- data.frame(min(Extracted_road_right$X),intercept2+slope2*min(Extracted_road_right$X))  
  colnames(P2) <- c("X", "Y")
  Line2 <- rbind(P1,P2)
  colnames(Line2) <- c("X", "Z")
  rm(rlm_fit,P1,P2)
  
  ggplot(NULL, aes(X, Z)) +
    geom_point(data=Extracted_nonroad, col="grey") + 
    geom_point(data=Extracted_road_left, col="green") + 
    geom_line(data=Line1, col="black", size=1.3) +
    ggplot2::annotate("text", 
                      mean(Extracted_road_left$X),  (max(Extracted_road_left$Z)+0.05), 
                      label=sprintf("Slope %% %3.2f",slope1),
                      color = "black") +
    geom_point(data=Extracted_road_right, col="red") + 
    geom_line(data=Line2, col="black", size=1.3) +
    ggplot2::annotate("text", 
                      mean(Extracted_road_right$X),  (max(Extracted_road_right$Z)+0.05), 
                      label=sprintf("Slope %% %3.2f",slope2),
                      color = "black") +
    coord_fixed(ratio = 10)+
    xlab("X")+ylab("z")+theme_classic(base_size = 20)
    #xlim(min(Extracted_nonroad$X),max(Extracted_nonroad$X))+
    #ylim(min(Extracted_nonroad$Z),max(Extracted_nonroad$Z))
  
  
