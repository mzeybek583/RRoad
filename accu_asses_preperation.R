  
  ## Accuracy assessment data preperation
  
  
  # Load Libraries ----------------------------------------------------------
  
  library(lidR)
  
  
  # Read data ---------------------------------------------------------------
  
  r.road <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/Manual_road.las")
  r.road@data$Classification <- 11
  r.nroad <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/Manual_nonroad.las")
  r.nroad@data$Classification <-2
  
  ref.las <- rbind(r.road,r.nroad)
  writeLAS(las = ref.las, "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/ref1.las")
  
  model.road <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/Extracted_Road.las")
  model.road@data$Classification <- 11
  model.nroad <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/Extracted_nonroad.las")
  model.nroad@data$Classification <-2
  
  mod.las <- rbind(model.road,model.nroad)
  writeLAS(las = mod.las, "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test1/mod1.las")
  
  
