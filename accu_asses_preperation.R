    
    ## Accuracy assessment data preperation
    
    
    # Load Libraries ----------------------------------------------------------
    
    library(lidR)
    
    
    # Read data ---------------------------------------------------------------
    
    r.road <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/Manual_road.las")
    r.road@data$Classification <- 11
    r.nroad <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/Manual_nonroad.las")
    r.nroad@data$Classification <-2
    
    ref.las <- rbind(r.road,r.nroad)
    writeLAS(las = ref.las, "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/ref3.las")
    rm(ref.las, r.nroad, r.road)
    
    model.road <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/Extracted_road.las")
    model.road@data$Classification <- 11
    model.nroad <- readLAS(files = "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/Extracted_nonroad.las")
    model.nroad@data$Classification <-2
    
    mod.las <- rbind(model.road,model.nroad)
    writeLAS(las = mod.las, "/home/mzeybek/Desktop/rlm_road/accuracy_ass/test3/mod3.las")
    
    rm(mod.las, model.nroad, model.road)
    
