rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".Pairwise"))
ipak("tidyverse")

# In this file:
# ==============================================================================
# 1. Turn id columns to factors
# 2. Add dummy codes
#    The process of using model.matrix is reviewed 
#    Including singularity avoidance.
#    Including two options for adding the dummy codes into the dataset.
# 4. Save an RImage

# Add dummy codes 
# ==============================================================================
  pw$p.id <- as.factor(pw$p.id)
  pw$t.id <- as.factor(pw$t.id)
  pw$group.id <- as.factor(pw$group.id)
  
  # Create dummy codes using model.matrix.
  # The dummy codes are inserted directly into a single column-name in the dataset.
  # It keeps the dataset tidy, and enables us to call it using a single name
  # during the MLM analysis. 
  # However, I found it very difficult to manipulate, and so, I first defined the 
  # dummy codes separately, to verify it works correctly.
  # After I finish and make sure it's OK, I'll put it back into the a single column
  # so it will be easy to view afterwards in the analysis.
  
  pDum <- model.matrix(~pw$p.id)
  tDum <- model.matrix(~pw$t.id)
  # To avoid singularity: 
    # we drop one dummy for Actor
    pDum <- pDum[,-1] 
    # and drop a target column for each group.
    tDum <- tDum[, duplicated(pw[!duplicated(pw$t.id),"group.id"])]

  
  # Add the dummy codes into the dataset:
  # --------------------------------------
  # Add the dummy codes in a hidden manner, so it will show as a single column:
  # r handles it very well, and it keeps the rest of the data nit.
  pw$pDum <- pDum
  pw$tDum <- tDum
  
  # The grep in the coming row is for the case where we leave all the dummy-code
  # columns.
  dummyCols <- c("partnum", "pDum", "tDum", grep(":", names(pw), value = TRUE))
  pw <- relocate(pw, any_of(dummyCols), .after = last_col()) 
  
  # save the image for APIM analysis
  rm(list = c("tDum", "pDum"))
  save.image(imgName(".ReadyforAPIM"))
  