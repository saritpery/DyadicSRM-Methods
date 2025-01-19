rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.Pairwise"))
ipak("tidyverse")

# In this file:
# ==============================================================================
# 1. Turn id columns to factors
# 2. Add dummy codes
#    The process of using model.matrix is reviewed 
#    Including singularity avoidance.
#    Including two options for adding the dummy codes into the dataset.
# 3. Scale the dyadic scores and dyadic ratings
#    Scores  = decomposed. 
#    Ratings = unDecomposed. 
#    Four files are created: 
#    dyScaledPW     scale dyadic ratings and scores 
#    allScaledPW    all individual and dyadic ratings and scores are scaled
#    nonScaledPW    None of the ratings and scores are scaled
#    pw             dyScaledPW
#    See file 14 that discusses scaling. 
# 4. Save an RImage

# Add dummy codes 
# ==============================================================================
  pw$p.id <- as.factor(pw$p.id)
  pw$a.id <- as.factor(pw$a.id)
  pw$group.id <- as.factor(pw$group.id)
  
  # Create dummy codes using model.matrix.
  # The dummy codes are inserted directly into a single column-name in the dataset.
  # It keeps the dataset tidy, and enables us to call it using a single name
  # during the MLM analysis. 
  # However, I found it very difficult to manipulate, and so, I first defined the 
  # dummy codes separately, to verify it works right.
  # After I finish and make sure it's OK, I'll put it back into the a single column
  # so it will be easy to view afterwards in the analysis.
  
  aDum <- model.matrix(~pw$a.id)
  pDum <- model.matrix(~pw$p.id)
  # To avoid singularity: 
    # we drop one dummy for Actor
    aDum <- aDum[,-1] 
    # and drop a target column for each group.
    pDum <- pDum[, duplicated(pw[!duplicated(pw$p.id),"group.id"])]

  
  # Add the dummy codes into the dataset:
  # --------------------------------------
  # Add the dummy codes in a hidden manner, so it will show as a single column:
  # r handles it very well, and it keeps the rest of the data nit.
  pw$aDum <- aDum
  pw$pDum <- pDum
  

  # Test the number of dummy codes: 
  # dim(aDum);dim(pDum)
  an <- length(unique(pw$a.id))
  pn <- length(unique(pw$p.id))
  k <- length(unique(pw$group.id))
  ncol(aDum) == an - 1
  ncol(pDum) == pn - k
  table(apply(pw$aDum, 2, sum)) # number of dyads for each actor
  table(apply(pw$pDum, 2, sum)) # number of dyads for each partner
  g <- group_by(pw, group.id) |> distinct(a.id) |> summarise(an = n())
  table(g$an)
  
  # The grep in the coming row is for the case where we leave all the dummy-code
  # columns.
  dummyCols <- c("partnum", "aDum", "pDum", grep(":", names(pw), value = TRUE))
  pw <- relocate(pw, any_of(dummyCols), .after = last_col()) 
  
# Scale relevant columns
# ==============================================================================
  dyScaledPW <- allScaledPW <- nonScaledPW <- pw
  
  # Choose columns to scale: 
  # for dyScalePW, Scale only dyadic scores and dyadic ratings columns
  scaleCols <- grep("\\.AB|\\.BA", names(dyScaledPW))
  dyScaledPW[,scaleCols] <- apply(dyScaledPW[,scaleCols], 2, function(a) {as.numeric(scale(a))})

  # for allScaledPW, Scale all scores and ratings columns
  scaleCols <- grep("\\.A|\\.B", names(allScaledPW))
  allScaledPW[,scaleCols] <- apply(allScaledPW[,scaleCols], 2, function(a) {as.numeric(scale(a))})
  
  pw <- nonScaledPW
  
  # save the image for APIM analysis
  rm(list = c("aDum", "pDum"))
  save.image(imgName(".FullRR.ReadyforAPIM"))
  