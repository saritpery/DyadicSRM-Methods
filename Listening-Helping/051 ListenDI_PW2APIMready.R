rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName("StudyPairWise"))
ipak(c("tidyverse"))

# prepare the data (mainly add dummy codes)
#==========================================
pw$p.id <- as.factor(pw$p.id)
pw$a.id <- as.factor(pw$a.id)
pw$group.id <- as.factor(pw$group.id)

# Create dummy codes using model.matrix.
# The dummy codes are inserted directly into a column in the dataset.
# It keeps the dataset tidy, and enables us to call it in a single name
# during the HLM analysis. 
# However, I found it very difficult to handle, and so, I defined the 
# the dummy codes also seperately as regular data frames, to verify it works right.
aDum <- as.data.frame(model.matrix(~pw$a.id))
pDum <- as.data.frame(model.matrix(~pw$p.id))

#To avoid singularity we drop one dummy for Actor and we drop one per group for
#Partner. 
aDum <- aDum[,-1] 
pDum <- pDum[,-1]

# for each group, we'll chose a single p.id value to drop from the pDum variables.
temp <- pw[!duplicated(pw$group.id), c("group.id", "p.id")]
temp$dummyCode <- paste0("pw$p.id", temp$p.id)
pDum <- pDum[,!(names(pDum) %in% temp$dummyCode)]

# Add the dummy codes into the dataset:
# --------------------------------------
# option 1: 
# Add the dummy codes in a hidden manner, so it will show as a single column:
# r handles it very well, and it keeps the rest of the data nit.
# You can read about option 2 in the JDC project (Humility-creativity) files 
#   in: 09_Prepare dummy codes for APIM.R
pw$aDum <- as.matrix(aDum)
pw$pDum <- as.matrix(pDum)

dummyCols <- c("partnum", "aDum", "pDum", grep(":", names(pw), value = TRUE))

# Scale all relationship effects columns and all rating columns
nonScaledPW <- pw
scaleCols <- grep("\\.AB|\\.BA", names(pw)) # The effects columns
pw <- mutate(pw, across(all_of(scaleCols), ~as.numeric(scale(.x))))

# save the image for APIM analysis

rm(list = c("aDum", "pDum", "temp", "scaleCols"))
save.image(imgName("Ready4APIM2"))
