rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file:   ==============================================================
# 0. Avi convinced me to drop this line of investigation. 
# 1. A discussion with myself about the right way to subset the individual-level 
#    ratings to create an independent dataset. 
# 2. Build a sub-set of independent dyads for each group
#    Build a sub-set of a single dyad from each group (very conservative)

# Discussion how we should analyze individual-level APIM on the data ===========
# The following is documented in the file called: 
#   APIM analysis for RR individuals.docx
# APIM analysis for individual-level raw ratings in a round-robin design
# Goal: ‎
#   I want to analyze the individual level raw ratings to verify what could have 
#   been achieved if we didn’t ‎use SRM at all, and if we didn’t use SRM with dyadic APIM. ‎
# Dilemma:‎
#   There are several ways to address it: ‎
# ‎ 1.‎	Perform the same APIM dummy-code approach: ‎
#       This way we keep the exact same pattern, just w/o decomposition. ‎
#       Maybe we should reduce the grand mean from each score to avoid 
#       group-dependency.‎(This option is performed in file 23)
# ‎ 2.‎	Pick independent dyads from each group:‎
#       Here we should make sure there is no group effect (ICC?)‎
#       Analyze on using APIM. ‎
# ‎ 3.‎	Very conservative: choose a single dyad from each group:‎
#       The most easy and straightforward APIM, but with less data. ‎

load("main.RImage")
load(imgName(".FullRR.Pairwise"))   #raw ratings are not scaled, no dummy code
# load(imgName(".FullRR.ReadyforAPIM")) #raw ratings are scaled

ipak (c("tidyverse","nlme"))

# Prepare subset of independent dyads ==========================================
# For each group choose a random set of dyads. 
# For odd-sized groups, we first randomly pick a participant to drop 
gs <- select(x, group.id, gSize) |> distinct()
d <- consd          <- x[0,] 
indpw <- consIndpw  <- pw[0,]
for (i in 1:nrow(gs)) {
  s <- gs$gSize[i]
  consFound <- FALSE
  ds <- sample(1:s, s, replace=FALSE)
  if ((s %% 2) != 0) ds <- ds[-1] # drop the first item for odd-sized groups
  if (length(ds)==0) next
  for (j in 1:(length(ds)/2)) {
    a <- ds[2*(j-1) + 1];p <- ds[2*(j-1) + 2]; g <- gs$group.id[i]
    dd <- x[x$group.id == g,]
    dd <- dd[(dd$Actor == a & dd$Partner == p) | 
                (dd$Actor == p & dd$Partner == a), ]
    if (nrow(dd) == 2) {
      d <- rbind(d, dd)
      dn <- as.character(pw$dyad)[pw$a.id == dd$a.id[1] & pw$p.id == dd$p.id[1]][1]
      indpw <- rbind(indpw, pw[pw$dyad == dn,])
      if(!consFound) { # add the first 'good' dyad to the conservative data
        consd <- rbind(consd, dd)
        consIndpw <- rbind(consIndpw, pw[pw$dyad == dn,])
        consFound <- TRUE
      }
    }     
  }
}
indpw <- select(indpw, all_of(idCols), 
                all_of(paste0(rep(varnames, each = 2), c("_raw.AB", "_raw.BA"))), 
                everything())

# clean the environment
rm(list = c("dd", "gs", "a", "consFound", "ds", "g", "i", "j", "p", "s", "dn"))
save.image(imgName("indepndentSubset"))
  