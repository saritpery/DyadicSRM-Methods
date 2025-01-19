rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: 
# ==============================================================================
# scaled vs. un-scaled columns in pw
# Findings: 
#   * When using APIM, use the scaled dyadic columns.
#   * For individual scores, use non-scaled.
#   * For individual indices, based on ds, use non-scaled.
#   * To compare DIs, you can scale the DIs, but not the ds it bases on. 
# 
# The purpose is to investigate the need for scaling in APIM analysis
# in general and also for the SRM decomposed scores. 
# 
# terminology: 
# scores  = decomposed
# ratings = undecomposed
# dyadic  = scores and ratings with .AB or .BA postfix
# all     = dyadic plus individual decomposed scores (e.g: target.A)
# 
# 1. load pw
# 2. Create three PWs:
#    a. nspw=nonScaledPW - no ratings/scores are scaled.
#    b. allscaledPW - all ratings and scores are scaled
#    c. dyadicScaledPW - only dyadic ratings and scores are scaled
# 2. Perform different analysis to witness the differences. 
#    Results: 
#    a. For individual indices based on dss, such as cilantro and discriminator
#       The variance of dss is compared. 
#       Therefore, the dss can't be scaled, they need to stay different. 
#    b. For DIs there is no substaintial difference between scaled and non-scaled
#       data, see file "31_DIs based on dyadic scores.R".
#    c. For 
# 3. document what I read about scaling

# Scaling: =====================================================================
  # scaling is centering plus division by sd. 
  # Therefore it can change the coefficients, but just like kg. vs. grams, it 
  # won't change the significance.
  # The reason for scaling is to have a standard beta-s as results instead of 
  # the coefficients being in the units of the measure. 

load("main.RImage")
ipak (c("psych", "tidyverse", "nlme", "lme4"))

# load the non-scaled pw into nspw. 
load(imgName(".FullRR.ReadyforAPIM"))

# Begin comparison with the non-scaled pw
nspw <- allscaledPW <- dyadicScaledPW <- nonScaledPW

# Choose columns to scale: 
# Scale all scores and ratings columns or only dyadic scores and ratings
scaleAll <- grep("\\.A|\\.B", names(nspw))
scaleDyadic <- grep("\\.AB|\\.BA", names(nspw)) 

allscaledPW[,scaleAll] <- apply(allscaledPW[,scaleAll], 2, function(a) {as.numeric(scale(a))})
dyadicScaledPW[,scaleDyadic] <- apply(dyadicScaledPW[,scaleDyadic], 2, function(a) {as.numeric(scale(a))})
# apply(allscaledPW[,scaleAll], 2, function(a) {sd(a, na.rm = TRUE)})

# we run the following analysis with a different pw 

for (pw in list(nspw, allscaledPW, dyadicScaledPW)) {
  
}

    # The difference when using the non-scaled scores for raw dyadic creativity.
    # fit <- showAPIM("Creat.raw ~ Hum_rel.AB * Hum_rel.BA + 
    #               Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
    #               aDum + pDum")
    
 