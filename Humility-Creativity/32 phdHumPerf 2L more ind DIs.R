rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Investigate different alternatives for product dyadic indices. 
# The product indices are all about distinguishing great/worst dyads 
# A simple product will do if the scores are positive, but things get messy 
# when some scores are negative. 
# Next I also add them as sole predictors of DIs

# MUCH OF THIS FILE'S CODE APPEARS ALSO IN FILE 
#     "20 2L initial exploration of levels integration.R"

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
# ipak("sjPlot")

# Basic power-couples indices ==================================================
  pw$Hum_TT.sum <- pw$Hum_target.A + pw$Hum_target.B
  pw$Hum_TT.prod <- pw$Hum_target.A * pw$Hum_target.B
  pw$Hum_TTGM.prod <- pw$Hum_targetGM.A * pw$Hum_targetGM.B
  pw$Hum_TTPos.prod <- sqrt(pw$Hum_targetPOS.A * pw$Hum_targetPOS.B)
  # flipprod = reverse the sign of the product of dyads with both negative scores
  pw$Hum_TT.flipprod <- pw$Hum_target.A * pw$Hum_target.B
  pw$Hum_TT.flipprod[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] <- 
    pw$Hum_TT.flipprod[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] * (-1)
  # absprod = dyads with the same sign are the product with the correct sign. 
  #           dyads with mixed signs are zero.
  pw$Hum_TT.absprod <- ((abs(pw$Hum_target.A) * pw$Hum_target.B + pw$Hum_target.A * abs(pw$Hum_target.B))/2)
  # powerprod = the power couple index holds how strong the dyad is on Hum. 
  #            if both AB are positive - the result is the product (positive)
  #            if both AB are negative - the result is the negative product
  #            if AB have different signs - the result have the sign of the bigger abs number
  pw$Hum_TT.powerprod <- pw$Hum_TT.flipprod
  items2chngSign <- pw$Hum_target.A < 0 & pw$Hum_target.B > 0 & 
    pw$Hum_target.B > abs(pw$Hum_target.A)
  items2chngSign <- items2chngSign | 
    pw$Hum_target.B < 0 & pw$Hum_target.A > 0 & pw$Hum_target.A > abs(pw$Hum_target.B)
  pw$Hum_TT.powerprod[items2chngSign] <- abs(pw$Hum_TT.powerprod[items2chngSign])
  ttDIs <- grep("Hum_TT", names(pw), value = TRUE)
  
  nonScaledPW$Hum_TT.prod <- nonScaledPW$Hum_target.A * nonScaledPW$Hum_target.B
  nonScaledPW$Hum_TTPos.prod <- nonScaledPW$Hum_targetPOS.A * nonScaledPW$Hum_targetPOS.B
  cor(nonScaledPW$Hum_TT.prod, nonScaledPW$Hum_TTPos.prod)
  cor(scale(nonScaledPW[,c("Hum_TT.prod", "Hum_TTPos.prod")]))
  # pw$Hum_TT.delta <- abs(pw$Hum_target.A - pw$Hum_target.B)
  # pw$Cre_RR.delta <- abs(pw$Creat_rel.AB - pw$Creat_rel.BA)
  
# Investigate all DIs: stem, normality, correlations, and more =================
  investigate <- FALSE
  if (investigate){
    # # To be able to compare effects we need to scale the indices columns
    ic <- grep("prod$|delta$|sum$", names(pw), value = TRUE)
    # pw[, ic] <- scale(pw[,ic])
    
    # #try to look at the VIF (Variance Inflation Factor) that indicates the 
    # #multicolinearity between the IVs
    # tempModel <- paste("Creat_rel.BA ~ ", paste(ic, collapse = " + "))
    # f <- lm(as.formula(tempModel), data = pw)
    # library(car)
    # vif(f)
    for (v in ic) {
      print(v)
      a <- pw[,v]
      stem(a)
    }
    as.data.frame(rbind(kurtosi(pw[,ic]), skew(pw[,ic])))
    
    cor(pw[,ic])
    
    cor(pw$HumPowerCouple, pw$Hum_TargTarg.prod)
    cor(pw$Hum_TargTarg.sum, pw$Hum_TargTarg.prod)
    plot(pw$HumPowerCouple, pw$Hum_TargTarg.sum)
    cor(pw$HumPowerCouple, pw$Hum_TTGM.prod)
    cor(pw$Hum_TargTarg.sum, pw$Hum_TargTarg.prod)
    plot(pw$HumPowerCouple, pw$Hum_TargTarg.sum)
    cor(select(pw, HumPowerCouple,Hum_TT.prod, Hum_TT.sum, 
               Hum_TTGM.prod, Hum_TT.delta,Creat_rel.BA, Hum_TTPos.prod  ))
    plot(select(pw, Hum_TargTarg.sum, Hum_TTGM.prod ))
    # describe(pw$Hum_TT.sum)
    # describe(pw$Creat_rel.BA)
    stem(pw$Hum_target.A*pw$Hum_target.B)
    kurtosi(pw$Hum_TT.powerprod)
    kurtosi(pw$Hum_TT.prod)
    skew(pw$HumPowerCouple)
    skew(pw$Hum_target.A*pw$Hum_target.B)
  } # End of investigate
  
# APIM investigation formulas ==================================================

f0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum", mlMethod = "ML")

# The sum's correlation is zero as expected. 
fsum1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")

  # fsumRR1 <- showAPIM("Cre_RR.sum ~ Hum_rel.AB + Hum_rel.BA + 
  #                   Hum_TT.sum + aDum + pDum", mlMethod = "ML")
  # fsumRR1 <- showAPIM("Cre_RR.delta ~ Hum_rel.AB + Hum_rel.BA + 
  #                   Hum_TT.sum + aDum + pDum", mlMethod = "ML")
  # 
  # fit1b <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + 
  #                     Hum_rel.BA * Hum_TT.prod +
  #                     aDum + pDum", mlMethod = "ML")

# Analyze all DIs of Hum's TT as predictors of unique dyadic creativity
for (i in ttDIs) {
  f <- paste("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + ", i, " + aDum + pDum", collapse = "")
  print (i)
  fit <- showAPIM(f, mlMethod = "ML")
}    

# Analyze all DIs of Hum's TT as predictors of RAW dyadic creativity 
# Results: Only the sum is significant or interesting
for (i in ttDIs) {
  print (i)
  f <- paste("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + ", i, " + aDum + pDum", collapse = "")
  fit <- showAPIM(f)
  f <- paste("Creat_raw.BA ~ Hum_raw.AB + Hum_raw.BA + ", i, " + aDum + pDum", collapse = "")
  fit <- showAPIM(f)
}    

# DIs as predictors and outcome ================================================

# once our outcome variable is per dyad (and not per directional dyad), we need
# to have a single row per dyad as our dataset and not two rows per dyad. 
dlong <- group_by(pw, dyad) |> filter(row_number()==1) |> 
  select(-aDum, -pDum) |> ungroup()
dlong$Creat_rel.mean <- rowMeans(select(dlong, Creat_rel.AB, Creat_rel.BA), na.rm = TRUE)
dlong$Creat_TT.sum <- dlong$Creat_target.A + dlong$Creat_target.B

for (i in ttDIs) {
  f <- paste("Creat_rel.mean ~ ", i, collapse = "")
  fit <- lm(as.formula(f), data = dlong)
  ci <- round(confint(fit)[2,],2)
  ci <- paste0("  CI [", ci[1], "," ,ci[2], "]")
  print(paste(i, " coef: ", round(fit$coefficients[2], 2), ci, collapse = ""))
  print( paste("Adjusted R2: ", round(summary(fit)$adj.r.squared, 2)))
}    


(fit <- lm(Creat_TT.sum ~ Hum_TT.sum, data = dlong))
confint(fit)
print( paste("Adjusted R2: ", round(summary(fit)$adj.r.squared, 2)))

       