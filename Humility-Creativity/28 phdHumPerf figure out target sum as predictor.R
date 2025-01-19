rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Define basic DIs (Dyadic Indices) that represents power couples
# compare different implementation of indices

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
# ipak("sjPlot")

pw <- dyScaledPW # Choose the right scaling option (only dyadic ratings and scores are scaled)

# Basic power-couples indices ==================================================
  pw$Hum_TT.sum <- pw$Hum_target.A + pw$Hum_target.B
  pw$Hum_PP.sum <- pw$Hum_perc.A + pw$Hum_perc.B
  pw$Hum_TT.prod <- pw$Hum_target.A * pw$Hum_target.B
  pw$Hum_TTGM.prod <- pw$Hum_targetGM.A * pw$Hum_targetGM.B
  pw$Hum_TTGM.sum <- pw$Hum_targetGM.A + pw$Hum_targetGM.B
  pw$Hum_TT.delta <- abs(pw$Hum_target.A - pw$Hum_target.B)
  pw$Hum_TTPos.prod <- pw$Hum_targetPOS.A * pw$Hum_targetPOS.B
  pw$Hum_TTPos.sqrProd <- sqrt(pw$Hum_TTPos.prod)
  pw$Hum_TTPos.sum <- pw$Hum_targetPOS.A + pw$Hum_targetPOS.B
  pw$Cre_RR.sum <- pw$Creat_rel.AB + pw$Creat_rel.BA
  pw$Cre_RR.delta <- abs(pw$Creat_rel.AB - pw$Creat_rel.BA)
  # Add humility power couple index:
  # The power couple index is the interaction with a change of sign if 
  # both dyad members have negative target scores/effects.
  pw$HumPowerCouple <- pw$Hum_target.A * pw$Hum_target.B
  pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] <- 
    pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] * (-1)
  
  ic <- c("Cre_RR.sum", "Creat_rel.BA", "Hum_target.A", "Hum_target.B", "Hum_TT.sum", "Hum_TT.delta")
  d <- select(pw, all_of(idCols), starts_with("Creat"), starts_with("Hum_"))
  dd <- select(pw, all_of(idCols), all_of(ic))
  dd[,ic] <- sapply(dd[,ic], function(x) round(x,2))
  plot(dd$Hum_TT.sum, dd$Creat_rel.BA)
  round(cor(dd[,ic]),2)
  
  d <- d[d$group.id == 210,]
  sum(d$Creat_rel.AB[d$a.id == "210:1"])
  sum(d$Creat_rel.BA[d$a.id == "210:1"])
  d[,5:44] <- sapply(d[,5:44], function(x) round(x,2))
  d <- select(d, a.id, p.id, Creat_rel.BA, Hum_TT.sum, Hum_TT.prod, everything() )
  
  stem(pw$HumPowerCouple)
  stem(pw$Hum_TT.prod)
  stem(pw$Hum_TT.sum)
  stem(pw$Hum_TTGM.sum)
  stem(pw$Hum_TTGM.prod)
  stem(pw$Hum_TT.delta)
  stem(pw$Hum_TTPos.prod)
  cor(pw$HumPowerCouple, pw$Hum_TT.prod)
  cor(pw$Hum_TT.sum, pw$Hum_TT.prod)
  plot(pw$HumPowerCouple, pw$Hum_TT.sum)
  cor(pw$HumPowerCouple, pw$Hum_TTGM.prod)
  cor(pw$Hum_TT.sum, pw$Hum_TT.prod)
  plot(pw$HumPowerCouple, pw$Hum_TT.sum)
  cor(select(pw, HumPowerCouple,Hum_TT.prod, Hum_TT.sum, 
             Hum_TTGM.prod, Hum_TT.delta,Creat_rel.BA, Hum_TTPos.prod  ))
  plot(select(pw, Hum_TT.sum, Hum_TTGM.prod ))
  plot(pw$Hum_TT.prod, pw$Hum_TTPos.prod)
  plot(pw$Hum_TT.sum, pw$Hum_TTPos.prod)
  plot(pw$Hum_TT.sum, pw$Hum_TTPos.sqrProd)
  # describe(pw$Hum_TT.sum)
  # describe(pw$Creat_rel.BA)
  stem(pw$Hum_target.A*pw$Hum_target.B)
  kurtosi(pw$HumPowerCouple)
  kurtosi(pw$Hum_TT.prod)
  skew(pw$HumPowerCouple)
  skew(pw$Hum_target.A*pw$Hum_target.B)
  
f0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum", mlMethod = "ML")

fprod1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
  # Results: 0.21, 0.35, 0.24 all significant.  
fprod2 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.prod + aDum + pDum", mlMethod = "ML")
  # Results: same as it should be. 0.21, 0.35, 0.24 all significant.  
fPOSprod <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TTPos.prod + aDum + pDum", mlMethod = "ML")
  # Results: The targets lose significance and power :(
  #          0.19, 0.33, 0.13.
fPosSqrProd <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TTPos.sqrProd + aDum + pDum", mlMethod = "ML")
  # Results: The targets lose significance :(
  #          Perhaps this is because is almost identical to the sum. the plot between
  #          the sum and this one is almost a straight line. 
  #          0.19, 0.33, 0.75.
fPower <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum", mlMethod = "ML")
  # Results: 0.20, 0.34, 0.28.all significant. 

# The following two analyses demonstrate the zero correlation between a rel score
# and the mean/sum index of any variable.
fsum1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")

fsum1 <- showAPIM("Creat_rel.BA ~ Safety_rel.AB + Safety_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")

# The following is wrong, because one cannot use the pw for a di as outcome. 
# It doubles the entries for the same rows, since both dyad's rows are equivalent
# for the analysis.
fsumRR1 <- showAPIM("Cre_RR.sum ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")
fsumRR1 <- showAPIM("Cre_RR.delta ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")

fit1b <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + 
                    Hum_rel.BA * Hum_TT.prod +
                    aDum + pDum", mlMethod = "ML")

    
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    describe(pw$Hum_TargTarg.sum)
    describe(pw$Creat_rel.BA)
    fsum1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum + aDum + pDum", mlMethod = "ML")
    fgm <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TTGM.prod + aDum + pDum", mlMethod = "ML")
    f2 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.sum * Hum_TT.delta + aDum + pDum", mlMethod = "ML")
    f2 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TT.delta + aDum + pDum", mlMethod = "ML")
    f2 <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TTPos.prod + aDum + pDum", mlMethod = "ML")
    f2 <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_TTGM.sum + aDum + pDum", mlMethod = "ML")
    
    
    # Add cross perceiver-target effects
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  Hum_perc.B:Hum_target.A + Hum_perc.A:Hum_target.B +
                  aDum + pDum")
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B +
                  Hum_perc.B:Hum_target.A +
                  aDum + pDum")
    
    # check same model with ML instead of REML, arrive significance
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
    
    # Compare the predictions of dyadic scores to those of raw dyadic ratings:
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Safety_rel.BA * Safety_rel.AB +
                  Hum_target.A:Hum_target.B + 
                  Creat_target.A:Creat_target.B + aDum + pDum")
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    fit <- showAPIM("Creat_rel.BA ~ Safety_rel.AB * Safety_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    
    
    # The difference when using the non-scaled scores for raw dyadic creativity.
    # fit <- showAPIM("Creat.raw ~ Hum_rel.AB * Hum_rel.BA + 
    #               Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
    #               aDum + pDum")
    
  # Prediction of the raw scores ===============================================
  # in the following analysis, we can see that the raw rating of B on A's humility 
  # are completely and solely dependent on the dyadic opposite score. 
  rawFromDyadic <- data.frame(matrix(ncol = 5, nrow = 0))
  for (i in varnames) {
    f <- paste0(i, "_raw.BA ~ ", i, "_rel.AB + ", i, "_rel.BA + aDum + pDum")
    t <- try(fit <- gls(as.formula(f),
                        na.action = na.omit, method = "REML", verbose = TRUE,
                        correlation = corCompSymm (form = ~1|dyad),
                        data = cbind(pw)))
    if (!"try-error" %in% class(t)) {
      res <- round(data.frame(summary(fit)$tTable[2:3,]), 2)
      res$DV <- paste0(i, "_raw.BA")
      rawFromDyadic <- rbind(rawFromDyadic, res)
    }}
  save(list = c(rawFromDyadic), file = "JDC raw from Dyadic")
  
  if (hideCode) {
    
    fit <- gls(Hum.BA  ~ Creat.AB + Creat.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    
    fit <- gls(Hum.BA  ~ Creat.AB + Creat.BA + Safety.AB + Safety.BA +aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Creat.AB  ~ Safety.AB + Safety.BA +aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    
    
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    # #
    fit <- gls(Contr.BA ~ Safety.AB + Safety.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Contr.BA ~ Safety.AB + Safety.BA + Hum.AB + Hum.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Safety.AB ~ Hum.AB + Hum.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Safety.AB ~ Contr.AB + Contr.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Safety.AB ~ Hum.AB + Hum.BA + Creat.AB + Creat.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Hum.BA ~ Safety.AB + Safety.BA + Creat.AB + Creat.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Safety.AB ~ Hum.AB + Hum.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Contr.BA ~ Safety.AB + Safety.BA + Hum.AB + Hum.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
    fit <- gls(Contr.BA ~ Hum.AB + Hum.BA + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompkSymm (form = ~1|dyad),
               data = cbind(pw))
    round(data.frame(summary(fit)$tTable[2:7,]), 2)
  }
  rm(list = setdiff(ls(), c("apEffects", "apRelEffects", "longInput", "pw", "x", 
                            "dummyCols", "fullRRData", "idCols", "varnames", "ipak")))
  if(saveEnv) save.image("JDCreadyForDyadicIndx")
  