rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file: ================================================================
# Play with the cilantro effect of participants as a predictor for stuff 

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))
ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", 
        "tidyverse", "nlme"))
load(imgName("cilantro"))
# ipak("sjPlot")
varnames <- varnames[1:4]

pw <- nonScaledPW # Choose the right scaling option (no scaling)

# Add the cilantro effect to pw ================================================
cilantro <- select(cilantro,a.id, ends_with("cilantro"), 
                   -starts_with("Shy"), -starts_with("Non"))
names(cilantro) <- gsub("_cilantro", "_cilant.B", names(cilantro))
cilantro$trait_cilant.B <- sqrt(rowMeans(select(cilantro, -a.id), na.rm = TRUE))
pw <- left_join(pw, cilantro, by = c("p.id"="a.id"))
names(cilantro) <- gsub("_cilant.B", "_cilant.A", names(cilantro))
pw <- left_join(pw, cilantro, by = "a.id")

# we see that the cilantro effect is near zero for most participants
cn <- as.data.frame(matrix(nrow = 5, ncol = 14))
names(cn)[1] <- "v"
cn$v <- c(varnames, "trait")
for (i in cn$v) { 
  v <- paste0(i, "_cilant.A")
  cv <- unlist(as.vector(cilantro[,v]))
  a <- describe(cv)
  names(cn)[2:14] <- names(a)
  cn[cn$v == i, 2:14] <- a
  stem(cv)
}
# In the following summary of cn (cilantro normality) we see that the cilantro
# is very skewed such that most participants are very low on cilantro, but there
# are some that are very high on the effect.
# Creativity is where the cilantro is spread more evenly across the numbers, 
# where different people have different ratings of cilantro.
cn
  
f0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum", mlMethod = "ML")
f <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
               Hum_cilant.A*Hum_cilant.B + aDum + pDum", mlMethod = "ML")
# result: doesn't converge
f <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
               trait_cilant.A*trait_cilant.B + aDum + pDum", mlMethod = "ML")
# result: doesn't converge
f <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
               Hum_cilant.A + aDum + pDum", mlMethod = "ML")
# result: doesn't converge
f <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + Hum_rel.BA:Hum_cilant.A  + 
                aDum + pDum", mlMethod = "ML")
# result: converge but doesn't add much
f <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + Hum_rel.BA:Hum_cilant.A  + 
                aDum + pDum", mlMethod = "ML")
# result: converge but doesn't add much

round(cor(select(pw, Creat_rel.BA, ends_with("cilant.A"))), 2)


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
  