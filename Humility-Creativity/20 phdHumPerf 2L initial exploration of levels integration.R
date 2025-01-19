rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak (c("psych", "tidyverse", "nlme", "lme4"))

# In this file: ================================================================
# MUCH OF THIS FILE'S CODE APPEARS ALSO IN FILE 
#     "29 FIGURE OUT TARGET PRODUCT AS PREDICTOR.R"
# - Test basic regressions with individual and dyadic level analyses
# - Addition of basic DIs (Dyadic Indices) for power couples
# - A note about scaled vs. un-scaled columns
# - Save pw with the added DIs for further analysis. 
# - Cilantro effects as basis for DIs

pw <- nonScaledPW # Choose the right scaling option (only dyadic ratings and scores are scaled)
load(imgName("cilantro"))

# Summary of best working and informative models ===============================
  # The models that worked best (below are all the trial-err process)    
  fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA +
                  aDum + pDum", mlMethod = "ML")
    # Results: 0.16, 0.30 all significant.
  fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
    # Results: 0.21, 0.35, 0.24 all significant.
  fgm1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_targetGM.A:Hum_targetGM.B + aDum + pDum", mlMethod = "ML")
    # Results: with GM, targets aren't sign. 0.17, 0.32, 0.06.
  fit2 <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                    Hum_perc.A:Hum_target.B + 
                    aDum + pDum", mlMethod = "ML")
    # Results: for raw all is weak. 0.08, 0.14, 0.06, all significant. This model
    #          is better than w/o the perceiver-target (loglik is higher)
    
  anova(fit0,fit1)
  # results: fit1 offers higher loglik, significantly better :)


# Combined Individual and Dyadic levels analysis ===============================
   
  # Playing with individual and dyad level models:
    # note that all the columns are scaled, but still each column represents the
    # right ratings. This is why the raw columns hold such small numbers...
    
    # A note about scaling: ====================================================
    # scaling is centering plus division by sd. 
    # Therefore it can change the coefficients, but just like kg. vs. grams, it 
    # won't change the significance.
    # The reason for scaling is to have a standard beta-s as results instead of 
    # the coefficients being in the units of the measure. 
    
    # Dyadic level scores with interaction:
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  aDum + pDum", mlMethod = "REML")
    fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA +
                  aDum + pDum", mlMethod = "REML")
    
    # Dyadic level scores with interactions between individual level scores:
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
      # result: the perceivers' interaction doesn't contribute to the model.
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
      # result: the relationship scores' interaction isn't significant
    fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + 
                  Creat_target.A:Creat_target.B + aDum + pDum")
    
    # compare the models w/o individual level elements and with targets
    fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  aDum + pDum", mlMethod = "ML")
    fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
    anova(fit0, fit1) 
      # results: fit1 offers higher loglik, significantly better :)
    gls::anova(fit0);
    anova.gls(fit0)
    
    # Add humility power couple index
    # The power couple index is the interaction with a change of sign if 
    # both dyad members have negative target scores/effects.
    pw$HumPowerCouple <- pw$Hum_target.A * pw$Hum_target.B
    pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] <- 
      pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] * (-1)
    
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    
    # Add cross perceiver-target effects
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  Hum_perc.B:Hum_target.A + Hum_perc.A:Hum_target.B +
                  aDum + pDum")
      # results: the perceivers, as well as perc.B:target.A are very far from significance.
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.A:Hum_target.B +
                  Hum_perc.A:Hum_target.B +
                  aDum + pDum")
      # results: the _rel.AB:_rel.BA & target.B:perc.A not significant. back to 
      #          previous out put.
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B +
                  aDum + pDum")
    
    # check same model with ML instead of REML, better significance
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
    
    # Compare the predictions of dyadic scores to those of raw dyadic ratings:
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
      # results: the perceivers are far from significance (p = 0.98)
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
      # results: the rel-cor isn't sig. the targets(!) are just close (p=0.05). 
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
      # results: the interaction is not significant (though close p = 0.07).
      #          when used REML, it was worse (p = 0.11)
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "REML")
    
    # explore raw dyadic creativity with safety:
    fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Safety_rel.BA * Safety_rel.AB +
                  Hum_target.A:Hum_target.B + 
                  Creat_target.A:Creat_target.B + aDum + pDum")
      # results: rel-cor isn't sig (0.96), lets go for more logical exploration.
    fit <- showAPIM("Creat_raw.BA ~ Safety_rel.BA + Safety_rel.AB +
                  Hum_rel.AB + Hum_rel.BA +
                  Hum_target.B:Hum_perc.A +
                  Safety_target.B:Safety_perc.A + 
                  Creat_target.A:Creat_perc.B + aDum + pDum")
    fit <- showAPIM("Creat_raw.BA ~ Safety_rel.BA + Safety_rel.AB +
                  Hum_rel.AB + Hum_rel.BA +
                  Hum_target.B:Hum_perc.A +
                  Safety_target.B:Safety_perc.A + 
                  Creat_target.A:Creat_perc.B + aDum + pDum")
    
    fit <- showAPIM("Creat_raw.BA ~ Safety_rel.BA * Safety_rel.AB +
                  Safety_target.A:Safety_target.B + 
                  Creat_target.A:Creat_perc.B + aDum + pDum")
    
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    fit <- showAPIM("Creat_rel.BA ~ Safety_rel.AB * Safety_rel.BA + 
                  HumPowerCouple + aDum + pDum")
    
# Cilantro effects =============================================================
    # Add the cilantro effects into the pw
    names(cilantro) <- gsub("cilantro", "cilantro.A", names(cilantro))
    pw <- left_join(pw, select(cilantro, a.id, ends_with("cilantro.A")), by = "a.id")
    names(cilantro) <- gsub("cilantro.A", "cilantro.B", names(cilantro))
    pw <- left_join(pw, select(cilantro, a.id, ends_with("cilantro.B")), 
                    by = c("p.id" = "a.id"))
    pw$Hum_cilantro.meanDI <- rowMeans(select(pw, Hum_cilantro.A, Hum_cilantro.B))
    pw$Safety_cilantro.meanDI <- rowMeans(select(pw, Safety_cilantro.A, Safety_cilantro.B))
    
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_cilantro.A:Hum_cilantro.B + aDum + pDum")
    # results: nothing... 
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_cilantro.meanDI + aDum + pDum")
    # results: nothing... 
    cor(pw$Creat_target.A, pw$Hum_cilantro.A) #-0.003
    cor(pw$Creat_target.A, pw$Creat_cilantro.A) #-0.036
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Safety_cilantro.A:Safety_cilantro.B + aDum + pDum")
    # results: nothing... 
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Safety_cilantro.meanDI + aDum + pDum")
    # results: nothing... 
    fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Safety_cilantro.A:Hum_rel.BA + aDum + pDum")
    # results: nothing... 
    fit <- showAPIM("Hum_rel.BA ~ Safety_rel.AB + Safety_rel.BA + 
                  Safety_cilantro.A:Safety_cilantro.B + aDum + pDum")
    # results: a slightly better nothing... 
    fit <- showAPIM("Hum_rel.BA ~ Safety_rel.AB + Safety_rel.BA + 
                  Safety_cilantro.A:Safety_cilantro.B + aDum + pDum")
    # results: a slightly better nothing... 
    
    
    # The difference when using the non-scaled scores for raw dyadic creativity.
    # fit <- showAPIM("Creat.raw ~ Hum_rel.AB * Hum_rel.BA + 
    #               Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
    #               aDum + pDum")
    
    
# Demographic analysis =======================================================
  # The demographic analysis includes
  # 1. mediation at individual level with perceiver/target scores
  # 2. mediation of dyadic indices type 4
  if(demogAnalysis){
    #          Gender = 1 Female; 
    #          Gender = 2 Male
    
    
    # add individual demog for mediation purposes
    demog <- c("Gender", "Mother_Ton", "Age", "Tenure")
    pw <- left_join(pw, select(x, a.id, any_of(demog)), by = "a.id")
    names(pw)[which(names(pw) %in% demog)] <- paste0(demog, ".A")
    pw <- left_join(pw, select(x, a.id, any_of(demog)), by = c("p.id" = "a.id"))
    names(pw)[which(names(pw) %in% demog)] <- paste0(demog, ".B")
    
    # gender on apeffects:
    m <- select(pw, a.id, any_of(grep("\\.A$", names(pw), value = TRUE)))
    m <- unique(m)
    names(m) <- gsub(".A", "", names(m))
    for (i in 2:(ncol(m)-length(demog))) {
      print(paste(names(m)[i], " by Gender " ))
      print(t.test(m[,i] ~ m[ ,"Gender"]))
    }
    # Conclusion:
    # No significant differences between gender on Actor/partner effects
    # Nothing is even close to significance.
    
    # Test for differences in the relationship indices of 
    # mixed-gender/females/males dyads
    mpw <- unique(select(pw, -any_of(dummyCols)))
    
    mpw$dyadGender <- ifelse(mpw$Gender.A == mpw$Gender.B & mpw$Gender.A == 1, "Females",
                             ifelse(mpw$Gender.A == mpw$Gender.B & mpw$Gender.A == 2, "Males", "MIX"))
    mpw$dyadMixGender <- ifelse(mpw$Gender.A == mpw$Gender.B, 
                                "SameGender",
                                "MixGender")
    mpw$dyadHum <- mpw$Hum_rel.AB + mpw$Hum_rel.BA
    
    aov(mpw$dyadHum ~ mpw$dyadGender)
    fit <- aov(dyadHum ~ dyadGender, data = mpw)
    summary(fit)
    model.tables(fit)
    fit <- aov(dyadHum ~ dyadMixGender, data = mpw)
    model.tables(fit)
  }

  
# RQ1
# ============
    if (hideCode) {
      pw$HumPowerCouple <- pw$Hum_target.A + pw$Hum_target.B
      pw$HumPercivPowerCouple <- pw$Hum_perc.A + pw$Hum_perc.B
      pw$CreatPowerCouple <- pw$Creat_target.A + pw$Creat_target.B
      var(pw$HumPowerCouple); var(pw$CreatPowerCouple)
      var(pw$HumPercivPowerCouple)
      sd(pw$HumPowerCouple); sd(pw$CreatPowerCouple)
    }
  # Begin APIM analysis:
  # ======================

    if (hideCode) {
      fit <- gls(Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      round(data.frame(summary(fit)$tTable[2:3,]), 2)
      
      fit <- gls(Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                   Hum_target.A + Hum_target.A:Hum_target.B + 
                   Hum_perc.A + Hum_perc.A:Hum_perc.B, 
                 na.action = na.omit, method = "REML", 
                 verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      round(data.frame(summary(fit)$tTable), 2)
      # does not converge
      fit <- gls(Creat_rel.BA ~ HumPowerCouple + Hum_rel.AB + Hum_rel.BA + 
                   aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      fit <- gls(Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      tab_model(fit)
      
      round(data.frame(summary(fit)$tTable[2:7,]), 2)
      fit <- gls(Creat_rel.BA ~ Safety_rel.AB + Safety_rel.BA + Hum_rel.AB + Hum_rel.BA + aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      round(data.frame(summary(fit)$tTable[2:5,]), 2)
      
      fit <- gls(Creat_rel.BA ~ Safety_rel.AB + Safety_rel.BA + aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      round(data.frame(summary(fit)$tTable[2:5,]), 2)
    }
  
#RQ3
  fitHS <- gls(Hum_rel.BA  ~ Safety_rel.AB + Safety_rel.BA + 
                aDum + pDum,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|dyad),
             data = cbind(pw))
  RQ3JDC.1 <- round(data.frame(summary(fitHS)$tTable[2:3,]), 2)
  names(RQ3JDC.1)[1] <- "Hum.BA"

  if (hideCode) {

    # Doesn't converge
    fit <- gls(Hum_rel.BA  ~ Safety_rel.AB + Safety_rel.BA + 
                 Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    
    
    # Doesn't converge whenever I insert the interaction with 
    # the individual perceiver score
    fit <- gls(Hum_rel.BA  ~ Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    fit <- gls(Hum_rel.BA  ~ Safety_rel.AB +  
                 Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    fit <- gls(Hum_raw.BA  ~ 
                 Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    fit <- gls(Hum_rel.BA  ~ Safety_rel.AB + Safety_rel.BA + 
                 Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
  }  
  
  RQ3JDC.2 <- lm(Hum_rel.BA  ~ Safety_rel.BA*Safety_perc.B , data = pw)
  save(list = c("RQ3JDC.2", "RQ3JDC.1"), file = "JDC RQ3")
  
  
  if(hideCode){
    # doesn't converge
    fit <- gls(Hum_raw.BA  ~ Hum_rel.AB + Hum_rel.BA*Hum_perc.B + 
                 aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    
    fit <- gls(Hum_raw.BA  ~ Safety_rel.AB + Safety_rel.BA + 
                 Safety_rel.BA*Safety_perc.B   + aDum + pDum,
               na.action = na.omit, method = "REML", verbose = TRUE,
               correlation = corCompSymm (form = ~1|dyad),
               data = cbind(pw))
    }
  

  # view the prediction of the raw scores ======================================
  # in the following analysis, we can see that the raw ratings of B on A
  # are completely and solely dependent on the dyadic scores of B on A. 
  rawFromDyadic <- data.frame(matrix(ncol = 5, nrow = 0))
  for (i in varnames) {
    f <- paste0(i, "_raw.BA ~ ", i, "_rel.AB + ", i, "_rel.BA + aDum + pDum")
    t <- try(fit <- gls(as.formula(f),
                        na.action = na.omit, method = "REML", verbose = TRUE,
                        correlation = corCompSymm (form = ~1|dyad),
                        data = cbind(pw)))
    if (!"try-error" %in% class(t)) {
      res <- round(data.frame(summary(fit)$tTable[2:3,]), 2)
      res$DV <- c(paste0(i, "_raw.BA"), "")
      rawFromDyadic <- rbind(rawFromDyadic, res)
    }}
  rawFromDyadic$IV <- rownames(rawFromDyadic)
  rownames(rawFromDyadic) <- NULL
  rawFromDyadic <- select(rawFromDyadic, DV, IV, everything())
  rawFromDyadic
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
  