rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# In this file:   ==============================================================
# Based on file 22. 
# Try initial decomposition of eSRM. 
# 
# Main findings:  ==============================================================
# 1. 

load("main.RImage")
load(imgName(".FullRR.ReadyforAPIM"))

ipak (c("tidyverse","nlme"))
dontSkip <- FALSE

# Start with raw ratings and look at variances =================================

rawVars <- c(paste0("Hum", 1:3), paste0("Cre", 1:3), paste0("Safe", 1:3),
             paste0("Contrib", 1:2), "NonTalk", "Shy", paste0("Self_Hum_", 1:3))

# prepare a perceiver wide format
e <- select(x, Actor, Partner, any_of(idCols), any_of(rawVars)) 
e <- e[e$a.id != e$p.id,]
ea <- pivot_wider(e, id_cols = c("group.id", "a.id"), names_from = "Partner", 
                 values_from = all_of(rawVars ))
nc <- ncol(ea)
ea$sd.a <- apply(ea, 1, function(a) sd(a[3:nc], na.rm = TRUE))
x <- left_join(x, ea[,c("a.id", "sd.a")], by = c("a.id"))
ep <- pivot_wider(e, id_cols = c("group.id", "p.id"), names_from = "Actor", 
                  values_from = all_of(rawVars ))
nc <- ncol(ep)
ep$sd.p<- apply(ep, 1, function(a) sd(a[3:nc], na.rm = TRUE))
x <- left_join(x, ep[,c("p.id", "sd.p")], by = c("p.id"))

x <- select(x, any_of(idCols), sd.a,sd.p, everything())



# Dyadic level analyses  =======================================================
# test the main hypothesis for creativity (Same as the JAP code)
if (dontSkip) {
  fitCH <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  aDum + pDum", mlMethod = "REML")
  fitCSH <- showAPIM("Creat_rel.BA ~ Safety_rel.BA + Safety_rel.AB + 
                  Hum_rel.AB + Hum_rel.BA + aDum + pDum", mlMethod = "REML")
  fitCS <- showAPIM("Creat_rel.BA ~ Safety_rel.BA + Safety_rel.AB +
                  aDum + pDum", mlMethod = "REML")
  fitSH <- showAPIM("Safety_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  aDum + pDum", mlMethod = "REML")
  
  # test for the opposite causality
  fit <- APIMformulaShow("Hum", c("Safety"))
  
  fit <- APIMformulaShow("Hum", "Creat")
  fit <- APIMformulaShow("Hum", c("Safety", "Creat"))
  fit <- APIMformulaShow("Safety", "Creat")
  fit <- APIMformulaShow("Safety", c("Hum", "Creat"))
  
  # The models that worked best (by all trial-err process)    
  fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA +
                  aDum + pDum", mlMethod = "REML")
  fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
}

# Individual & Dyadic level analyses   =========================================
# Playing with individual and dyad level models:
# note that all the columns are scaled, but still each column represents the
# right ratings. This is why the raw columns hold such small numbers...

if(dontSkip){
  # Dyadic level scores with interaction:
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  aDum + pDum", mlMethod = "REML")
  fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA +
                  aDum + pDum", mlMethod = "REML")
  
  # Dyadic level scores with interactions between individual level scores:
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB  + Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
  # result: the perceivers' interaction don't contribute to the model.
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
  # result: the relationship scores' interaction isn't significant
  fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + 
                  Creat_target.A:Creat_target.B + aDum + pDum")
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
}
# Single Variable 2-level analysis =============================================
# Due to something Tom said about Paul Eastweek's research, I want to verify
# that there is no correlation between the two levels of the same variable
if (dontSkip) {
  h <- showAPIM("Hum_rel.AB ~ Hum_target.A : Hum_target.B +
          Hum_perc.A : Hum_perc.B")
  # HumAB = group-mean + percA + targetB + gammaAB + resid
  
  singleVarDyadic <- data.frame(matrix(ncol = 5, nrow = 0))
  for (i in varnames) {
    itargets <- paste0(paste0(i, "_target"), c(".A", ".B"))
    ipercs <- paste0(paste0(i, "_perc"), c(".A", ".B"))
    f <- paste0(i, "_rel.BA ~ ", 
                paste0(itargets, ":",ipercs[2:1], collapse = " + "),
                " + aDum + pDum")
    t <- try(fit <- gls(as.formula(f),
                        na.action = na.omit, method = "REML", verbose = TRUE,
                        correlation = corCompSymm (form = ~1|dyad),
                        data = cbind(pw)))
    if (!"try-error" %in% class(t)) {
      nr <- nrow(data.frame(summary(fit)$tTable))
      res <- round(data.frame(summary(fit)$tTable[(nr-1):nr,]), 2)
      res$DV <- c(paste0(i, "_rel.BA"), "")
      singleVarDyadic <- rbind(singleVarDyadic, res)
    }}
  singleVarDyadic$IV <- rownames(singleVarDyadic)
  rownames(singleVarDyadic) <- NULL
  singleVarDyadic <- select(singleVarDyadic, DV, IV, everything())
  singleVarDyadic
}

# mlMethod = "ML" vs. "REML" ===================================================
# check same model with ML instead of REML, better significance
# ML is required to compare models with different set of fixed effects items, 
# as we wish to compare next. 
# Note you can't compare the fits.
# Comparison is for models and not for estimation methods. 
fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "REML")
fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")

# Models comparison with and w/o Individual level analyses  ====================
# compare the models w/o individual level elements and with targets
fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  aDum + pDum", mlMethod = "ML")
fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum", mlMethod = "ML")
anova(fit0, fit1) 
# results: fit1 offers higher loglik (lower abs value), significantly better :)

# The models that worked best (followed by all trial-err process)    
fit0 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA +
                  aDum + pDum", mlMethod = "ML")
fit1 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_target.A:Hum_target.B + aDum + pDum")
    
# Humility power couples index  ================================================
# Add humility power couple index
# The power couple index is the interaction with a change of sign if 
# both dyad members have negative target scores/effects.
# Avi told me one can't use such an index because of the non linear change of sign.
# Therefore, he advised me to use the interaction only. 
pw$HumTargetsInt <- pw$Hum_target.A * pw$Hum_target.B
pw$CreTargetsInt <- pw$Creat_target.A * pw$Creat_target.B
fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB:HumTargetsInt + 
                  Hum_rel.BA:HumTargetsInt + 
                  aDum + pDum")               # nothing is significant
fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + 
                  Hum_rel.BA*HumTargetsInt + 
                  aDum + pDum", mlMethod = "ML") 
fit2 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + 
                  Hum_rel.BA + HumTargetsInt + 
                  aDum + pDum", mlMethod = "ML")
fit3 <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + 
                  Hum_rel.BA + HumTargetsInt + CreTargetsInt +
                  aDum + pDum", mlMethod = "ML")

anova(fit0, fit2)
anova(fit2, fit3)
anova(fit0, fit3)


# investigate "true" power couples (with the switch of sign)
if(dontSkip){
  pw$HumPowerCouple <- pw$Hum_target.A * pw$Hum_target.B
  pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] <- 
    pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B < 0] * (-1)
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
  # Compare the distribution of the power-couple index vs. the interaction index:
  stem(pw$HumPowerCouple)
  stem(pw$HumTargetsInt)
  # kurtosis = the measure of heavy tail. Best kurtosis = 3. 
  kurtosi(pw$HumPowerCouple)
  kurtosi(pw$HumTargetsInt)
  # skewness = the measure of a-symmetry. Best skewness = 0.
  skew(pw$HumPowerCouple)
  skew(pw$HumTargetsInt)
}

# Raw vs. decomposed dyadic scores analyses  ===================================
# Compare the predictions of dyadic scores to those of raw dyadic ratings:
fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
                  Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
                  aDum + pDum")
# results: the perceivers are far from significance (p = 0.98)
fit <- showAPIM("Creat_raw.BA ~ Hum_rel.AB + Hum_rel.BA + 
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

# Scaled vs. non-scaled scores  ================================================
# The difference when using the non-scaled scores for raw dyadic creativity.
# fit <- showAPIM("Creat.raw ~ Hum_rel.AB * Hum_rel.BA + 
#               Hum_perc.A:Hum_perc.B + Hum_target.A:Hum_target.B +
#               aDum + pDum")
    

# Individual-level equivalent analysis  ========================================
# The goal of this section is to show the advantage of SRM analysis. 
# If we're able to show effects that are available only when using SRM, 
# it will be very nice...
# Avi convinced me to drop that line of research. 

d <- select(x, a.id, p.id, ends_with(".raw"))
d <- d[d$a.id != d$p.id,]
cor(d$Hum.raw, d$Creat.raw)  #0.46
lm(Creat.raw ~ Hum.raw + Safety.raw, d)

# Perceiver effects - acquiescence vs construct-perception =====================
# I wish to test what part of the perceptions is a general acquiescence bias vs. 
# construct specific perception tendency.

ipak(c("psych"))

# will it be better including the gm? 
perc <- select(apEffects, ends_with("_perc"))
psych::alpha(perc, check.keys = TRUE) 
# results: 0.52 = has some consistency, but far from being a pure 
#          acquiescence representation. 
#          How do I get to a number?
#          Consult Limor. 

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
  # in the following analysis, we can see that the raw rating of B on A
  # are completely and solely dependent on the equivalent dyadic score. 
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
  
  # view the prediction of the dyadic scores ===================================
  # in the following analysis, we can see that the dyadic ratings of B on A
  # are completely and solely dependent on the equivalent dyadic score. 
  DyadicFromInd <- data.frame(matrix(ncol = 5, nrow = 0))
  for (i in varnames) {
    f <- paste0(i, "_rel.BA ~ ", i, "_rel.AB + ", i, "_rel.BA + ", 
                i, "_target.B:", i, "_target.A + ",
                i, "_perc.B:", i, "_perc.A + ",
                # i, "_target.B:", i, "_perc.A + ",
                # i, "_perc.B:", i, "_target.A + ",
                "aDum + pDum")
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
  