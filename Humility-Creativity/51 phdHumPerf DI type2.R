# Dyadic indices type 4
# These indices are confound from the dyadic scores of a dyad
# ============================================================

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# Utility Variables ===========================================================
  hideCode <- FALSE
  saveEnv <- FALSE

# Prepare the data and load packages ==========================================
  load("main.RImage")
  load(imgName(".FullRR.ReadyforAPIM"))
  
  ipak (c("data.table", "apaTables", "psych", "tidyverse", "sjPlot", "nlme"))

  # Dyadic indices type 4 (from dyadic scores) based on a single variable =====
  
  # Some dyadic indices are at the dyad level, and hence won't require the pw 
  # format. d is the dataset that will hold the dyads only. 

  pw <- pw[!duplicated(pw$dyad),]
  pw <- select(pw, -any_of(dummyCols))
  
  
  i <- varnames[1];j <- varnames[2]
  # Creation of all dyadic indices
  for (i in varnames) {
    ic <- paste0(paste0(i, "_rel"), c(".AB", ".BA"))
    pw[,paste0("ind_max_", i, 4)] <- apply(pw[,ic], 1, function(a) max(a, na.rm = T ))
    pw[,paste0("ind_ave_", i, 4)] <- apply(pw[,ic], 1, function(a) mean(a, na.rm = T ))
    pw[,paste0("ind_min_", i, 4)] <- apply(pw[,ic], 1, function(a) min(a, na.rm = T ))
    pw[,paste0("ind_dif_", i, 4)] <- abs(pw[,ic[1]] - pw[,ic[2]])
    # directed indices
    pw[,paste0("ind_AminB_", i, "4d")] <- pw[,ic[1]] - pw[,ic[2]]
  }

  #Since some dyadic indices can be directional (such as the directioanl 
  # difference between the relationship scores), we'll build the dyadic indices
  # for pw as well.
  i <- varnames[1];j <- varnames[2]
  # Creation of all dyadic indices
  for (i in varnames) {
    ic <- paste0(paste0(i, "_rel"), c(".AB", ".BA"))
    pw[,paste0("ind_max_", i, 4)] <- apply(pw[,ic], 1, function(a) max(a, na.rm = T ))
    pw[,paste0("ind_ave_", i, 4)] <- apply(pw[,ic], 1, function(a) mean(a, na.rm = T ))
    pw[,paste0("ind_min_", i, 4)] <- apply(pw[,ic], 1, function(a) min(a, na.rm = T ))
    pw[,paste0("ind_dif_", i, 4)] <- abs(pw[,ic[1]] - pw[,ic[2]])
    # directed indices
    pw[,paste0("ind_AminB_", i, "4d")] <- pw[,ic[1]] - pw[,ic[2]]
  }
  
  # View the creativity relationship score prediction by humility individual and 
  # dyadic level
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.B:Hum_target.A +
                   aDum + pDum")
  # it can be seen that the interaction between the target scores of humility is
  # very high. that means that similarity in the humility scores between the 
  # dyad members increases the unique creativity spotted in the dyad.
  # So how about the difference if they are both high vs. both low?
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Creat_target.A:ind_ave_Hum4 + aDum + pDum")
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  Hum_target.B:ind_ave_Hum4 + aDum + pDum")
  
  # how do i simulate a low vs. high interaction? 
  pw$interactionHum <- pw$Hum_target.A*pw$Hum_target.B  
  # test that it represents the interaction: ok
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  interactionHum + aDum + pDum")
  pw$HumPowerCouple <- pw$Hum_target.A*pw$Hum_target.B
  pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B <0] <- 
    (-1)* pw$HumPowerCouple[pw$Hum_target.A < 0 & pw$Hum_target.B <0]
  fit <- showAPIM("Creat_rel.BA ~ Hum_rel.AB * Hum_rel.BA + 
                  HumPowerCouple + aDum + pDum")
  fit <- showAPIM("Creat_rel.BA ~ Safety_rel.AB * Safety_rel.BA + 
                  HumPowerCouple + aDum + pDum")
  
  
  #general ICC check...
  ICClme <- function(out) {
    varests <- as.numeric(VarCorr(out)[1:2])
    return(round(varests[1]/sum(varests), 2))
  }
  iccTable <- data.frame(matrix(ncol = 2, nrow = 0))
  names(iccTable) <- c("Var", "ICC")
  for (i in names(pw)) {
    if(!(i %in% idCols)) {
      f <- as.formula(paste0(i, " ~ 1"))
      emptyM  <- lme(f, random = ~ 1 | group.id, data = pw)
      iccTable[nrow(iccTable)+1,] <- c(i,ICClme(emptyM))
    }
  }
  
  rgc <- 1; hlmc <- 1; hlms <- regs <- list()
  inds <- grep("ind_",names(pw), value = TRUE)
  # iind <- i; jind <- inds[4]
  # correlations between each two indices
  for (i in 1:(length(varnames)-1)) {
    ic <- grep(varnames[i], inds, value = TRUE)
    for (j in (i+1):length(varnames)) {
      jc <- grep(varnames[j], inds, value = TRUE)
      for (iind in ic) {
        for(jind in jc){
          print(c(iind, jind))
          #check if it's an HLM procedure or a linear regression one:
          reg <- iccTable[iccTable$Var == iind, "ICC"] == 0
          reg <- reg & (iccTable[iccTable$Var == jind, "ICC"] == 0)
          f <- paste0(iind, " ~ ", jind)
          if (reg) {
            regs[[rgc]] <- c(f, lm(as.formula(f), data = pw))
            rgc <- rgc+1
          }
          if (!reg){      # we have to do an HLM analysis for these variables
            hlms[[hlmc]] <- c(f, lme(as.formula(f), 
                                     random = ~ 1 | group.id, data = pw))
            hlmc <- hlmc+1
          }
        }
        
      }
        
    }
    
  }
  
  
  fit <- lme(Hum_rel.AB  ~ ind_max_Safety4,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|group.id),
             data = pw)
  
  summary(fit)
  VarCorr(fit)
  fit <- gls(ind_ave_Hum4  ~ ind_max_Safety4,
             na.action = na.omit, method = "REML", verbose = TRUE,
             correlation = corCompSymm (form = ~1|group.id),
             data = pw)
  summary(fit)
  
  round(data.frame(summary(fit)$tTable[2:3,]), 2)
    if (hideCode) {
      fit <- gls(Creat_rel.BA ~ Hum_rel.AB + Hum_rel.BA + aDum + pDum,
                 na.action = na.omit, method = "REML", verbose = TRUE,
                 correlation = corCompSymm (form = ~1|dyad),
                 data = cbind(pw))
      round(data.frame(summary(fit)$tTable[2:3,]), 2)
      
      cor(pw$Creat_rel.AB, pw$HumPowerCouple)
      cor(pw$HumPercivPowerCouple, pw$Creat_rel.AB)
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
      
      
      round(data.frame(summary(fit)$tTable[,]), 2)
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
  save.image("JDC RQ3")

  # view the prediction of the raw scores ======================================
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
  save.image("JDC raw from Dyadic")
  
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
  