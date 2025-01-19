# SRM - RR ANALYSIS - Preperation for APIM
# ========================================
# In this file we generate two pairwise dataframes:
#   pw and apRelEffects.
#   
#   pw:       hold for each dyad, the directional relationship scores of all the SRM 
#             constructs. in a pair-wise format such that each row contains the 
#             data of both i to j, and j to i.
#             
#   apRelEffects: 
#             same as pw, but for the sake of convenience and availability, 
#             it also includes the individual level scores and data.

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console

# SRM - RR ANALYSIS:
# ==============================================================================


load("main.RImage")
load(imgName(".FullRR.ReadyforAnalaysis"))

ipak (c("TripleR", "ggplot2", "data.table", "apaTables", "psych", "tidyverse"))

x <- as.data.frame(x)

# prepare relational effects table (attrRelEff)
# ==============================================

  varlist <- c("H1/H2", "Creat1/Creat2", "Contrib1/Contrib2", 
               "Sf1/Sf2",  "NonTalk","Shy")
  varnames <- c("Hum", "Creat", "Contr", 
                "Safety", "NonTalk", "Shy")
  idCols <- c("group.id","a.id", "p.id", "dyad" )
  for (i in 1:length(varlist)) {
    fit <- RR(as.formula(paste(varlist[i], " ~ a.id*p.id|group.id")), 
              data=x, gm = FALSE, na.rm = TRUE)
    if (grepl("/", varlist[i])) names(fit$effectsRel)[2:3] <- c( "a.id","p.id")
    ifelse (i == 1, attrRelEff <- fit$effectsRel,
            attrRelEff <- left_join(attrRelEff, fit$effectsRel, by = idCols))
    names(attrRelEff)[length(names(attrRelEff))] <- paste0(varnames[i], "_rel")
  }

# prepare the raw ratings table (raws)
# ==============================================

  idColsRaw <- c("group.id","a.id", "p.id")
  raws <- x[,c(idColsRaw, paste0(varnames, ".raw"))]
  raws[,idColsRaw] <- apply(raws[,idColsRaw], 2, as.factor)

# Prepare the pairwise table from both raw and relationship effects
# ==================================================================

  pw1 <- attrRelEff[c(TRUE, FALSE), ]
  names(pw1) <- gsub("_rel", "_rel.AB", names(pw1))
  pw1 <- left_join(pw1, raws, by = idColsRaw)
  names(pw1) <- gsub(".raw", "_raw.AB", names(pw1))
  
  pw2 <- attrRelEff[c(FALSE, TRUE), ]
  pw2 <- left_join(pw2, raws, by = idColsRaw)
  names(pw2) <- names(pw1)

  pw12 <- cbind(pw1, pw2)
  pw21 <- cbind(pw2, pw1)
  pw <- rbind(pw12, pw21)
  names(pw) <- c(names(pw1), 
                 gsub("_rel", "_rel.BA", names(attrRelEff)),
                 gsub(".raw", "_raw.BA", names(raws[grep(".raw", names(raws))])))
  pw <- pw[ , unique(names(pw))]
  pw <- pw[order(pw$dyad), ]
  pw$partnum <- c(1,2)
  
# Add individual-level scores to the pairwise table
# =================================================
  # In this section we add the individual scores (perceiver&target=actor&partner):
  # 1. Relative (natural) scores (e.g: Hum_perc.A)
  # 2. GM - including the mean of each group (e.g: Hum_percGM.A)
  # 3. Positive - We drive the scale so all scores are positive (e.g: Hum_percPOS.A)
  # calculate the individual scores
  apEffects <- getEffects(~ a.id*p.id | group.id, data = x, varlist = varlist)
  # enter the perceiver scores
  names(apEffects) <- c("a.id", "group.id", paste0(rep(varnames, each = 2), c("_perc.A", "_target.A")) )
  # do the same for the effects including the gm
  gmAPEffects <- getEffects(~ a.id*p.id | group.id, data = x, varlist = varlist, gm = TRUE)
  names(gmAPEffects) <- c("a.id", "group.id", paste0(rep(varnames, each = 2), c("_percGM.A", "_targetGM.A")) )
  
  posAPEffects <- as.data.frame(apply(apEffects[,-c(1:2)], 2, function(a) {minEff <- min(a); return(a - minEff)}))
  names(posAPEffects) <- paste0(rep(varnames, each = 2), c("_percPOS.A", "_targetPOS.A"))
  posAPEffects <- cbind(apEffects[,1:2], posAPEffects)
  
  gmAPEffects <- left_join(apEffects, gmAPEffects, by = c("group.id", "a.id"))
  gmAPEffects <- left_join(gmAPEffects, posAPEffects, by = c("group.id", "a.id"))
  pw <- left_join(pw, gmAPEffects, by = c("group.id", "a.id"))
  # enter the target scores
  names(gmAPEffects) <- gsub(".A$", ".B", names(gmAPEffects))
  gmAPEffects <- rename(gmAPEffects, p.id = a.id)
  pw <- left_join(pw, gmAPEffects, by = c("group.id", "p.id"))
  # adjust the apeffects and gmAPEffects names to represent individual scores again for future use
  names(gmAPEffects) <- gsub(".B", "", names(gmAPEffects))
  names(apEffects) <- gsub(".A", "", names(apEffects))
  apEffects <- rename(apEffects, id = a.id)
  gmAPEffects <- rename(gmAPEffects, id = p.id)
  
  rm(list = setdiff(ls(), c("longInput", "idCols", "gmAPEffects",
                            "apEffects", "x", "pw", "varnames")))
  load("main.RImage")
  save.image(imgName(".FullRR.Pairwise"))

